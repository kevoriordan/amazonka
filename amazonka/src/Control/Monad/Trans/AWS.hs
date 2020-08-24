{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
-- Module      : Control.Monad.Trans.AWS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- The 'AWST' transformer provides the environment required to perform AWS
-- operations. The transformer is intended to be used directly
-- or embedded as a layer within a transformer stack.
--
-- "Network.AWS" contains an 'IO' specialised version of 'AWST' with a typeclass
-- to assist in automatically lifting operations.
module Control.Monad.Trans.AWS
    (
    -- * Running AWS Actions
      AWST
    , AWST'
    , runAWST
    , runResourceT
    , AWSConstraint

    -- * Authentication and Environment
    , newBasicEnv
    , Env
    , HasEnv       (..)
    , askEnv

    -- ** Credential Discovery
    , BasicCredentials  (..)
    -- $discovery

    -- ** Supported Regions
    , Region       (..)

    -- * Sending Requests
    -- $sending

    , send

    -- ** Pagination
    -- $pagination

    , paginate

    -- ** Waiters
    -- $waiters

    , await

    -- ** Service Configuration
    -- $service

    -- *** Overriding Defaults
    , configure
    , override

    -- *** Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- ** Streaming
    -- $streaming

    -- *** Request Bodies
    , ToHashedBody (..)
    , hashedFile
    , hashedBody

    -- *** Chunked Request Bodies
    , ToBody       (..)
    , ChunkSize    (..)
    , defaultChunkSize
    , chunkedFile
    , unsafeChunkedBody

    -- *** Response Bodies
    , sinkBody

    -- *** File Size and MD5/SHA256
    , getFileSize
    , sinkMD5
    , sinkSHA256

    -- * Presigning Requests
    -- $presigning

    , presignURL
    , presign

    -- * EC2 Instance Metadata
    -- $metadata

    , isEC2
    , dynamic
    , metadata
    , userdata

    , EC2.Dynamic  (..)
    , EC2.Metadata (..)

    -- * Running Asynchronous Actions
    -- $async

    -- * Handling Errors
    -- $errors

    , AsError      (..)
    , AsAuthError  (..)

    , trying
    , catching

    -- ** Building Error Prisms
    , Error._MatchServiceError
    , Error.hasService
    , Error.hasStatus
    , Error.hasCode

    -- * Logging
    -- $logging

    , Logger
    , LogLevel     (..)

    -- ** Constructing a Logger
    , newLogger

    -- ** Endpoints
    , Endpoint
    , setEndpoint

    -- * Re-exported Types
    , module Network.AWS.Types
    , module Network.AWS.Waiter
    , module Network.AWS.Pager
    , RqBody
    , HashedBody
    , ChunkedBody
    , RsBody
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class    (MonadError (..))
import Control.Monad.IO.Unlift
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Writer.Class
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

import Data.Conduit      hiding (await)
import Data.Conduit.Lazy (MonadActive (..))
import Data.IORef
import Data.Monoid


import Network.AWS.AuthTypes
import Network.AWS.BasicEnv
import Network.AWS.EnvTypes
import Network.AWS.Internal.Body
import Network.AWS.Internal.HTTP
import Network.AWS.Internal.Logger
import Network.AWS.Lens            (catching, throwingM, trying, view, (^.))
import Network.AWS.Pager           (AWSPager (..))
import Network.AWS.Prelude         as AWS
import Network.AWS.Request         (requestURL)
import Network.AWS.Types           hiding (LogLevel (..))
import Network.AWS.Waiter          (Accept, Wait)

import qualified Network.AWS.EC2.Metadata as EC2
import qualified Network.AWS.Error        as Error
import qualified Network.AWS.Presign      as Sign

type AWST = AWST' Env

newtype AWST' r m a = AWST' { unAWST :: ReaderT r m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadIO
        , MonadActive
        , MonadTrans
#if MIN_VERSION_base(4,9,0)
        , MonadFail
#endif
        )

instance MonadThrow m => MonadThrow (AWST' r m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (AWST' r m) where
    catch (AWST' m) f = AWST' (catch m (unAWST . f))

instance MonadMask m => MonadMask (AWST' r m) where
    mask a = AWST' $ mask $ \u ->
        unAWST $ a (AWST' . u . unAWST)

    uninterruptibleMask a = AWST' $ uninterruptibleMask $ \u ->
        unAWST $ a (AWST' . u . unAWST)

#if MIN_VERSION_exceptions(0,10,0)
    generalBracket acquire rel action = AWST' $
        generalBracket
            (unAWST acquire)
            (\a ex -> unAWST $ rel a ex)
            (\a -> unAWST $ action a)
#endif


instance MonadBase b m => MonadBase b (AWST' r m) where
    liftBase = liftBaseDefault

instance MonadTransControl (AWST' r) where
    type StT (AWST' r) a = StT (ReaderT r) a

    liftWith = defaultLiftWith AWST' unAWST
    restoreT = defaultRestoreT AWST'

instance MonadBaseControl b m => MonadBaseControl b (AWST' r m) where
    type StM (AWST' r m) a = ComposeSt (AWST' r) m a

    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadUnliftIO m => MonadUnliftIO (AWST' r m) where
#if MIN_VERSION_unliftio_core(0,2,0)
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
        AWST' $
        withRunInIO $ \run ->
        inner (run . unAWST)
#else
    {-# INLINE askUnliftIO #-}
    askUnliftIO = AWST' $ (\(UnliftIO f) -> UnliftIO $ f . unAWST)
        <$> askUnliftIO
#endif

instance MonadResource m => MonadResource (AWST' r m) where
    liftResourceT = lift . liftResourceT

instance MonadError e m => MonadError e (AWST' r m) where
    throwError     = lift . throwError
    catchError m f = AWST' (unAWST m `catchError` (unAWST . f))

instance Monad m => MonadReader r (AWST' r m) where
    ask     = AWST' ask
    local f = AWST' . local f . unAWST
    reader  = AWST' . reader

instance MonadWriter w m => MonadWriter w (AWST' r m) where
    writer = lift . writer
    tell   = lift . tell
    listen = AWST' . listen . unAWST
    pass   = AWST' . pass   . unAWST

instance MonadState s m => MonadState s (AWST' r m) where
    get = lift get
    put = lift . put

instance MFunctor (AWST' r) where
    hoist nat = AWST' . hoist nat . unAWST

instance PrimMonad m => PrimMonad (AWST' r m) where
    type PrimState (AWST' r m) = PrimState m
    primitive = AWST' . primitive

-- | Run an 'AWST' action with the specified environment.
runAWST :: HasEnv r => r -> AWST' r m a -> m a
runAWST r (AWST' m) = runReaderT m r

askEnv :: (Monad m, HasEnv r) => AWST' r m Env
askEnv = AWST' (asks (^. environment))

-- | An alias for the constraints required to send requests,
-- which 'AWST' implicitly fulfils.
type AWSConstraint r m =
    ( MonadThrow     m
    , MonadCatch     m
    , MonadResource  m
    , MonadReader  r m
    , HasEnv       r
    )

-- | Send a request, returning the associated response if successful.
--
-- Throws 'Error'.
send :: (AWSConstraint r m, AWSRequest a)
     => a
     -> m (Rs a)
send = retrier >=> fmap snd . hoistError

-- | Repeatedly send a request, automatically setting markers and
-- paginating over multiple responses while available.
--
-- Throws 'Error'.
paginate :: (AWSConstraint r m, AWSPager a)
         => a
         -> ConduitM () (Rs a) m ()
paginate = go
  where
    go !x = do
        !y <- lift $ send x
        yield y
        maybe (pure ()) go (page x y)

-- | Poll the API with the supplied request until a specific 'Wait' condition
-- is fulfilled.
--
-- Throws 'Error'.
await :: (AWSConstraint r m, AWSRequest a)
      => Wait a
      -> a
      -> m Accept
await w = waiter w >=> hoistError

-- | Presign an URL that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presignURL :: ( MonadIO m
              , MonadReader r m
              , HasEnv r
              , AWSRequest a
              )
           => UTCTime     -- ^ Signing time.
           -> Seconds     -- ^ Expiry time.
           -> a           -- ^ Request to presign.
           -> m ByteString
presignURL ts ex = fmap requestURL . presign ts ex

-- | Presign an HTTP request that is valid from the specified time until the
-- number of seconds expiry has elapsed.
presign :: ( MonadIO m
           , MonadReader r m
           , HasEnv r
           , AWSRequest a
           )
        => UTCTime     -- ^ Signing time.
        -> Seconds     -- ^ Expiry time.
        -> a           -- ^ Request to presign.
        -> m ClientRequest
presign ts ex x = do
    Env{..} <- view environment
    Sign.presignWith (appEndo (getDual _envOverride)) _envAuth _envRegion ts ex x

-- | Test whether the underlying host is running on EC2.
-- This is memoised and any external check occurs for the first invocation only.
isEC2 :: (MonadIO m, MonadReader r m, HasEnv r) => m Bool
isEC2 = do
    ref <- view envEC2
    mp  <- liftIO (readIORef ref)
    case mp of
        Just p  -> return p
        Nothing -> do
            m  <- view envManager
            !p <- EC2.isEC2 m
            liftIO (atomicWriteIORef ref (Just p))
            return p

-- | Retrieve the specified 'Dynamic' data.
--
-- Throws 'HttpException'.
dynamic :: (MonadIO m, MonadThrow m, MonadReader r m, HasEnv r)
        => EC2.Dynamic
        -> m ByteString
dynamic d = view envManager >>= flip EC2.dynamic d

-- | Retrieve the specified 'Metadata'.
--
-- Throws 'HttpException'.
metadata :: (MonadIO m, MonadThrow m, MonadReader r m, HasEnv r)
         => EC2.Metadata
         -> m ByteString
metadata m = view envManager >>= flip EC2.metadata m

-- | Retrieve the user data. Returns 'Nothing' if no user data is assigned
-- to the instance.
--
-- Throws 'HttpException'.
userdata :: (MonadIO m, MonadCatch m, MonadReader r m, HasEnv r)
         => m (Maybe ByteString)
userdata = view envManager >>= EC2.userdata

hoistError :: MonadThrow m => Either Error a -> m a
hoistError = either (throwingM _Error) return

