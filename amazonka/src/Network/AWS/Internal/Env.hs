{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Internal.Env
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Environment and AWS specific configuration for the
-- 'Network.AWS.AWS' and 'Control.Monad.Trans.AWS.AWST' monads.
module Network.AWS.Internal.Env
    (
    -- * Creating the Environment
      Env      (..)
    , HasEnv   (..)

    -- * Overriding Default Configuration
    , override
    , configure

    -- * Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- * Retry HTTP Exceptions
    , retryConnectionFailure
    ) where

import Control.Monad.Reader

import Data.Function (on)
import Data.IORef
import Data.Monoid

import Network.AWS.Internal.Auth
import Network.AWS.Internal.Logger
import Network.AWS.Lens            (Getter, Lens', lens, to, (.~), (<>~), (?~))
import Network.AWS.Types
import Network.HTTP.Conduit        (HttpException (HttpExceptionRequest, InvalidUrlException),
                                    HttpExceptionContent (ConnectionClosed, ConnectionFailure, ConnectionTimeout, InternalException, NoResponseDataReceived),
                                    Manager)

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion     :: !Region
    , _envLogger     :: !Logger
    , _envRetryCheck :: !(Int -> HttpException -> Bool)
    , _envOverride   :: !(Dual (Endo Service))
    , _envManager    :: !Manager
    , _envEC2        :: !(IORef (Maybe Bool))
    , _envAuth       :: !Auth
    }

-- Note: The strictness annotations aobe are applied to ensure
-- total field initialisation.

class HasEnv a where
    environment   :: Lens' a Env
    {-# MINIMAL environment #-}

    -- | The current region.
    envRegion     :: Lens' a Region

    -- | The function used to output log messages.
    envLogger     :: Lens' a Logger

    -- | The function used to determine if an 'HttpException' should be retried.
    envRetryCheck :: Lens' a (Int -> HttpException -> Bool)

    -- | The currently applied overrides to all 'Service' configuration.
    envOverride   :: Lens' a (Dual (Endo Service))

    -- | The 'Manager' used to create and manage open HTTP connections.
    envManager    :: Lens' a Manager

    -- | The credentials used to sign requests for authentication with AWS.
    envAuth       :: Lens' a Auth

    -- | A memoised predicate for whether the underlying host is an EC2 instance.
    envEC2        :: Getter a (IORef (Maybe Bool))

    envRegion     = environment . lens _envRegion     (\s a -> s { _envRegion     = a })
    envLogger     = environment . lens _envLogger     (\s a -> s { _envLogger     = a })
    envRetryCheck = environment . lens _envRetryCheck (\s a -> s { _envRetryCheck = a })
    envOverride   = environment . lens _envOverride   (\s a -> s { _envOverride   = a })
    envManager    = environment . lens _envManager    (\s a -> s { _envManager    = a })
    envAuth       = environment . lens _envAuth       (\s a -> s { _envAuth       = a })
    envEC2        = environment . to _envEC2

instance HasEnv Env where
    environment = id

instance ToLog Env where
    build Env{..} = b <> "\n" <> build _envAuth
      where
        b = buildLines
            [ "[Amazonka Env] {"
            , "  region = " <> build _envRegion
            , "}"
            ]

-- | Provide a function which will be added to the existing stack
-- of overrides applied to all service configuration.
--
-- To override a specific service, it's suggested you use
-- either 'configure' or 'reconfigure' with a modified version of the default
-- service, such as @Network.AWS.DynamoDB.dynamoDB@.
override :: HasEnv a => (Service -> Service) -> a -> a
override f = envOverride <>~ Dual (Endo f)

-- | Configure a specific service. All requests belonging to the
-- supplied service will use this configuration instead of the default.
--
-- It's suggested you use a modified version of the default service, such
-- as @Network.AWS.DynamoDB.dynamoDB@.
--
-- /See:/ 'reconfigure'.
configure :: HasEnv a => Service -> a -> a
configure s = override f
  where
    f x | on (==) _svcAbbrev s x = s
        | otherwise              = x

-- | Scope an action such that all requests belonging to the supplied service
-- will use this configuration instead of the default.
--
-- It's suggested you use a modified version of the default service, such
-- as @Network.AWS.DynamoDB.dynamoDB@.
--
-- /See:/ 'configure'.
reconfigure :: (MonadReader r m, HasEnv r) => Service -> m a -> m a
reconfigure = local . configure

-- | Scope an action within the specific 'Region'.
within :: (MonadReader r m, HasEnv r) => Region -> m a -> m a
within r = local (envRegion .~ r)

-- | Scope an action such that any retry logic for the 'Service' is
-- ignored and any requests will at most be sent once.
once :: (MonadReader r m, HasEnv r) => m a -> m a
once = local (override (serviceRetry . retryAttempts .~ 0))

-- | Scope an action such that any HTTP response will use this timeout value.
--
-- Default timeouts are chosen by considering:
--
-- * This 'timeout', if set.
--
-- * The related 'Service' timeout for the sent request if set. (Usually 70s)
--
-- * The 'envManager' timeout if set.
--
-- * The default 'ClientRequest' timeout. (Approximately 30s)
timeout :: (MonadReader r m, HasEnv r) => Seconds -> m a -> m a
timeout s = local (override (serviceTimeout ?~ s))



-- | Retry the subset of transport specific errors encompassing connection
-- failure up to the specific number of times.
retryConnectionFailure :: Int -> Int -> HttpException -> Bool
#if MIN_VERSION_http_client(0,5,0)
retryConnectionFailure _     _ InvalidUrlException {}      = False
retryConnectionFailure limit n (HttpExceptionRequest _ ex)
    | n >= limit = False
    | otherwise  =
        case ex of
            NoResponseDataReceived -> True
            ConnectionTimeout      -> True
            ConnectionClosed       -> True
            ConnectionFailure {}   -> True
            InternalException {}   -> True
            _                      -> False
#else
retryConnectionFailure limit n _ = \case
    _ | n >= limit                -> False
    NoResponseDataReceived        -> True
    FailedConnectionException  {} -> True
    FailedConnectionException2 {} -> True
    TlsException               {} -> True
    _                             -> False
#endif
