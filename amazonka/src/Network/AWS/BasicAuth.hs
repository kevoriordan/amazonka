{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
--
-- The format of environment variables and the credentials file follows the official
-- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs AWS SDK guidelines>.
module Network.AWS.BasicAuth
    (
     getBasicAuth
    -- ** Defaults
    -- *** Environment
    , envAccessKey
    , envSecretKey
    , envSessionToken

    -- *** Credentials File
    , credAccessKey
    , credSecretKey
    , credSessionToken
    , credProfile
    , credFile

    -- ** Credentials
    -- $credentials

    , fromKeys
    , fromSession
    , fromTemporarySession
    , fromEnv
    , fromEnvKeys
    , fromFile
    , fromFilePath
    , fromProfile
    , fromProfileName
    , fromContainer

    , lookupEnvOpt
    , lookupEnvReq
    , fetchAuthInBackground
    , envVarRegion

    -- ** Keys
    , AccessKey    (..)
    , SecretKey    (..)
    , SessionToken (..)

    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Char  (isSpace)
import Data.IORef
import Data.Time  (diffUTCTime, getCurrentTime)

import Network.AWS.AuthTypes
import Network.AWS.Data.JSON
import Network.AWS.EC2.Metadata
import Network.AWS.Lens         (catching, catching_, throwingM, (<&>),
                                 _IOException)
import Network.AWS.Prelude
import Network.HTTP.Conduit

import System.Directory   (doesFileExist, getHomeDirectory)
import System.Environment
import System.Mem.Weak

import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Ini                   as INI
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Network.HTTP.Conduit       as HTTP

-- | Default access key environment variable.
envAccessKey :: Text -- ^ AWS_ACCESS_KEY_ID
envAccessKey = "AWS_ACCESS_KEY_ID"

-- | Default secret key environment variable.
envSecretKey :: Text -- ^ AWS_SECRET_ACCESS_KEY
envSecretKey = "AWS_SECRET_ACCESS_KEY"

-- | Default session token environment variable.
envSessionToken :: Text -- ^ AWS_SESSION_TOKEN
envSessionToken = "AWS_SESSION_TOKEN"

-- | Default credentials profile environment variable.
envProfile :: Text -- ^ AWS_PROFILE
envProfile = "AWS_PROFILE"

-- | Default region environment variable
envVarRegion :: Text -- ^ AWS_REGION
envVarRegion = "AWS_REGION"

-- | Path to obtain container credentials environment variable (see
-- 'FromContainer').
envContainerCredentialsURI :: Text -- ^ AWS_CONTAINER_CREDENTIALS_RELATIVE_URI
envContainerCredentialsURI = "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"

-- | Credentials INI file access key variable.
credAccessKey :: Text -- ^ aws_access_key_id
credAccessKey = "aws_access_key_id"

-- | Credentials INI file secret key variable.
credSecretKey :: Text -- ^ aws_secret_access_key
credSecretKey = "aws_secret_access_key"

-- | Credentials INI file session token variable.
credSessionToken :: Text -- ^ aws_session_token
credSessionToken = "aws_session_token"

-- | Credentials INI default profile section variable.
credProfile :: Text -- ^ default
credProfile = "default"

-- | Default path for the credentials file. This looks in in the @HOME@ directory
-- as determined by the <http://hackage.haskell.org/package/directory directory>
-- library.
--
-- * UNIX/OSX: @$HOME/.aws/credentials@
--
-- * Windows: @C:\/Users\//\<user\>\.aws\credentials@
--
-- /Note:/ This does not match the default AWS SDK location of
-- @%USERPROFILE%\.aws\credentials@ on Windows. (Sorry.)
credFile :: (MonadCatch m, MonadIO m) => m FilePath
credFile = catching_ _IOException dir err
  where
    dir = (++ p) <$> liftIO getHomeDirectory
    err = throwM $ MissingFileError ("$HOME" ++ p)

    -- TODO: probably should be using System.FilePath above.
    p = "/.aws/credentials"


{- $discovery
AuthN/AuthZ information is handled similarly to other AWS SDKs. You can read
some of the options available <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs here>.

When running on an EC2 instance and using 'FromProfile' or 'Discover', a thread
is forked which transparently handles the expiry and subsequent refresh of IAM
profile information. See 'Network.AWS.Auth.fromProfileName' for more information.
-}

{- $sending
To send a request you need to create a value of the desired operation type using
the relevant constructor, as well as any further modifications of default/optional
parameters using the appropriate lenses. This value can then be sent using 'send'
or 'paginate' and the library will take care of serialisation/authentication and
so forth.

The default 'Service' configuration for a request contains retry configuration that is used to
determine if a request can safely be retried and what kind of back off/on strategy
should be used. (Usually exponential.)
Typically services define retry strategies that handle throttling, general server
errors and transport errors. Streaming requests are never retried.
-}

{- $pagination
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
Error conditions that are not handled by the 'Wait' configuration will be thrown,
or the first successful response that fulfills the success condition will be
returned.

'Wait' specifications can be found under the @Network.AWS.{ServiceName}.Waiters@
namespace for services which support 'await'.
-}

{- $service
When a request is sent, various values such as the endpoint,
retry strategy, timeout and error handlers are taken from the associated 'Service'
for a request. For example, 'DynamoDB' will use the 'Network.AWS.DynamoDB.dynamoDB'
configuration when sending 'PutItem', 'Query' and all other operations.

You can modify a specific 'Service''s default configuration by using
'configure' or 'reconfigure'. To modify all configurations simultaneously, see 'override'.

An example of how you might alter default configuration using these mechanisms
is demonstrated below. Firstly, the default 'dynamoDB' service is configured to
use non-SSL localhost as the endpoint:

> let dynamo :: Service
>     dynamo = setEndpoint False "localhost" 8000 dynamoDB

The updated configuration is then passed to the 'Env' during setup:

> e <- newEnv Discover <&> configure dynamo
> runResourceT . runAWS e $ do
>     -- This S3 operation will communicate with remote AWS APIs.
>     x <- send listBuckets
>
>     -- DynamoDB operations will communicate with localhost:8000.
>     y <- send listTables
>
>     -- Any operations for services other than DynamoDB, are not affected.
>     ...

You can also scope the 'Endpoint' modifications (or any other 'Service' configuration)
to specific actions:

> e <- newEnv Discover
> runResourceT . runAWS e $ do
>     -- Service operations here will communicate with AWS, even DynamoDB.
>     x <- send listTables
>
>     reconfigure dynamo $ do
>        -- In here, DynamoDB operations will communicate with localhost:8000,
>        -- with operations for services not being affected.
>        ...

Functions such as 'within', 'once', and 'timeout' likewise modify the underlying
configuration for all service requests within their respective scope.
-}

{- $streaming
Streaming comes in two flavours. 'HashedBody' represents a request
that requires a precomputed 'SHA256' hash, or a 'ChunkedBody' type for those services
that can perform incremental signing and do not require the entire payload to
be hashed (such as 'S3'). The type signatures for request smart constructors
advertise which respective body type is required, denoting the underlying signing
capabilities.

'ToHashedBody' and 'ToBody' typeclass instances are available to construct the
streaming bodies, automatically calculating any hash or size as needed for types
such as 'Text', 'ByteString', or Aeson's 'Value' type. To read files and other
'IO' primitives, functions such as 'hashedFile', 'chunkedFile', or 'hashedBody'
should be used.

For responses that contain streaming bodies (such as 'GetObject'), you can use
'sinkBody' to connect the response body to a <http://hackage.haskell.org/package/conduit conduit>
compatible sink.
-}

{- $presigning
Presigning requires the 'Service' signer to be an instance of 'AWSPresigner'.
Not all signing algorithms support this.
-}

{- $metadata
Metadata can be retrieved from the underlying host assuming that you're running
the code on an EC2 instance or have a compatible @instance-data@ endpoint available.
-}

{- $async
Requests can be sent asynchronously, but due to guarantees about resource closure
require the use of <http://hackage.haskell.org/package/lifted-async lifted-async>.

The following example demonstrates retrieving two objects from S3 concurrently:

> import Control.Concurrent.Async.Lifted
> import Control.Lens
> import Control.Monad.Trans.AWS
> import Network.AWS.S3
>
> do x   <- async . send $ getObject "bucket" "prefix/object-foo"
>    y   <- async . send $ getObject "bucket" "prefix/object-bar"
>    foo <- wait x
>    bar <- wait y
>    ...

/See:/ <http://hackage.haskell.org/package/lifted-async Control.Concurrent.Async.Lifted>
-}

{- $errors
Errors are thrown by the library using 'MonadThrow' (unless "Control.Monad.Error.AWS" is used).
Sub-errors of the canonical 'Error' type can be caught using 'trying' or
'catching' and the appropriate 'AsError' 'Prism':

@
trying '_Error'          (send $ ListObjects "bucket-name") :: Either 'Error'          ListObjectsResponse
trying '_TransportError' (send $ ListObjects "bucket-name") :: Either 'HttpException'  ListObjectsResponse
trying '_SerializeError' (send $ ListObjects "bucket-name") :: Either 'SerializeError' ListObjectsResponse
trying '_ServiceError'   (send $ ListObjects "bucket-name") :: Either 'ServiceError'   ListObjectsResponse
@

Many of the individual @amazonka-*@ libraries export compatible 'Getter's for
matching service specific error codes and messages in the style above.
See the @Error Matchers@ heading in each respective library for details.
-}

{- $logging
The exposed logging interface is a primitive 'Logger' function which gets
threaded through service calls and serialisation routines. This allows the
library to output useful information and diagnostics.

The 'newLogger' function can be used to construct a simple logger which writes
output to a 'Handle', but in most production code you should probably consider
using a more robust logging library such as
<http://hackage.haskell.org/package/tinylog tinylog> or
<http://hackage.haskell.org/package/fast-logger fast-logger>.
-}

-- | Retrieve authentication information via the specified 'Credentials' mechanism.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read,
-- and credentials files are invalid or cannot be found.
getBasicAuth :: (Applicative m, MonadIO m, MonadCatch m)
        => Manager
        -> BasicCredentials
        -> m (Auth, Maybe Region)
getBasicAuth m = \case
    FromKeysBasic    a s     -> return (fromKeys a s, Nothing)
    FromSessionBasic a s t   -> return (fromSession a s t, Nothing)
    FromEnvBasic     a s t r -> fromEnvKeys a s t r
    FromProfileBasic n       -> fromProfileName m n
    FromFileBasic    n f     -> fromFilePath n f
    FromContainerBasic       -> fromContainer m
    DiscoverBasic            ->
        -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
        -- let both errors propagate.
        catching_ _MissingEnvError fromEnv $
            -- proceed, missing env keys
            catching _MissingFileError fromFile $ \f ->
                -- proceed, missing credentials file
                catching_ _MissingEnvError (fromContainer m) $ do
                  -- proceed, missing env key
                  p <- isEC2 m
                  unless p $
                      -- not an EC2 instance, rethrow the previous error.
                      throwingM _MissingFileError f
                   -- proceed, check EC2 metadata for IAM information.
                  fromProfile m


-- | Explicit access and secret keys.
fromKeys :: AccessKey -> SecretKey -> Auth
fromKeys a s = Auth (AuthEnv a (Sensitive s) Nothing Nothing)

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, and session token.
--
-- /See:/ 'fromTemporarySession'
fromSession :: AccessKey -> SecretKey -> SessionToken -> Auth
fromSession a s t =
    Auth (AuthEnv a (Sensitive s) (Just (Sensitive t)) Nothing)

-- | Temporary credentials from a STS session consisting of
-- the access key, secret key, session token, and expiration time.
--
-- /See:/ 'fromSession'
fromTemporarySession :: AccessKey
                     -> SecretKey
                     -> SessionToken
                     -> UTCTime
                     -> Auth
fromTemporarySession a s t e =
    Auth (AuthEnv a (Sensitive s) (Just (Sensitive t)) (Just (Time e)))

-- | Retrieve access key, secret key, and a session token from the default
-- environment variables.
--
-- Throws 'MissingEnvError' if either of the default environment variables
-- cannot be read, but not if the session token is absent.
--
-- /See:/ 'envAccessKey', 'envSecretKey', 'envSessionToken'
fromEnv :: (Applicative m, MonadIO m, MonadThrow m) => m (Auth, Maybe Region)
fromEnv =
    fromEnvKeys
        envAccessKey
        envSecretKey
        (Just envSessionToken)
        (Just envVarRegion)

-- | Retrieve access key, secret key and a session token from specific
-- environment variables.
--
-- Throws 'MissingEnvError' if either of the specified key environment variables
-- cannot be read, but not if the session token is absent.
fromEnvKeys :: (Applicative m, MonadIO m, MonadThrow m)
            => Text       -- ^ Access key environment variable.
            -> Text       -- ^ Secret key environment variable.
            -> Maybe Text -- ^ Session token environment variable.
            -> Maybe Text -- ^ Region environment variable.
            -> m (Auth, Maybe Region)
fromEnvKeys access secret session region' =
    (,) <$> fmap Auth lookupKeys <*> lookupRegion
  where
    lookupKeys = AuthEnv
        <$> (lookupEnvReq access  <&> AccessKey . BS8.pack)
        <*> (lookupEnvReq secret  <&> Sensitive . SecretKey . BS8.pack)
        <*> (opt session <&> fmap (Sensitive . SessionToken . BS8.pack))
        <*> return Nothing

    lookupRegion :: (MonadIO m, MonadThrow m) => m (Maybe Region)
    lookupRegion = runMaybeT $ do
        k <- MaybeT (return region')
        r <- MaybeT (opt region')
        case fromText (Text.pack r) of
            Right x -> return x
            Left  e -> throwM . InvalidEnvError $
                "Unable to parse ENV variable: " <> k <> ", " <> Text.pack e

    opt Nothing  = return Nothing
    opt (Just k) = lookupEnvOpt k

-- | Loads the default @credentials@ INI file using the default profile name.
--
-- Throws 'MissingFileError' if 'credFile' is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
--
-- /See:/ 'credProfile', 'credFile', and 'envProfile'
fromFile :: (Applicative m, MonadIO m, MonadCatch m) => m (Auth, Maybe Region)
fromFile = do
  p <- lookupEnvOpt envProfile
  fromFilePath (maybe credProfile Text.pack p)
      =<< credFile

-- | Retrieve the access, secret and session token from the specified section
-- (profile) in a valid INI @credentials@ file.
--
-- Throws 'MissingFileError' if the specified file is missing, or 'InvalidFileError'
-- if an error occurs during parsing.
fromFilePath :: (Applicative m, MonadIO m, MonadCatch m)
             => Text
             -> FilePath
             -> m (Auth, Maybe Region)
fromFilePath n f = do
    p <- liftIO (doesFileExist f)
    unless p $
        throwM (MissingFileError f)
    ini <- either (invalidErr Nothing) return =<< liftIO (INI.readIniFile f)
    env <- AuthEnv
        <$> (req credAccessKey    ini <&> AccessKey)
        <*> (req credSecretKey    ini <&> Sensitive . SecretKey)
        <*> (opt credSessionToken ini <&> fmap (Sensitive . SessionToken))
        <*> return Nothing
    return (Auth env, Nothing)
  where
    req k i =
        case INI.lookupValue n k i of
            Left  e         -> invalidErr (Just k) e
            Right x
                | blank x   -> invalidErr (Just k) "cannot be a blank string."
                | otherwise -> return (Text.encodeUtf8 x)

    opt k i = return $
        case INI.lookupValue n k i of
            Left  _ -> Nothing
            Right x -> Just (Text.encodeUtf8 x)

    invalidErr Nothing  e = throwM $ InvalidFileError (Text.pack e)
    invalidErr (Just k) e = throwM $ InvalidFileError
        (Text.pack f <> ", key " <> k <> " " <> Text.pack e)

    blank x = Text.null x || Text.all isSpace x

-- | Retrieve the default IAM Profile from the local EC2 instance-data.
--
-- The default IAM profile is determined by Amazon as the first profile found
-- in the response from:
-- @http://169.254.169.254/latest/meta-data/iam/security-credentials/@
--
-- Throws 'RetrievalError' if the HTTP call fails, or 'InvalidIAMError' if
-- the default IAM profile cannot be read.
fromProfile :: (MonadIO m, MonadCatch m) => Manager -> m (Auth, Maybe Region)
fromProfile m = do
    ls <- try $ metadata m (IAM (SecurityCredentials Nothing))
    case BS8.lines <$> ls of
        Right (x:_) -> fromProfileName m (Text.decodeUtf8 x)
        Left  e     -> throwM (RetrievalError e)
        _           -> throwM $
            InvalidIAMError "Unable to get default IAM Profile from EC2 metadata"

-- | Lookup a specific IAM Profile by name from the local EC2 instance-data.
--
-- Additionally starts a refresh thread for the given authentication environment.
--
-- The resulting 'IORef' wrapper + timer is designed so that multiple concurrent
-- accesses of 'AuthEnv' from the 'AWS' environment are not required to calculate
-- expiry and sequentially queue to update it.
--
-- The forked timer ensures a singular owner and pre-emptive refresh of the
-- temporary session credentials before expiration.
--
-- A weak reference is used to ensure that the forked thread will eventually
-- terminate when 'Auth' is no longer referenced.
--
-- If no session token or expiration time is present the credentials will
-- be returned verbatim.
--
fromProfileName :: (MonadIO m, MonadCatch m)
                => Manager
                -> Text
                -> m (Auth, Maybe Region)
fromProfileName m name = do
    auth <- liftIO $ fetchAuthInBackground getCredentials
    reg  <- getRegion
    return (auth, Just reg)
  where
    getCredentials :: IO AuthEnv
    getCredentials =
        try (metadata m (IAM . SecurityCredentials $ Just name)) >>=
            handleErr (eitherDecode' . LBS8.fromStrict) invalidIAMErr

    getRegion :: (MonadIO m, MonadCatch m) => m Region
    getRegion =
       try (identity m) >>=
           handleErr (fmap _region) invalidIdentityErr

    handleErr _ _ (Left  e) = throwM (RetrievalError e)
    handleErr f g (Right x) = either (throwM . g) return (f x)

    invalidIAMErr = InvalidIAMError
        . mappend ("Error parsing IAM profile '" <> name <> "' ")
        . Text.pack

    invalidIdentityErr = InvalidIAMError
        . mappend "Error parsing Instance Identity Document "
        . Text.pack

-- | Obtain credentials exposed to a task via the ECS container agent, as
-- described in the <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
-- section of the AWS ECS documentation. The credentials are obtained by making
-- a request to <http://169.254.170.2> at the path contained by the
-- 'envContainerCredentialsURI' environment variable.
--
-- The ECS container agent provides an access key, secret key, session token,
-- and expiration time, but it does not include a region, so the region will
-- attempt to be determined from the 'envRegion' environment variable if it is
-- set.
--
-- Like 'fromProfileName', additionally starts a refresh thread that will
-- periodically fetch fresh credentials before the current ones expire.
--
-- Throws 'MissingEnvError' if the 'envContainerCredentialsURI' environment
-- variable is not set or 'InvalidIAMError' if the payload returned by the ECS
-- container agent is not of the expected format.
fromContainer :: (MonadIO m, MonadThrow m)
              => Manager
              -> m (Auth, Maybe Region)
fromContainer m = do
    req  <- getCredentialsURI
    auth <- liftIO $ fetchAuthInBackground (renew req)
    reg  <- getRegion
    return (auth, reg)
  where
    getCredentialsURI :: (MonadIO m, MonadThrow m) => m HTTP.Request
    getCredentialsURI = do
        mp <- lookupEnvOpt envContainerCredentialsURI
        p  <- maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> envContainerCredentialsURI)
                    return
                    mp
#if MIN_VERSION_http_client(0,4,30)
        parseUrlThrow $ "http://169.254.170.2" <> p
#else
        parseUrl $ "http://169.254.170.2" <> p
#endif

    renew :: HTTP.Request -> IO AuthEnv
    renew req = do
        rs <- httpLbs req m
        either (throwM . invalidIdentityErr) return (eitherDecode (responseBody rs))

    invalidIdentityErr = InvalidIAMError
        . mappend "Error parsing Task Identity Document "
        . Text.pack

    getRegion :: MonadIO m => m (Maybe Region)
    getRegion = runMaybeT $ do
        mr <- MaybeT $ lookupEnvOpt envVarRegion
        either (const . MaybeT $ return Nothing)
                return
                (fromText (Text.pack mr))

-- | Implements the background fetching behavior used by 'fromProfileName' and
-- 'fromContainer'. Given an 'IO' action that produces an 'AuthEnv', this spawns
-- a thread that mutates the 'IORef' returned in the resulting 'Auth' to keep
-- the temporary credentials up to date.
fetchAuthInBackground :: IO AuthEnv -> IO Auth
fetchAuthInBackground menv = menv >>= \(!env) -> liftIO $
    case _authExpiry env of
        Nothing -> return (Auth env)
        Just x  -> do
          r <- newIORef env
          p <- myThreadId
          s <- timer menv r p x
          return (Ref s r)
  where
    timer :: IO AuthEnv -> IORef AuthEnv -> ThreadId -> ISO8601 -> IO ThreadId
    timer ma !r !p !x = forkIO $ do
        s <- myThreadId
        w <- mkWeakIORef r (killThread s)
        loop ma w p x

    loop :: IO AuthEnv -> Weak (IORef AuthEnv) -> ThreadId -> ISO8601 -> IO ()
    loop ma w !p !x = do
        diff x <$> getCurrentTime >>= threadDelay
        env <- try ma
        case env of
            Left   e -> throwTo p (RetrievalError e)
            Right !a -> do
                 mr <- deRefWeak w
                 case mr of
                     Nothing -> return ()
                     Just  r -> do
                         atomicWriteIORef r a
                         maybe (return ()) (loop ma w p) (_authExpiry a)

    diff (Time !x) !y = (* 1000000) $ if n > 0 then n else 1
      where
        !n = truncate (diffUTCTime x y) - 60

lookupEnvOpt :: MonadIO m => Text -> m (Maybe FilePath)
lookupEnvOpt key = liftIO (lookupEnv (Text.unpack key))

lookupEnvReq :: (MonadIO m, MonadThrow m) => Text -> m FilePath
lookupEnvReq key = do
        m <- lookupEnvOpt key
        maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> key)
              return
              m
