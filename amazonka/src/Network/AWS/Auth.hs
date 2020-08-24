{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.Auth where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Maybe (MaybeT (..))


import           Network.AWS.AuthTypes
import           Network.AWS.BasicAuth
import qualified Network.AWS.EC2.Metadata as EC2
import           Network.AWS.Lens         (catching, catching_, throwingM,
                                           (<&>), (^.))

import Network.AWS.Prelude
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.HTTP.Conduit

import qualified Data.Text as Text



-- | Role arn to assume through web identity (see FromWebIdentityToken)
envAwsRoleArn :: Text -- ^ AWS_ROLE_ARN
envAwsRoleArn = "AWS_ROLE_ARN"

envWebIdentityTokenFile :: Text -- ^ AWS_WEB_IDENTITY_TOKEN_FILE
envWebIdentityTokenFile = "AWS_WEB_IDENTITY_TOKEN_FILE"

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
getAuth :: (Applicative m, MonadIO m, MonadCatch m)
        => Manager
        -> Credentials
        -> m (Auth, Maybe Region)
getAuth m = \case
    FromKeys    a s     -> return (fromKeys a s, Nothing)
    FromSession a s t   -> return (fromSession a s t, Nothing)
    FromEnv     a s t r -> fromEnvKeys a s t r
    FromProfile n       -> fromProfileName m n
    FromFile    n f     -> fromFilePath n f
    FromContainer       -> fromContainer m
    FromWebIdentityToken -> fromWebIdentityToken
    Discover            ->
        -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
        -- let both errors propagate.
        catching_ _MissingEnvError fromEnv $
            catching_ _MissingEnvError fromWebIdentityToken $
                -- proceed, missing env keys
                catching _MissingFileError fromFile $ \f ->
                    -- proceed, missing credentials file
                    catching_ _MissingEnvError (fromContainer m) $ do
                    -- proceed, missing env key
                    p <- EC2.isEC2 m
                    unless p $
                        -- not an EC2 instance, rethrow the previous error.
                        throwingM _MissingFileError f
                    -- proceed, check EC2 metadata for IAM information.
                    fromProfile m



fromWebIdentityToken :: (MonadIO m, MonadCatch m) => m (Auth, Maybe Region)
fromWebIdentityToken = do
    env <- newBasicEnv DiscoverBasic
    roleToAssume <- lookupEnvReq envAwsRoleArn <&> Text.pack
    tokenIdentityFile <- lookupEnvReq envWebIdentityTokenFile <&> Text.pack
    auth <- liftIO $ fetchAuthInBackground (renew env roleToAssume tokenIdentityFile)
    reg  <- getRegion
    return (auth, reg)
    where
        renew :: Env -> Text -> Text -> IO AuthEnv
        renew env roleToAssume tokenIdentityFile = do
            let assumeRoleReq = assumeRoleWithWebIdentity roleToAssume "" tokenIdentityFile
            assumeRoleResp <- runResourceT $ runAWST env $ send assumeRoleReq
            maybe (throwM . InvalidIAMError $ "No credentials returned") pure $ assumeRoleResp ^. arwwirsCredentials

        getRegion :: MonadIO m => m (Maybe Region)
        getRegion = runMaybeT $ do
            mr <- MaybeT $ lookupEnvOpt envVarRegion
            either (const . MaybeT $ return Nothing)
                    return
                    (fromText (Text.pack mr))


