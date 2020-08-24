{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.AWS.AuthTypes

where

import Control.Monad.Catch

import Network.AWS.Data
import Network.AWS.Lens (Prism', exception, prism)

import qualified Data.ByteString.Char8 as BS8
import           Network.AWS.Prelude


-- | Determines how AuthN/AuthZ information is retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys. See 'fromKeys'.

    | FromSession AccessKey SecretKey SessionToken
      -- ^ Explicit access key, secret key and a session token. See 'fromSession'.

    | FromEnv Text Text (Maybe Text) (Maybe Text)
      -- ^ Lookup specific environment variables for access key, secret key,
      -- an optional session token, and an optional region, respectively.

    | FromProfile Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
      -- Environment variables to lookup for the access key, secret key and
      -- optional session token.

    | FromFile Text FilePath
      -- ^ A credentials profile name (the INI section) and the path to the AWS
      -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file.

    | FromContainer
      -- ^ Obtain credentials by attempting to contact the ECS container agent
      -- at <http://169.254.170.2> using the path in 'envContainerCredentialsURI'.
      -- See <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
      -- in the AWS documentation for more information.

    | FromWebIdentityToken
      -- ^ Obtain credentials from web identity token file supplied by OIDC provider
      -- See IAM roles for service accounts for example (https://aws.amazon.com/blogs/opensource/introducing-fine-grained-iam-roles-service-accounts/)

    | Discover
      -- ^ Attempt credentials discovery via the following steps:
      --
      -- * Read the 'envAccessKey', 'envSecretKey', and 'envRegion' from the environment if they are set.
      --
      -- * Read the credentials file if 'credFile' exists.
      --
      -- * Obtain credentials from the ECS container agent if
      -- 'envContainerCredentialsURI' is set.
      --
      -- * Retrieve the first available IAM profile and read
      -- the 'Region' from the instance identity document, if running on EC2.
      --
      -- An attempt is made to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog Credentials where
    build = \case
        FromKeys    a _ ->
            "FromKeys " <> build a <> " ****"
        FromSession a _ _ ->
            "FromSession " <> build a <> " **** ****"
        FromEnv     a s t r ->
            "FromEnv " <> build a <> " " <> build s <> " " <> m t <> " " <> m r
        FromProfile n ->
            "FromProfile " <> build n
        FromFile    n f ->
            "FromFile " <> build n <> " " <> build f
        FromContainer ->
            "FromContainer"
        FromWebIdentityToken ->
            "FromWebIdentityToken"
        Discover ->
            "Discover"
      where
        m (Just x) = "(Just " <> build x <> ")"
        m Nothing  = "Nothing"

instance Show Credentials where
    show = BS8.unpack . toBS . build

-- | Determines how AuthN/AuthZ information is retrieved.
data BasicCredentials
    = FromKeysBasic AccessKey SecretKey
      -- ^ Explicit access and secret keys. See 'fromKeys'.

    | FromSessionBasic AccessKey SecretKey SessionToken
      -- ^ Explicit access key, secret key and a session token. See 'fromSession'.

    | FromEnvBasic Text Text (Maybe Text) (Maybe Text)
      -- ^ Lookup specific environment variables for access key, secret key,
      -- an optional session token, and an optional region, respectively.

    | FromProfileBasic Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
      -- Environment variables to lookup for the access key, secret key and
      -- optional session token.

    | FromFileBasic Text FilePath
      -- ^ A credentials profile name (the INI section) and the path to the AWS
      -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file.

    | FromContainerBasic
      -- ^ Obtain credentials by attempting to contact the ECS container agent
      -- at <http://169.254.170.2> using the path in 'envContainerCredentialsURI'.
      -- See <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
      -- in the AWS documentation for more information.

    | DiscoverBasic
      -- ^ Attempt credentials discovery via the following steps:
      --
      -- * Read the 'envAccessKey', 'envSecretKey', and 'envRegion' from the environment if they are set.
      --
      -- * Read the credentials file if 'credFile' exists.
      --
      -- * Obtain credentials from the ECS container agent if
      -- 'envContainerCredentialsURI' is set.
      --
      -- * Retrieve the first available IAM profile and read
      -- the 'Region' from the instance identity document, if running on EC2.
      --
      -- An attempt is made to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog BasicCredentials where
    build = \case
        FromKeysBasic    a _ ->
            "FromKeysBasic " <> build a <> " ****"
        FromSessionBasic a _ _ ->
            "FromSessionBasic " <> build a <> " **** ****"
        FromEnvBasic     a s t r ->
            "FromEnvBasic " <> build a <> " " <> build s <> " " <> m t <> " " <> m r
        FromProfileBasic n ->
            "FromProfileBasic " <> build n
        FromFileBasic    n f ->
            "FromFileBasic " <> build n <> " " <> build f
        FromContainerBasic ->
            "FromContainerBasic"
        DiscoverBasic ->
            "DiscoverBasic"
      where
        m (Just x) = "(Just " <> build x <> ")"
        m Nothing  = "Nothing"

instance Show BasicCredentials where
    show = BS8.unpack . toBS . build


-- | An error thrown when attempting to read AuthN/AuthZ information.
data AuthError
    = RetrievalError   HttpException
    | MissingEnvError  Text
    | InvalidEnvError  Text
    | MissingFileError FilePath
    | InvalidFileError Text
    | InvalidIAMError  Text
      deriving (Show, Typeable)

instance Exception AuthError

instance ToLog AuthError where
    build = \case
        RetrievalError   e -> build e
        MissingEnvError  e -> "[MissingEnvError]  { message = " <> build e <> "}"
        InvalidEnvError  e -> "[InvalidEnvError]  { message = " <> build e <> "}"
        MissingFileError f -> "[MissingFileError] { path = "    <> build f <> "}"
        InvalidFileError e -> "[InvalidFileError] { message = " <> build e <> "}"
        InvalidIAMError  e -> "[InvalidIAMError]  { message = " <> build e <> "}"

class AsAuthError a where
    -- | A general authentication error.
    _AuthError        :: Prism' a AuthError
    {-# MINIMAL _AuthError #-}

    -- | An error occured while communicating over HTTP with
    -- the local metadata endpoint.
    _RetrievalError   :: Prism' a HttpException

    -- | The named environment variable was not found.
    _MissingEnvError  :: Prism' a Text

    -- | An error occured parsing named environment variable's value.
    _InvalidEnvError  :: Prism' a Text

    -- | The specified credentials file could not be found.
    _MissingFileError :: Prism' a FilePath

    -- | An error occured parsing the credentials file.
    _InvalidFileError :: Prism' a Text

    -- | The specified IAM profile could not be found or deserialised.
    _InvalidIAMError  :: Prism' a Text

    _RetrievalError   = _AuthError . _RetrievalError
    _MissingEnvError  = _AuthError . _MissingEnvError
    _InvalidEnvError  = _AuthError . _InvalidEnvError
    _MissingFileError = _AuthError . _MissingFileError
    _InvalidFileError = _AuthError . _InvalidFileError
    _InvalidIAMError  = _AuthError . _InvalidIAMError

instance AsAuthError SomeException where
    _AuthError = exception

instance AsAuthError AuthError where
    _AuthError = id

    _RetrievalError = prism RetrievalError $ \case
        RetrievalError   e -> Right e
        x                  -> Left  x

    _MissingEnvError = prism MissingEnvError $ \case
        MissingEnvError  e -> Right e
        x                  -> Left  x

    _InvalidEnvError = prism InvalidEnvError $ \case
        InvalidEnvError  e -> Right e
        x                  -> Left  x

    _MissingFileError = prism MissingFileError $ \case
        MissingFileError f -> Right f
        x                  -> Left  x

    _InvalidFileError = prism InvalidFileError $ \case
        InvalidFileError e -> Right e
        x                  -> Left  x

    _InvalidIAMError = prism InvalidIAMError $ \case
        InvalidIAMError  e -> Right e
        x                  -> Left  x

