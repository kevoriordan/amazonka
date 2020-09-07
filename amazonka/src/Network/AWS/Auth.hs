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
module Network.AWS.Auth
    (
    -- * Authentication
    -- ** Retrieving Authentication
      getAuth
    , Credentials  (..)
    , Auth         (..)

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

    -- ** Keys
    , AccessKey    (..)
    , SecretKey    (..)
    , SessionToken (..)

    -- ** Handling Errors
    , AsAuthError  (..)
    , AuthError    (..)
    ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class


import Network.AWS.Data.Log
import Network.AWS.EC2.Metadata
import Network.AWS.Internal.Auth
import Network.AWS.Lens          (catching, catching_, throwingM)
import Network.AWS.Prelude
import Network.HTTP.Conduit


import qualified Data.ByteString.Char8 as BS8




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
        Discover ->
            "Discover"
      where
        m (Just x) = "(Just " <> build x <> ")"
        m Nothing  = "Nothing"

instance Show Credentials where
    show = BS8.unpack . toBS . build


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
    Discover            ->
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

