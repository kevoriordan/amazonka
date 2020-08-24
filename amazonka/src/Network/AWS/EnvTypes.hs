{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Network.AWS.EnvTypes (HasEnv(..), Env(..))

where

import Data.IORef
import Data.Monoid


import Network.AWS.Internal.Logger
import Network.AWS.Lens            (Getter, Lens', lens, to)

import Network.AWS.Types
import Network.HTTP.Conduit




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

