{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppLaunchStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.AppLaunchStatus (
  AppLaunchStatus (
    ..
    , ConfigurationInProgress
    , ConfigurationInvalid
    , DeltaLaunchFailed
    , DeltaLaunchInProgress
    , LaunchFailed
    , LaunchInProgress
    , LaunchPending
    , Launched
    , ReadyForConfiguration
    , ReadyForLaunch
    , TerminateFailed
    , TerminateInProgress
    , Terminated
    , ValidationInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AppLaunchStatus = AppLaunchStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ConfigurationInProgress :: AppLaunchStatus
pattern ConfigurationInProgress = AppLaunchStatus' "CONFIGURATION_IN_PROGRESS"

pattern ConfigurationInvalid :: AppLaunchStatus
pattern ConfigurationInvalid = AppLaunchStatus' "CONFIGURATION_INVALID"

pattern DeltaLaunchFailed :: AppLaunchStatus
pattern DeltaLaunchFailed = AppLaunchStatus' "DELTA_LAUNCH_FAILED"

pattern DeltaLaunchInProgress :: AppLaunchStatus
pattern DeltaLaunchInProgress = AppLaunchStatus' "DELTA_LAUNCH_IN_PROGRESS"

pattern LaunchFailed :: AppLaunchStatus
pattern LaunchFailed = AppLaunchStatus' "LAUNCH_FAILED"

pattern LaunchInProgress :: AppLaunchStatus
pattern LaunchInProgress = AppLaunchStatus' "LAUNCH_IN_PROGRESS"

pattern LaunchPending :: AppLaunchStatus
pattern LaunchPending = AppLaunchStatus' "LAUNCH_PENDING"

pattern Launched :: AppLaunchStatus
pattern Launched = AppLaunchStatus' "LAUNCHED"

pattern ReadyForConfiguration :: AppLaunchStatus
pattern ReadyForConfiguration = AppLaunchStatus' "READY_FOR_CONFIGURATION"

pattern ReadyForLaunch :: AppLaunchStatus
pattern ReadyForLaunch = AppLaunchStatus' "READY_FOR_LAUNCH"

pattern TerminateFailed :: AppLaunchStatus
pattern TerminateFailed = AppLaunchStatus' "TERMINATE_FAILED"

pattern TerminateInProgress :: AppLaunchStatus
pattern TerminateInProgress = AppLaunchStatus' "TERMINATE_IN_PROGRESS"

pattern Terminated :: AppLaunchStatus
pattern Terminated = AppLaunchStatus' "TERMINATED"

pattern ValidationInProgress :: AppLaunchStatus
pattern ValidationInProgress = AppLaunchStatus' "VALIDATION_IN_PROGRESS"

{-# COMPLETE
  ConfigurationInProgress,
  ConfigurationInvalid,
  DeltaLaunchFailed,
  DeltaLaunchInProgress,
  LaunchFailed,
  LaunchInProgress,
  LaunchPending,
  Launched,
  ReadyForConfiguration,
  ReadyForLaunch,
  TerminateFailed,
  TerminateInProgress,
  Terminated,
  ValidationInProgress,
  AppLaunchStatus' #-}

instance FromText AppLaunchStatus where
    parser = (AppLaunchStatus' . mk) <$> takeText

instance ToText AppLaunchStatus where
    toText (AppLaunchStatus' ci) = original ci

-- | Represents an enum of /known/ $AppLaunchStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AppLaunchStatus where
    toEnum i = case i of
        0 -> ConfigurationInProgress
        1 -> ConfigurationInvalid
        2 -> DeltaLaunchFailed
        3 -> DeltaLaunchInProgress
        4 -> LaunchFailed
        5 -> LaunchInProgress
        6 -> LaunchPending
        7 -> Launched
        8 -> ReadyForConfiguration
        9 -> ReadyForLaunch
        10 -> TerminateFailed
        11 -> TerminateInProgress
        12 -> Terminated
        13 -> ValidationInProgress
        _ -> (error . showText) $ "Unknown index for AppLaunchStatus: " <> toText i
    fromEnum x = case x of
        ConfigurationInProgress -> 0
        ConfigurationInvalid -> 1
        DeltaLaunchFailed -> 2
        DeltaLaunchInProgress -> 3
        LaunchFailed -> 4
        LaunchInProgress -> 5
        LaunchPending -> 6
        Launched -> 7
        ReadyForConfiguration -> 8
        ReadyForLaunch -> 9
        TerminateFailed -> 10
        TerminateInProgress -> 11
        Terminated -> 12
        ValidationInProgress -> 13
        AppLaunchStatus' name -> (error . showText) $ "Unknown AppLaunchStatus: " <> original name

-- | Represents the bounds of /known/ $AppLaunchStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AppLaunchStatus where
    minBound = ConfigurationInProgress
    maxBound = ValidationInProgress

instance Hashable     AppLaunchStatus
instance NFData       AppLaunchStatus
instance ToByteString AppLaunchStatus
instance ToQuery      AppLaunchStatus
instance ToHeader     AppLaunchStatus

instance FromJSON AppLaunchStatus where
    parseJSON = parseJSONText "AppLaunchStatus"
