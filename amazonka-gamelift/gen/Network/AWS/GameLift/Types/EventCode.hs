{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EventCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.EventCode (
  EventCode (
    ..
    , FleetActivationFailed
    , FleetActivationFailedNoInstances
    , FleetBinaryDownloadFailed
    , FleetCreated
    , FleetCreationExtractingBuild
    , FleetCreationRunningInstaller
    , FleetCreationValidatingRuntimeConfig
    , FleetDeleted
    , FleetInitializationFailed
    , FleetNewGameSessionProtectionPolicyUpdated
    , FleetScalingEvent
    , FleetStateActivating
    , FleetStateActive
    , FleetStateBuilding
    , FleetStateDownloading
    , FleetStateError
    , FleetStateValidating
    , FleetVPCPeeringDeleted
    , FleetVPCPeeringFailed
    , FleetVPCPeeringSucceeded
    , FleetValidationExecutableRuntimeFailure
    , FleetValidationLaunchPathNotFound
    , FleetValidationTimedOut
    , GameSessionActivationTimeout
    , GenericEvent
    , InstanceInterrupted
    , ServerProcessCrashed
    , ServerProcessForceTerminated
    , ServerProcessInvalidPath
    , ServerProcessProcessExitTimeout
    , ServerProcessProcessReadyTimeout
    , ServerProcessSDKInitializationTimeout
    , ServerProcessTerminatedUnhealthy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventCode = EventCode' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern FleetActivationFailed :: EventCode
pattern FleetActivationFailed = EventCode' "FLEET_ACTIVATION_FAILED"

pattern FleetActivationFailedNoInstances :: EventCode
pattern FleetActivationFailedNoInstances = EventCode' "FLEET_ACTIVATION_FAILED_NO_INSTANCES"

pattern FleetBinaryDownloadFailed :: EventCode
pattern FleetBinaryDownloadFailed = EventCode' "FLEET_BINARY_DOWNLOAD_FAILED"

pattern FleetCreated :: EventCode
pattern FleetCreated = EventCode' "FLEET_CREATED"

pattern FleetCreationExtractingBuild :: EventCode
pattern FleetCreationExtractingBuild = EventCode' "FLEET_CREATION_EXTRACTING_BUILD"

pattern FleetCreationRunningInstaller :: EventCode
pattern FleetCreationRunningInstaller = EventCode' "FLEET_CREATION_RUNNING_INSTALLER"

pattern FleetCreationValidatingRuntimeConfig :: EventCode
pattern FleetCreationValidatingRuntimeConfig = EventCode' "FLEET_CREATION_VALIDATING_RUNTIME_CONFIG"

pattern FleetDeleted :: EventCode
pattern FleetDeleted = EventCode' "FLEET_DELETED"

pattern FleetInitializationFailed :: EventCode
pattern FleetInitializationFailed = EventCode' "FLEET_INITIALIZATION_FAILED"

pattern FleetNewGameSessionProtectionPolicyUpdated :: EventCode
pattern FleetNewGameSessionProtectionPolicyUpdated = EventCode' "FLEET_NEW_GAME_SESSION_PROTECTION_POLICY_UPDATED"

pattern FleetScalingEvent :: EventCode
pattern FleetScalingEvent = EventCode' "FLEET_SCALING_EVENT"

pattern FleetStateActivating :: EventCode
pattern FleetStateActivating = EventCode' "FLEET_STATE_ACTIVATING"

pattern FleetStateActive :: EventCode
pattern FleetStateActive = EventCode' "FLEET_STATE_ACTIVE"

pattern FleetStateBuilding :: EventCode
pattern FleetStateBuilding = EventCode' "FLEET_STATE_BUILDING"

pattern FleetStateDownloading :: EventCode
pattern FleetStateDownloading = EventCode' "FLEET_STATE_DOWNLOADING"

pattern FleetStateError :: EventCode
pattern FleetStateError = EventCode' "FLEET_STATE_ERROR"

pattern FleetStateValidating :: EventCode
pattern FleetStateValidating = EventCode' "FLEET_STATE_VALIDATING"

pattern FleetVPCPeeringDeleted :: EventCode
pattern FleetVPCPeeringDeleted = EventCode' "FLEET_VPC_PEERING_DELETED"

pattern FleetVPCPeeringFailed :: EventCode
pattern FleetVPCPeeringFailed = EventCode' "FLEET_VPC_PEERING_FAILED"

pattern FleetVPCPeeringSucceeded :: EventCode
pattern FleetVPCPeeringSucceeded = EventCode' "FLEET_VPC_PEERING_SUCCEEDED"

pattern FleetValidationExecutableRuntimeFailure :: EventCode
pattern FleetValidationExecutableRuntimeFailure = EventCode' "FLEET_VALIDATION_EXECUTABLE_RUNTIME_FAILURE"

pattern FleetValidationLaunchPathNotFound :: EventCode
pattern FleetValidationLaunchPathNotFound = EventCode' "FLEET_VALIDATION_LAUNCH_PATH_NOT_FOUND"

pattern FleetValidationTimedOut :: EventCode
pattern FleetValidationTimedOut = EventCode' "FLEET_VALIDATION_TIMED_OUT"

pattern GameSessionActivationTimeout :: EventCode
pattern GameSessionActivationTimeout = EventCode' "GAME_SESSION_ACTIVATION_TIMEOUT"

pattern GenericEvent :: EventCode
pattern GenericEvent = EventCode' "GENERIC_EVENT"

pattern InstanceInterrupted :: EventCode
pattern InstanceInterrupted = EventCode' "INSTANCE_INTERRUPTED"

pattern ServerProcessCrashed :: EventCode
pattern ServerProcessCrashed = EventCode' "SERVER_PROCESS_CRASHED"

pattern ServerProcessForceTerminated :: EventCode
pattern ServerProcessForceTerminated = EventCode' "SERVER_PROCESS_FORCE_TERMINATED"

pattern ServerProcessInvalidPath :: EventCode
pattern ServerProcessInvalidPath = EventCode' "SERVER_PROCESS_INVALID_PATH"

pattern ServerProcessProcessExitTimeout :: EventCode
pattern ServerProcessProcessExitTimeout = EventCode' "SERVER_PROCESS_PROCESS_EXIT_TIMEOUT"

pattern ServerProcessProcessReadyTimeout :: EventCode
pattern ServerProcessProcessReadyTimeout = EventCode' "SERVER_PROCESS_PROCESS_READY_TIMEOUT"

pattern ServerProcessSDKInitializationTimeout :: EventCode
pattern ServerProcessSDKInitializationTimeout = EventCode' "SERVER_PROCESS_SDK_INITIALIZATION_TIMEOUT"

pattern ServerProcessTerminatedUnhealthy :: EventCode
pattern ServerProcessTerminatedUnhealthy = EventCode' "SERVER_PROCESS_TERMINATED_UNHEALTHY"

{-# COMPLETE
  FleetActivationFailed,
  FleetActivationFailedNoInstances,
  FleetBinaryDownloadFailed,
  FleetCreated,
  FleetCreationExtractingBuild,
  FleetCreationRunningInstaller,
  FleetCreationValidatingRuntimeConfig,
  FleetDeleted,
  FleetInitializationFailed,
  FleetNewGameSessionProtectionPolicyUpdated,
  FleetScalingEvent,
  FleetStateActivating,
  FleetStateActive,
  FleetStateBuilding,
  FleetStateDownloading,
  FleetStateError,
  FleetStateValidating,
  FleetVPCPeeringDeleted,
  FleetVPCPeeringFailed,
  FleetVPCPeeringSucceeded,
  FleetValidationExecutableRuntimeFailure,
  FleetValidationLaunchPathNotFound,
  FleetValidationTimedOut,
  GameSessionActivationTimeout,
  GenericEvent,
  InstanceInterrupted,
  ServerProcessCrashed,
  ServerProcessForceTerminated,
  ServerProcessInvalidPath,
  ServerProcessProcessExitTimeout,
  ServerProcessProcessReadyTimeout,
  ServerProcessSDKInitializationTimeout,
  ServerProcessTerminatedUnhealthy,
  EventCode' #-}

instance FromText EventCode where
    parser = (EventCode' . mk) <$> takeText

instance ToText EventCode where
    toText (EventCode' ci) = original ci

-- | Represents an enum of /known/ $EventCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventCode where
    toEnum i = case i of
        0 -> FleetActivationFailed
        1 -> FleetActivationFailedNoInstances
        2 -> FleetBinaryDownloadFailed
        3 -> FleetCreated
        4 -> FleetCreationExtractingBuild
        5 -> FleetCreationRunningInstaller
        6 -> FleetCreationValidatingRuntimeConfig
        7 -> FleetDeleted
        8 -> FleetInitializationFailed
        9 -> FleetNewGameSessionProtectionPolicyUpdated
        10 -> FleetScalingEvent
        11 -> FleetStateActivating
        12 -> FleetStateActive
        13 -> FleetStateBuilding
        14 -> FleetStateDownloading
        15 -> FleetStateError
        16 -> FleetStateValidating
        17 -> FleetVPCPeeringDeleted
        18 -> FleetVPCPeeringFailed
        19 -> FleetVPCPeeringSucceeded
        20 -> FleetValidationExecutableRuntimeFailure
        21 -> FleetValidationLaunchPathNotFound
        22 -> FleetValidationTimedOut
        23 -> GameSessionActivationTimeout
        24 -> GenericEvent
        25 -> InstanceInterrupted
        26 -> ServerProcessCrashed
        27 -> ServerProcessForceTerminated
        28 -> ServerProcessInvalidPath
        29 -> ServerProcessProcessExitTimeout
        30 -> ServerProcessProcessReadyTimeout
        31 -> ServerProcessSDKInitializationTimeout
        32 -> ServerProcessTerminatedUnhealthy
        _ -> (error . showText) $ "Unknown index for EventCode: " <> toText i
    fromEnum x = case x of
        FleetActivationFailed -> 0
        FleetActivationFailedNoInstances -> 1
        FleetBinaryDownloadFailed -> 2
        FleetCreated -> 3
        FleetCreationExtractingBuild -> 4
        FleetCreationRunningInstaller -> 5
        FleetCreationValidatingRuntimeConfig -> 6
        FleetDeleted -> 7
        FleetInitializationFailed -> 8
        FleetNewGameSessionProtectionPolicyUpdated -> 9
        FleetScalingEvent -> 10
        FleetStateActivating -> 11
        FleetStateActive -> 12
        FleetStateBuilding -> 13
        FleetStateDownloading -> 14
        FleetStateError -> 15
        FleetStateValidating -> 16
        FleetVPCPeeringDeleted -> 17
        FleetVPCPeeringFailed -> 18
        FleetVPCPeeringSucceeded -> 19
        FleetValidationExecutableRuntimeFailure -> 20
        FleetValidationLaunchPathNotFound -> 21
        FleetValidationTimedOut -> 22
        GameSessionActivationTimeout -> 23
        GenericEvent -> 24
        InstanceInterrupted -> 25
        ServerProcessCrashed -> 26
        ServerProcessForceTerminated -> 27
        ServerProcessInvalidPath -> 28
        ServerProcessProcessExitTimeout -> 29
        ServerProcessProcessReadyTimeout -> 30
        ServerProcessSDKInitializationTimeout -> 31
        ServerProcessTerminatedUnhealthy -> 32
        EventCode' name -> (error . showText) $ "Unknown EventCode: " <> original name

-- | Represents the bounds of /known/ $EventCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventCode where
    minBound = FleetActivationFailed
    maxBound = ServerProcessTerminatedUnhealthy

instance Hashable     EventCode
instance NFData       EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromJSON EventCode where
    parseJSON = parseJSONText "EventCode"
