{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types
    (
    -- * Service Configuration
      workSpaces

    -- * Errors
    , _InvalidResourceStateException
    , _ResourceAssociatedException
    , _WorkspacesDefaultRoleNotFoundException
    , _InvalidParameterValuesException
    , _UnsupportedWorkspaceConfigurationException
    , _ResourceCreationFailedException
    , _AccessDeniedException
    , _OperationNotSupportedException
    , _ResourceNotFoundException
    , _ResourceAlreadyExistsException
    , _ResourceLimitExceededException
    , _OperationInProgressException
    , _ResourceUnavailableException
    , _UnsupportedNetworkConfigurationException

    -- * AccessPropertyValue
    , AccessPropertyValue (..)

    -- * Compute
    , Compute (..)

    -- * ConnectionState
    , ConnectionState (..)

    -- * DedicatedTenancyModificationStateEnum
    , DedicatedTenancyModificationStateEnum (..)

    -- * DedicatedTenancySupportEnum
    , DedicatedTenancySupportEnum (..)

    -- * DedicatedTenancySupportResultEnum
    , DedicatedTenancySupportResultEnum (..)

    -- * ModificationResourceEnum
    , ModificationResourceEnum (..)

    -- * ModificationStateEnum
    , ModificationStateEnum (..)

    -- * OperatingSystemType
    , OperatingSystemType (..)

    -- * ReconnectEnum
    , ReconnectEnum (..)

    -- * RunningMode
    , RunningMode (..)

    -- * TargetWorkspaceState
    , TargetWorkspaceState (..)

    -- * Tenancy
    , Tenancy (..)

    -- * WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- * WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- * WorkspaceImageIngestionProcess
    , WorkspaceImageIngestionProcess (..)

    -- * WorkspaceImageRequiredTenancy
    , WorkspaceImageRequiredTenancy (..)

    -- * WorkspaceImageState
    , WorkspaceImageState (..)

    -- * WorkspaceState
    , WorkspaceState (..)

    -- * AccountModification
    , AccountModification
    , accountModification
    , amStartTime
    , amDedicatedTenancySupport
    , amModificationState
    , amDedicatedTenancyManagementCidrRange
    , amErrorCode
    , amErrorMessage

    -- * ClientProperties
    , ClientProperties
    , clientProperties
    , cpReconnectEnabled

    -- * ClientPropertiesResult
    , ClientPropertiesResult
    , clientPropertiesResult
    , cprResourceId
    , cprClientProperties

    -- * ComputeType
    , ComputeType
    , computeType
    , ctName

    -- * DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties
    , defaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpUserEnabledAsLocalAdministrator
    , dwcpEnableWorkDocs
    , dwcpEnableMaintenanceMode
    , dwcpEnableInternetAccess
    , dwcpDefaultOu

    -- * FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest
    , failedCreateWorkspaceRequest
    , fcwrWorkspaceRequest
    , fcwrErrorCode
    , fcwrErrorMessage

    -- * FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest
    , failedWorkspaceChangeRequest
    , fwcrErrorCode
    , fwcrWorkspaceId
    , fwcrErrorMessage

    -- * IPRuleItem
    , IPRuleItem
    , ipRuleItem
    , iriRuleDesc
    , iriIpRule

    -- * ModificationState
    , ModificationState
    , modificationState
    , msState
    , msResource

    -- * OperatingSystem
    , OperatingSystem
    , operatingSystem
    , osType

    -- * RebootRequest
    , RebootRequest
    , rebootRequest
    , rWorkspaceId

    -- * RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rrWorkspaceId

    -- * RootStorage
    , RootStorage
    , rootStorage
    , rsCapacity

    -- * SelfservicePermissions
    , SelfservicePermissions
    , selfservicePermissions
    , spRestartWorkspace
    , spChangeComputeType
    , spSwitchRunningMode
    , spRebuildWorkspace
    , spIncreaseVolumeSize

    -- * Snapshot
    , Snapshot
    , snapshot
    , sSnapshotTime

    -- * StartRequest
    , StartRequest
    , startRequest
    , sWorkspaceId

    -- * StopRequest
    , StopRequest
    , stopRequest
    , srWorkspaceId

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TerminateRequest
    , TerminateRequest
    , terminateRequest
    , trWorkspaceId

    -- * UserStorage
    , UserStorage
    , userStorage
    , usCapacity

    -- * Workspace
    , Workspace
    , workspace
    , wDirectoryId
    , wState
    , wIPAddress
    , wModificationStates
    , wUserName
    , wSubnetId
    , wBundleId
    , wWorkspaceProperties
    , wRootVolumeEncryptionEnabled
    , wErrorCode
    , wVolumeEncryptionKey
    , wComputerName
    , wWorkspaceId
    , wUserVolumeEncryptionEnabled
    , wErrorMessage

    -- * WorkspaceAccessProperties
    , WorkspaceAccessProperties
    , workspaceAccessProperties
    , wapDeviceTypeWindows
    , wapDeviceTypeWeb
    , wapDeviceTypeAndroid
    , wapDeviceTypeOSx
    , wapDeviceTypeChromeOS
    , wapDeviceTypeIos
    , wapDeviceTypeZeroClient

    -- * WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbLastUpdatedTime
    , wbBundleId
    , wbOwner
    , wbRootStorage
    , wbName
    , wbImageId
    , wbComputeType
    , wbUserStorage
    , wbDescription

    -- * WorkspaceConnectionStatus
    , WorkspaceConnectionStatus
    , workspaceConnectionStatus
    , wcsLastKnownUserConnectionTimestamp
    , wcsConnectionStateCheckTimestamp
    , wcsWorkspaceId
    , wcsConnectionState

    -- * WorkspaceCreationProperties
    , WorkspaceCreationProperties
    , workspaceCreationProperties
    , wcpCustomSecurityGroupId
    , wcpUserEnabledAsLocalAdministrator
    , wcpEnableMaintenanceMode
    , wcpEnableInternetAccess
    , wcpDefaultOu

    -- * WorkspaceDirectory
    , WorkspaceDirectory
    , workspaceDirectory
    , wdRegistrationCode
    , wdIAMRoleId
    , wdDirectoryId
    , wdState
    , wdCustomerUserName
    , wdSubnetIds
    , wdIpGroupIds
    , wdAlias
    , wdWorkspaceSecurityGroupId
    , wdDirectoryType
    , wdTenancy
    , wdWorkspaceCreationProperties
    , wdDNSIPAddresses
    , wdWorkspaceAccessProperties
    , wdDirectoryName
    , wdSelfservicePermissions

    -- * WorkspaceImage
    , WorkspaceImage
    , workspaceImage
    , wiState
    , wiOperatingSystem
    , wiRequiredTenancy
    , wiName
    , wiImageId
    , wiErrorCode
    , wiErrorMessage
    , wiDescription

    -- * WorkspaceProperties
    , WorkspaceProperties
    , workspaceProperties
    , wpComputeTypeName
    , wpRunningMode
    , wpRootVolumeSizeGib
    , wpRunningModeAutoStopTimeoutInMinutes
    , wpUserVolumeSizeGib

    -- * WorkspaceRequest
    , WorkspaceRequest
    , workspaceRequest
    , wrWorkspaceProperties
    , wrRootVolumeEncryptionEnabled
    , wrVolumeEncryptionKey
    , wrUserVolumeEncryptionEnabled
    , wrTags
    , wrDirectoryId
    , wrUserName
    , wrBundleId

    -- * WorkspacesIPGroup
    , WorkspacesIPGroup
    , workspacesIPGroup
    , wigGroupDesc
    , wigUserRules
    , wigGroupId
    , wigGroupName
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.WorkSpaces.Types.AccessPropertyValue
import Network.AWS.WorkSpaces.Types.Compute
import Network.AWS.WorkSpaces.Types.ConnectionState
import Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportEnum
import Network.AWS.WorkSpaces.Types.DedicatedTenancySupportResultEnum
import Network.AWS.WorkSpaces.Types.ModificationResourceEnum
import Network.AWS.WorkSpaces.Types.ModificationStateEnum
import Network.AWS.WorkSpaces.Types.OperatingSystemType
import Network.AWS.WorkSpaces.Types.ReconnectEnum
import Network.AWS.WorkSpaces.Types.RunningMode
import Network.AWS.WorkSpaces.Types.TargetWorkspaceState
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
import Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
import Network.AWS.WorkSpaces.Types.WorkspaceImageRequiredTenancy
import Network.AWS.WorkSpaces.Types.WorkspaceImageState
import Network.AWS.WorkSpaces.Types.WorkspaceState
import Network.AWS.WorkSpaces.Types.AccountModification
import Network.AWS.WorkSpaces.Types.ClientProperties
import Network.AWS.WorkSpaces.Types.ClientPropertiesResult
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Network.AWS.WorkSpaces.Types.IPRuleItem
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.RebootRequest
import Network.AWS.WorkSpaces.Types.RebuildRequest
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Snapshot
import Network.AWS.WorkSpaces.Types.StartRequest
import Network.AWS.WorkSpaces.Types.StopRequest
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.TerminateRequest
import Network.AWS.WorkSpaces.Types.UserStorage
import Network.AWS.WorkSpaces.Types.Workspace
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceBundle
import Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
import Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectory
import Network.AWS.WorkSpaces.Types.WorkspaceImage
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceRequest
import Network.AWS.WorkSpaces.Types.WorkspacesIPGroup

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
workSpaces :: Service
workSpaces
  = Service{_svcAbbrev = "WorkSpaces", _svcSigner = v4,
            _svcPrefix = "workspaces",
            _svcVersion = "2015-04-08",
            _svcEndpoint = defaultEndpoint workSpaces,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "WorkSpaces",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The state of the resource is not valid for this operation.
--
--
_InvalidResourceStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateException
  = _MatchServiceError workSpaces
      "InvalidResourceStateException"

-- | The resource is associated with a directory.
--
--
_ResourceAssociatedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAssociatedException
  = _MatchServiceError workSpaces
      "ResourceAssociatedException"

-- | The workspaces_DefaultRole role could not be found. If this is the first time you are registering a directory, you will need to create the workspaces_DefaultRole role before you can register a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/workspaces-access-control.html#create-default-role Creating the workspaces_DefaultRole Role> .
--
--
_WorkspacesDefaultRoleNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_WorkspacesDefaultRoleNotFoundException
  = _MatchServiceError workSpaces
      "WorkspacesDefaultRoleNotFoundException"

-- | One or more parameter values are not valid.
--
--
_InvalidParameterValuesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValuesException
  = _MatchServiceError workSpaces
      "InvalidParameterValuesException"

-- | The configuration of this WorkSpace is not supported for this operation. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/required-service-components.html Required Configuration and Service Components for WorkSpaces > .
--
--
_UnsupportedWorkspaceConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedWorkspaceConfigurationException
  = _MatchServiceError workSpaces
      "UnsupportedWorkspaceConfigurationException"

-- | The resource could not be created.
--
--
_ResourceCreationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceCreationFailedException
  = _MatchServiceError workSpaces
      "ResourceCreationFailedException"

-- | The user is not authorized to access a resource.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError workSpaces
      "AccessDeniedException"

-- | This operation is not supported.
--
--
_OperationNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotSupportedException
  = _MatchServiceError workSpaces
      "OperationNotSupportedException"

-- | The resource could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError workSpaces
      "ResourceNotFoundException"

-- | The specified resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException
  = _MatchServiceError workSpaces
      "ResourceAlreadyExistsException"

-- | Your resource limits have been exceeded.
--
--
_ResourceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceededException
  = _MatchServiceError workSpaces
      "ResourceLimitExceededException"

-- | The properties of this WorkSpace are currently being modified. Try again in a moment.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException
  = _MatchServiceError workSpaces
      "OperationInProgressException"

-- | The specified resource is not available.
--
--
_ResourceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceUnavailableException
  = _MatchServiceError workSpaces
      "ResourceUnavailableException"

-- | The configuration of this network is not supported for this operation, or your network configuration conflicts with the Amazon WorkSpaces management network IP range. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/amazon-workspaces-vpc.html Configure a VPC for Amazon WorkSpaces> .
--
--
_UnsupportedNetworkConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedNetworkConfigurationException
  = _MatchServiceError workSpaces
      "UnsupportedNetworkConfigurationException"
