{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types
    (
    -- * Service Configuration
      directoryService

    -- * Errors
    , _InvalidCertificateException
    , _DirectoryLimitExceededException
    , _InvalidLDAPSStatusException
    , _DirectoryDoesNotExistException
    , _SnapshotLimitExceededException
    , _DomainControllerLimitExceededException
    , _UserDoesNotExistException
    , _InsufficientPermissionsException
    , _ServiceException
    , _NoAvailableCertificateException
    , _InvalidParameterException
    , _OrganizationsException
    , _UnsupportedOperationException
    , _InvalidTargetException
    , _CertificateAlreadyExistsException
    , _CertificateLimitExceededException
    , _DirectoryUnavailableException
    , _AccessDeniedException
    , _AuthenticationFailedException
    , _DirectoryAlreadySharedException
    , _ClientException
    , _TagLimitExceededException
    , _DirectoryNotSharedException
    , _EntityAlreadyExistsException
    , _InvalidNextTokenException
    , _EntityDoesNotExistException
    , _ShareLimitExceededException
    , _IPRouteLimitExceededException
    , _InvalidPasswordException
    , _CertificateDoesNotExistException
    , _CertificateInUseException

    -- * CertificateState
    , CertificateState (..)

    -- * DirectoryEdition
    , DirectoryEdition (..)

    -- * DirectorySize
    , DirectorySize (..)

    -- * DirectoryStage
    , DirectoryStage (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * DomainControllerStatus
    , DomainControllerStatus (..)

    -- * IPRouteStatusMsg
    , IPRouteStatusMsg (..)

    -- * LDAPSStatus
    , LDAPSStatus (..)

    -- * LDAPSType
    , LDAPSType (..)

    -- * RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- * RadiusStatus
    , RadiusStatus (..)

    -- * ReplicationScope
    , ReplicationScope (..)

    -- * SchemaExtensionStatus
    , SchemaExtensionStatus (..)

    -- * SelectiveAuth
    , SelectiveAuth (..)

    -- * ShareMethod
    , ShareMethod (..)

    -- * ShareStatus
    , ShareStatus (..)

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * SnapshotType
    , SnapshotType (..)

    -- * TargetType
    , TargetType (..)

    -- * TopicStatus
    , TopicStatus (..)

    -- * TrustDirection
    , TrustDirection (..)

    -- * TrustState
    , TrustState (..)

    -- * TrustType
    , TrustType (..)

    -- * Attribute
    , Attribute
    , attribute
    , aValue
    , aName

    -- * Certificate
    , Certificate
    , certificate
    , cState
    , cCommonName
    , cCertificateId
    , cExpiryDateTime
    , cRegisteredDateTime
    , cStateReason

    -- * CertificateInfo
    , CertificateInfo
    , certificateInfo
    , ciState
    , ciCommonName
    , ciCertificateId
    , ciExpiryDateTime

    -- * Computer
    , Computer
    , computer
    , cComputerId
    , cComputerAttributes
    , cComputerName

    -- * ConditionalForwarder
    , ConditionalForwarder
    , conditionalForwarder
    , cfDNSIPAddrs
    , cfRemoteDomainName
    , cfReplicationScope

    -- * DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- * DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdCustomerUserName
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdSecurityGroupId
    , dcsdConnectIPs
    , dcsdAvailabilityZones

    -- * DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddEdition
    , ddRadiusStatus
    , ddStage
    , ddDirectoryId
    , ddAccessURL
    , ddShortName
    , ddSize
    , ddDesiredNumberOfDomainControllers
    , ddRadiusSettings
    , ddLaunchTime
    , ddAlias
    , ddShareStatus
    , ddName
    , ddShareMethod
    , ddStageLastUpdatedDateTime
    , ddSSOEnabled
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddStageReason
    , ddConnectSettings
    , ddOwnerDirectoryDescription
    , ddDescription
    , ddShareNotes

    -- * DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlCloudOnlyMicrosoftADLimitReached
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADLimit
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADCurrentCount

    -- * DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- * DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- * DomainController
    , DomainController
    , domainController
    , dcStatus
    , dcDirectoryId
    , dcVPCId
    , dcLaunchTime
    , dcSubnetId
    , dcAvailabilityZone
    , dcStatusLastUpdatedDateTime
    , dcStatusReason
    , dcDNSIPAddr
    , dcDomainControllerId

    -- * EventTopic
    , EventTopic
    , eventTopic
    , etStatus
    , etDirectoryId
    , etTopicName
    , etTopicARN
    , etCreatedDateTime

    -- * IPRoute
    , IPRoute
    , ipRoute
    , irCidrIP
    , irDescription

    -- * IPRouteInfo
    , IPRouteInfo
    , ipRouteInfo
    , iriDirectoryId
    , iriIPRouteStatusReason
    , iriAddedDateTime
    , iriCidrIP
    , iriIPRouteStatusMsg
    , iriDescription

    -- * LDAPSSettingInfo
    , LDAPSSettingInfo
    , lDAPSSettingInfo
    , ldapssiLastUpdatedDateTime
    , ldapssiLDAPSStatusReason
    , ldapssiLDAPSStatus

    -- * LogSubscription
    , LogSubscription
    , logSubscription
    , lsDirectoryId
    , lsLogGroupName
    , lsSubscriptionCreatedDateTime

    -- * OwnerDirectoryDescription
    , OwnerDirectoryDescription
    , ownerDirectoryDescription
    , oddRadiusStatus
    , oddDirectoryId
    , oddRadiusSettings
    , oddAccountId
    , oddDNSIPAddrs
    , oddVPCSettings

    -- * RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsDisplayLabel
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsRadiusServers
    , rsUseSameUsername
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- * SchemaExtensionInfo
    , SchemaExtensionInfo
    , schemaExtensionInfo
    , seiDirectoryId
    , seiSchemaExtensionId
    , seiSchemaExtensionStatusReason
    , seiSchemaExtensionStatus
    , seiDescription
    , seiEndDateTime
    , seiStartDateTime

    -- * ShareTarget
    , ShareTarget
    , shareTarget
    , stId
    , stType

    -- * SharedDirectory
    , SharedDirectory
    , sharedDirectory
    , sSharedAccountId
    , sOwnerAccountId
    , sLastUpdatedDateTime
    , sShareStatus
    , sShareMethod
    , sOwnerDirectoryId
    , sSharedDirectoryId
    , sShareNotes
    , sCreatedDateTime

    -- * Snapshot
    , Snapshot
    , snapshot
    , sStatus
    , sDirectoryId
    , sStartTime
    , sName
    , sType
    , sSnapshotId

    -- * SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * Trust
    , Trust
    , trust
    , tDirectoryId
    , tTrustState
    , tLastUpdatedDateTime
    , tTrustDirection
    , tStateLastUpdatedDateTime
    , tTrustType
    , tTrustStateReason
    , tSelectiveAuth
    , tRemoteDomainName
    , tTrustId
    , tCreatedDateTime

    -- * UnshareTarget
    , UnshareTarget
    , unshareTarget
    , utId
    , utType
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.DirectoryService.Types.DirectoryEdition
import Network.AWS.DirectoryService.Types.DirectorySize
import Network.AWS.DirectoryService.Types.DirectoryStage
import Network.AWS.DirectoryService.Types.DirectoryType
import Network.AWS.DirectoryService.Types.DomainControllerStatus
import Network.AWS.DirectoryService.Types.IPRouteStatusMsg
import Network.AWS.DirectoryService.Types.LDAPSStatus
import Network.AWS.DirectoryService.Types.LDAPSType
import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import Network.AWS.DirectoryService.Types.RadiusStatus
import Network.AWS.DirectoryService.Types.ReplicationScope
import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import Network.AWS.DirectoryService.Types.SelectiveAuth
import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import Network.AWS.DirectoryService.Types.SnapshotStatus
import Network.AWS.DirectoryService.Types.SnapshotType
import Network.AWS.DirectoryService.Types.TargetType
import Network.AWS.DirectoryService.Types.TopicStatus
import Network.AWS.DirectoryService.Types.TrustDirection
import Network.AWS.DirectoryService.Types.TrustState
import Network.AWS.DirectoryService.Types.TrustType
import Network.AWS.DirectoryService.Types.Attribute
import Network.AWS.DirectoryService.Types.Certificate
import Network.AWS.DirectoryService.Types.CertificateInfo
import Network.AWS.DirectoryService.Types.Computer
import Network.AWS.DirectoryService.Types.ConditionalForwarder
import Network.AWS.DirectoryService.Types.DirectoryConnectSettings
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryDescription
import Network.AWS.DirectoryService.Types.DirectoryLimits
import Network.AWS.DirectoryService.Types.DirectoryVPCSettings
import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.DomainController
import Network.AWS.DirectoryService.Types.EventTopic
import Network.AWS.DirectoryService.Types.IPRoute
import Network.AWS.DirectoryService.Types.IPRouteInfo
import Network.AWS.DirectoryService.Types.LDAPSSettingInfo
import Network.AWS.DirectoryService.Types.LogSubscription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.SchemaExtensionInfo
import Network.AWS.DirectoryService.Types.ShareTarget
import Network.AWS.DirectoryService.Types.SharedDirectory
import Network.AWS.DirectoryService.Types.Snapshot
import Network.AWS.DirectoryService.Types.SnapshotLimits
import Network.AWS.DirectoryService.Types.Tag
import Network.AWS.DirectoryService.Types.Trust
import Network.AWS.DirectoryService.Types.UnshareTarget

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
directoryService :: Service
directoryService
  = Service{_svcAbbrev = "DirectoryService",
            _svcSigner = v4, _svcPrefix = "ds",
            _svcVersion = "2015-04-16",
            _svcEndpoint = defaultEndpoint directoryService,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "DirectoryService",
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

-- | The certificate PEM that was provided has incorrect encoding.
--
--
_InvalidCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateException
  = _MatchServiceError directoryService
      "InvalidCertificateException"

-- | The maximum number of directories in the region has been reached. You can use the 'GetDirectoryLimits' operation to determine your directory limits in the region.
--
--
_DirectoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryLimitExceededException
  = _MatchServiceError directoryService
      "DirectoryLimitExceededException"

-- | The LDAP activities could not be performed because they are limited by the LDAPS status.
--
--
_InvalidLDAPSStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLDAPSStatusException
  = _MatchServiceError directoryService
      "InvalidLDAPSStatusException"

-- | The specified directory does not exist in the system.
--
--
_DirectoryDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryDoesNotExistException
  = _MatchServiceError directoryService
      "DirectoryDoesNotExistException"

-- | The maximum number of manual snapshots for the directory has been reached. You can use the 'GetSnapshotLimits' operation to determine the snapshot limits for a directory.
--
--
_SnapshotLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotLimitExceededException
  = _MatchServiceError directoryService
      "SnapshotLimitExceededException"

-- | The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.
--
--
_DomainControllerLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DomainControllerLimitExceededException
  = _MatchServiceError directoryService
      "DomainControllerLimitExceededException"

-- | The user provided a username that does not exist in your directory.
--
--
_UserDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_UserDoesNotExistException
  = _MatchServiceError directoryService
      "UserDoesNotExistException"

-- | The account does not have sufficient permission to perform the operation.
--
--
_InsufficientPermissionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPermissionsException
  = _MatchServiceError directoryService
      "InsufficientPermissionsException"

-- | An exception has occurred in AWS Directory Service.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException
  = _MatchServiceError directoryService
      "ServiceException"

-- | The LDAP activities could not be performed because at least one valid certificate must be registered with the system.
--
--
_NoAvailableCertificateException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableCertificateException
  = _MatchServiceError directoryService
      "NoAvailableCertificateException"

-- | One or more parameters are not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError directoryService
      "InvalidParameterException"

-- | Exception encountered while trying to access your AWS organization.
--
--
_OrganizationsException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationsException
  = _MatchServiceError directoryService
      "OrganizationsException"

-- | The operation is not supported.
--
--
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException
  = _MatchServiceError directoryService
      "UnsupportedOperationException"

-- | The specified shared target is not valid.
--
--
_InvalidTargetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTargetException
  = _MatchServiceError directoryService
      "InvalidTargetException"

-- | The certificate has already been registered into the system.
--
--
_CertificateAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateAlreadyExistsException
  = _MatchServiceError directoryService
      "CertificateAlreadyExistsException"

-- | The certificate could not be added because the certificate limit has been reached.
--
--
_CertificateLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateLimitExceededException
  = _MatchServiceError directoryService
      "CertificateLimitExceededException"

-- | The specified directory is unavailable or could not be found.
--
--
_DirectoryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryUnavailableException
  = _MatchServiceError directoryService
      "DirectoryUnavailableException"

-- | You do not have sufficient access to perform this action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError directoryService
      "AccessDeniedException"

-- | An authentication error occurred.
--
--
_AuthenticationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthenticationFailedException
  = _MatchServiceError directoryService
      "AuthenticationFailedException"

-- | The specified directory has already been shared with this AWS account.
--
--
_DirectoryAlreadySharedException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryAlreadySharedException
  = _MatchServiceError directoryService
      "DirectoryAlreadySharedException"

-- | A client exception has occurred.
--
--
_ClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientException
  = _MatchServiceError directoryService
      "ClientException"

-- | The maximum allowed number of tags was exceeded.
--
--
_TagLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededException
  = _MatchServiceError directoryService
      "TagLimitExceededException"

-- | The specified directory has not been shared with this AWS account.
--
--
_DirectoryNotSharedException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryNotSharedException
  = _MatchServiceError directoryService
      "DirectoryNotSharedException"

-- | The specified entity already exists.
--
--
_EntityAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException
  = _MatchServiceError directoryService
      "EntityAlreadyExistsException"

-- | The @NextToken@ value is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException
  = _MatchServiceError directoryService
      "InvalidNextTokenException"

-- | The specified entity could not be found.
--
--
_EntityDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityDoesNotExistException
  = _MatchServiceError directoryService
      "EntityDoesNotExistException"

-- | The maximum number of AWS accounts that you can share with this directory has been reached.
--
--
_ShareLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ShareLimitExceededException
  = _MatchServiceError directoryService
      "ShareLimitExceededException"

-- | The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.
--
--
_IPRouteLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_IPRouteLimitExceededException
  = _MatchServiceError directoryService
      "IpRouteLimitExceededException"

-- | The new password provided by the user does not meet the password complexity requirements defined in your directory.
--
--
_InvalidPasswordException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPasswordException
  = _MatchServiceError directoryService
      "InvalidPasswordException"

-- | The certificate is not present in the system for describe or deregister activities.
--
--
_CertificateDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateDoesNotExistException
  = _MatchServiceError directoryService
      "CertificateDoesNotExistException"

-- | The certificate is being used for the LDAP security connection and cannot be removed without disabling LDAP security.
--
--
_CertificateInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateInUseException
  = _MatchServiceError directoryService
      "CertificateInUseException"
