{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types
    (
    -- * Service Configuration
      elasticSearch

    -- * Errors
    , _DisabledOperationException
    , _AccessDeniedException
    , _ValidationException
    , _ResourceNotFoundException
    , _ResourceAlreadyExistsException
    , _InternalException
    , _BaseException
    , _LimitExceededException
    , _ConflictException
    , _InvalidTypeException

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * DescribePackagesFilterName
    , DescribePackagesFilterName (..)

    -- * DomainPackageStatus
    , DomainPackageStatus (..)

    -- * ESPartitionInstanceType
    , ESPartitionInstanceType (..)

    -- * ESWarmPartitionInstanceType
    , ESWarmPartitionInstanceType (..)

    -- * LogType
    , LogType (..)

    -- * OptionState
    , OptionState (..)

    -- * PackageStatus
    , PackageStatus (..)

    -- * PackageType
    , PackageType (..)

    -- * ReservedElasticsearchInstancePaymentOption
    , ReservedElasticsearchInstancePaymentOption (..)

    -- * TLSSecurityPolicy
    , TLSSecurityPolicy (..)

    -- * UpgradeStatus
    , UpgradeStatus (..)

    -- * UpgradeStep
    , UpgradeStep (..)

    -- * VolumeType
    , VolumeType (..)

    -- * AccessPoliciesStatus
    , AccessPoliciesStatus
    , accessPoliciesStatus
    , apsOptions
    , apsStatus

    -- * AdditionalLimit
    , AdditionalLimit
    , additionalLimit
    , alLimitName
    , alLimitValues

    -- * AdvancedOptionsStatus
    , AdvancedOptionsStatus
    , advancedOptionsStatus
    , aosOptions
    , aosStatus

    -- * AdvancedSecurityOptions
    , AdvancedSecurityOptions
    , advancedSecurityOptions
    , asoEnabled
    , asoInternalUserDatabaseEnabled

    -- * AdvancedSecurityOptionsInput
    , AdvancedSecurityOptionsInput
    , advancedSecurityOptionsInput
    , asoiEnabled
    , asoiInternalUserDatabaseEnabled
    , asoiMasterUserOptions

    -- * AdvancedSecurityOptionsStatus
    , AdvancedSecurityOptionsStatus
    , advancedSecurityOptionsStatus
    , asosOptions
    , asosStatus

    -- * CognitoOptions
    , CognitoOptions
    , cognitoOptions
    , coIdentityPoolId
    , coEnabled
    , coUserPoolId
    , coRoleARN

    -- * CognitoOptionsStatus
    , CognitoOptionsStatus
    , cognitoOptionsStatus
    , cosOptions
    , cosStatus

    -- * CompatibleVersionsMap
    , CompatibleVersionsMap
    , compatibleVersionsMap
    , cvmSourceVersion
    , cvmTargetVersions

    -- * DescribePackagesFilter
    , DescribePackagesFilter
    , describePackagesFilter
    , dpfValue
    , dpfName

    -- * DomainEndpointOptions
    , DomainEndpointOptions
    , domainEndpointOptions
    , deoEnforceHTTPS
    , deoTLSSecurityPolicy

    -- * DomainEndpointOptionsStatus
    , DomainEndpointOptionsStatus
    , domainEndpointOptionsStatus
    , deosOptions
    , deosStatus

    -- * DomainInfo
    , DomainInfo
    , domainInfo
    , diDomainName

    -- * DomainPackageDetails
    , DomainPackageDetails
    , domainPackageDetails
    , dpdLastUpdated
    , dpdPackageId
    , dpdPackageType
    , dpdPackageName
    , dpdDomainPackageStatus
    , dpdDomainName
    , dpdErrorDetails
    , dpdReferencePath

    -- * EBSOptions
    , EBSOptions
    , ebsOptions
    , eoVolumeSize
    , eoIOPS
    , eoVolumeType
    , eoEBSEnabled

    -- * EBSOptionsStatus
    , EBSOptionsStatus
    , ebsOptionsStatus
    , eosOptions
    , eosStatus

    -- * ElasticsearchClusterConfig
    , ElasticsearchClusterConfig
    , elasticsearchClusterConfig
    , eccDedicatedMasterCount
    , eccDedicatedMasterType
    , eccDedicatedMasterEnabled
    , eccInstanceCount
    , eccZoneAwarenessEnabled
    , eccInstanceType
    , eccWarmEnabled
    , eccZoneAwarenessConfig
    , eccWarmCount
    , eccWarmType

    -- * ElasticsearchClusterConfigStatus
    , ElasticsearchClusterConfigStatus
    , elasticsearchClusterConfigStatus
    , eccsOptions
    , eccsStatus

    -- * ElasticsearchDomainConfig
    , ElasticsearchDomainConfig
    , elasticsearchDomainConfig
    , edcEBSOptions
    , edcNodeToNodeEncryptionOptions
    , edcAccessPolicies
    , edcLogPublishingOptions
    , edcAdvancedSecurityOptions
    , edcElasticsearchClusterConfig
    , edcSnapshotOptions
    , edcCognitoOptions
    , edcEncryptionAtRestOptions
    , edcVPCOptions
    , edcDomainEndpointOptions
    , edcAdvancedOptions
    , edcElasticsearchVersion

    -- * ElasticsearchDomainStatus
    , ElasticsearchDomainStatus
    , elasticsearchDomainStatus
    , edsEBSOptions
    , edsNodeToNodeEncryptionOptions
    , edsAccessPolicies
    , edsServiceSoftwareOptions
    , edsLogPublishingOptions
    , edsAdvancedSecurityOptions
    , edsCreated
    , edsSnapshotOptions
    , edsCognitoOptions
    , edsEncryptionAtRestOptions
    , edsDeleted
    , edsVPCOptions
    , edsEndpoints
    , edsDomainEndpointOptions
    , edsProcessing
    , edsEndpoint
    , edsUpgradeProcessing
    , edsAdvancedOptions
    , edsElasticsearchVersion
    , edsDomainId
    , edsDomainName
    , edsARN
    , edsElasticsearchClusterConfig

    -- * ElasticsearchVersionStatus
    , ElasticsearchVersionStatus
    , elasticsearchVersionStatus
    , evsOptions
    , evsStatus

    -- * EncryptionAtRestOptions
    , EncryptionAtRestOptions
    , encryptionAtRestOptions
    , earoEnabled
    , earoKMSKeyId

    -- * EncryptionAtRestOptionsStatus
    , EncryptionAtRestOptionsStatus
    , encryptionAtRestOptionsStatus
    , earosOptions
    , earosStatus

    -- * ErrorDetails
    , ErrorDetails
    , errorDetails
    , edErrorType
    , edErrorMessage

    -- * InstanceCountLimits
    , InstanceCountLimits
    , instanceCountLimits
    , iclMaximumInstanceCount
    , iclMinimumInstanceCount

    -- * InstanceLimits
    , InstanceLimits
    , instanceLimits
    , ilInstanceCountLimits

    -- * Limits
    , Limits
    , limits
    , lInstanceLimits
    , lAdditionalLimits
    , lStorageTypes

    -- * LogPublishingOption
    , LogPublishingOption
    , logPublishingOption
    , lpoEnabled
    , lpoCloudWatchLogsLogGroupARN

    -- * LogPublishingOptionsStatus
    , LogPublishingOptionsStatus
    , logPublishingOptionsStatus
    , lposStatus
    , lposOptions

    -- * MasterUserOptions
    , MasterUserOptions
    , masterUserOptions
    , muoMasterUserPassword
    , muoMasterUserName
    , muoMasterUserARN

    -- * NodeToNodeEncryptionOptions
    , NodeToNodeEncryptionOptions
    , nodeToNodeEncryptionOptions
    , ntneoEnabled

    -- * NodeToNodeEncryptionOptionsStatus
    , NodeToNodeEncryptionOptionsStatus
    , nodeToNodeEncryptionOptionsStatus
    , ntneosOptions
    , ntneosStatus

    -- * OptionStatus
    , OptionStatus
    , optionStatus
    , osPendingDeletion
    , osUpdateVersion
    , osCreationDate
    , osUpdateDate
    , osState

    -- * PackageDetails
    , PackageDetails
    , packageDetails
    , pdPackageId
    , pdPackageType
    , pdCreatedAt
    , pdPackageName
    , pdPackageStatus
    , pdPackageDescription
    , pdErrorDetails

    -- * PackageSource
    , PackageSource
    , packageSource
    , psS3Key
    , psS3BucketName

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReservedElasticsearchInstance
    , ReservedElasticsearchInstance
    , reservedElasticsearchInstance
    , reiState
    , reiCurrencyCode
    , reiStartTime
    , reiReservedElasticsearchInstanceOfferingId
    , reiReservedElasticsearchInstanceId
    , reiElasticsearchInstanceCount
    , reiReservationName
    , reiElasticsearchInstanceType
    , reiRecurringCharges
    , reiUsagePrice
    , reiFixedPrice
    , reiDuration
    , reiPaymentOption

    -- * ReservedElasticsearchInstanceOffering
    , ReservedElasticsearchInstanceOffering
    , reservedElasticsearchInstanceOffering
    , reioCurrencyCode
    , reioReservedElasticsearchInstanceOfferingId
    , reioElasticsearchInstanceType
    , reioRecurringCharges
    , reioUsagePrice
    , reioFixedPrice
    , reioDuration
    , reioPaymentOption

    -- * ServiceSoftwareOptions
    , ServiceSoftwareOptions
    , serviceSoftwareOptions
    , ssoAutomatedUpdateDate
    , ssoCurrentVersion
    , ssoOptionalDeployment
    , ssoUpdateStatus
    , ssoCancellable
    , ssoUpdateAvailable
    , ssoDescription
    , ssoNewVersion

    -- * SnapshotOptions
    , SnapshotOptions
    , snapshotOptions
    , soAutomatedSnapshotStartHour

    -- * SnapshotOptionsStatus
    , SnapshotOptionsStatus
    , snapshotOptionsStatus
    , sosOptions
    , sosStatus

    -- * StorageType
    , StorageType
    , storageType
    , stStorageTypeLimits
    , stStorageSubTypeName
    , stStorageTypeName

    -- * StorageTypeLimit
    , StorageTypeLimit
    , storageTypeLimit
    , stlLimitName
    , stlLimitValues

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * UpgradeHistory
    , UpgradeHistory
    , upgradeHistory
    , uhUpgradeStatus
    , uhStepsList
    , uhUpgradeName
    , uhStartTimestamp

    -- * UpgradeStepItem
    , UpgradeStepItem
    , upgradeStepItem
    , usiUpgradeStepStatus
    , usiProgressPercent
    , usiIssues
    , usiUpgradeStep

    -- * VPCDerivedInfo
    , VPCDerivedInfo
    , vpcDerivedInfo
    , vdiSecurityGroupIds
    , vdiSubnetIds
    , vdiVPCId
    , vdiAvailabilityZones

    -- * VPCDerivedInfoStatus
    , VPCDerivedInfoStatus
    , vpcDerivedInfoStatus
    , vdisOptions
    , vdisStatus

    -- * VPCOptions
    , VPCOptions
    , vpcOptions
    , voSecurityGroupIds
    , voSubnetIds

    -- * ZoneAwarenessConfig
    , ZoneAwarenessConfig
    , zoneAwarenessConfig
    , zacAvailabilityZoneCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.ElasticSearch.Types.DeploymentStatus
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import Network.AWS.ElasticSearch.Types.DomainPackageStatus
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.OptionState
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStep
import Network.AWS.ElasticSearch.Types.VolumeType
import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainInfo
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.Limits
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.PackageDetails
import Network.AWS.ElasticSearch.Types.PackageSource
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.ElasticSearch.Types.Tag
import Network.AWS.ElasticSearch.Types.UpgradeHistory
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.ElasticSearch.Types.VPCOptions
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
elasticSearch :: Service
elasticSearch
  = Service{_svcAbbrev = "ElasticSearch",
            _svcSigner = v4, _svcPrefix = "es",
            _svcVersion = "2015-01-01",
            _svcEndpoint = defaultEndpoint elasticSearch,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "ElasticSearch",
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

-- | An error occured because the client wanted to access a not supported operation. Gives http status code of 409.
--
--
_DisabledOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_DisabledOperationException
  = _MatchServiceError elasticSearch
      "DisabledOperationException"
      . hasStatus 409

-- | An error occurred because user does not have permissions to access the resource. Returns HTTP status code 403.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError elasticSearch
      "AccessDeniedException"
      . hasStatus 403

-- | An exception for missing / invalid input fields. Gives http status code of 400.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException
  = _MatchServiceError elasticSearch
      "ValidationException"
      . hasStatus 400

-- | An exception for accessing or deleting a resource that does not exist. Gives http status code of 400.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError elasticSearch
      "ResourceNotFoundException"
      . hasStatus 409

-- | An exception for creating a resource that already exists. Gives http status code of 400.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException
  = _MatchServiceError elasticSearch
      "ResourceAlreadyExistsException"
      . hasStatus 409

-- | The request processing has failed because of an unknown error, exception or failure (the failure is internal to the service) . Gives http status code of 500.
--
--
_InternalException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalException
  = _MatchServiceError elasticSearch
      "InternalException"
      . hasStatus 500

-- | An error occurred while processing the request.
--
--
_BaseException :: AsError a => Getting (First ServiceError) a ServiceError
_BaseException
  = _MatchServiceError elasticSearch "BaseException"

-- | An exception for trying to create more than allowed resources or sub-resources. Gives http status code of 409.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError elasticSearch
      "LimitExceededException"
      . hasStatus 409

-- | An error occurred because the client attempts to remove a resource that is currently in use. Returns HTTP status code 409.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException
  = _MatchServiceError elasticSearch
      "ConflictException"
      . hasStatus 409

-- | An exception for trying to create or access sub-resource that is either invalid or not supported. Gives http status code of 409.
--
--
_InvalidTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException
  = _MatchServiceError elasticSearch
      "InvalidTypeException"
      . hasStatus 409
