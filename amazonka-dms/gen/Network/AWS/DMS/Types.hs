{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types
    (
    -- * Service Configuration
      dms

    -- * Errors
    , _ReplicationSubnetGroupDoesNotCoverEnoughAZs
    , _ResourceQuotaExceededFault
    , _InvalidSubnet
    , _KMSThrottlingFault
    , _SNSInvalidTopicFault
    , _KMSNotFoundFault
    , _AccessDeniedFault
    , _ResourceAlreadyExistsFault
    , _StorageQuotaExceededFault
    , _KMSKeyNotAccessibleFault
    , _ResourceNotFoundFault
    , _UpgradeDependencyFailureFault
    , _KMSDisabledFault
    , _KMSAccessDeniedFault
    , _InsufficientResourceCapacityFault
    , _KMSInvalidStateFault
    , _SubnetAlreadyInUse
    , _SNSNoAuthorizationFault
    , _InvalidCertificateFault
    , _InvalidResourceStateFault

    -- * AuthMechanismValue
    , AuthMechanismValue (..)

    -- * AuthTypeValue
    , AuthTypeValue (..)

    -- * CompressionTypeValue
    , CompressionTypeValue (..)

    -- * DataFormatValue
    , DataFormatValue (..)

    -- * DmsSSLModeValue
    , DmsSSLModeValue (..)

    -- * EncodingTypeValue
    , EncodingTypeValue (..)

    -- * EncryptionModeValue
    , EncryptionModeValue (..)

    -- * MessageFormatValue
    , MessageFormatValue (..)

    -- * MigrationTypeValue
    , MigrationTypeValue (..)

    -- * NestingLevelValue
    , NestingLevelValue (..)

    -- * ParquetVersionValue
    , ParquetVersionValue (..)

    -- * RefreshSchemasStatusTypeValue
    , RefreshSchemasStatusTypeValue (..)

    -- * ReleaseStatusValues
    , ReleaseStatusValues (..)

    -- * ReloadOptionValue
    , ReloadOptionValue (..)

    -- * ReplicationEndpointTypeValue
    , ReplicationEndpointTypeValue (..)

    -- * SourceType
    , SourceType (..)

    -- * StartReplicationTaskTypeValue
    , StartReplicationTaskTypeValue (..)

    -- * AccountQuota
    , AccountQuota
    , accountQuota
    , aqMax
    , aqUsed
    , aqAccountQuotaName

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * Certificate
    , Certificate
    , certificate
    , cCertificateOwner
    , cSigningAlgorithm
    , cValidFromDate
    , cCertificatePem
    , cCertificateARN
    , cCertificateCreationDate
    , cCertificateIdentifier
    , cCertificateWallet
    , cKeyLength
    , cValidToDate

    -- * Connection
    , Connection
    , connection
    , cStatus
    , cReplicationInstanceARN
    , cEndpointIdentifier
    , cReplicationInstanceIdentifier
    , cEndpointARN
    , cLastFailureMessage

    -- * DmsTransferSettings
    , DmsTransferSettings
    , dmsTransferSettings
    , dtsServiceAccessRoleARN
    , dtsBucketName

    -- * DynamoDBSettings
    , DynamoDBSettings
    , dynamoDBSettings
    , ddsServiceAccessRoleARN

    -- * ElasticsearchSettings
    , ElasticsearchSettings
    , elasticsearchSettings
    , esFullLoadErrorPercentage
    , esErrorRetryDuration
    , esServiceAccessRoleARN
    , esEndpointURI

    -- * Endpoint
    , Endpoint
    , endpoint
    , eStatus
    , eDmsTransferSettings
    , eServerName
    , eCertificateARN
    , eServiceAccessRoleARN
    , eEngineDisplayName
    , eExtraConnectionAttributes
    , eKafkaSettings
    , eEndpointType
    , eRedshiftSettings
    , eElasticsearchSettings
    , eUsername
    , eExternalTableDefinition
    , eEngineName
    , eNeptuneSettings
    , eKMSKeyId
    , eMongoDBSettings
    , eSSLMode
    , eDatabaseName
    , eS3Settings
    , eKinesisSettings
    , eEndpointIdentifier
    , eExternalId
    , eDynamoDBSettings
    , eEndpointARN
    , ePort

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- * EventCategoryGroup
    , EventCategoryGroup
    , eventCategoryGroup
    , ecgSourceType
    , ecgEventCategories

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEnabled
    , esSourceType
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * KafkaSettings
    , KafkaSettings
    , kafkaSettings
    , ksTopic
    , ksBroker

    -- * KinesisSettings
    , KinesisSettings
    , kinesisSettings
    , ksIncludeTransactionDetails
    , ksIncludeTableAlterOperations
    , ksServiceAccessRoleARN
    , ksPartitionIncludeSchemaTable
    , ksStreamARN
    , ksIncludeControlDetails
    , ksIncludePartitionValue
    , ksMessageFormat

    -- * MongoDBSettings
    , MongoDBSettings
    , mongoDBSettings
    , mdsServerName
    , mdsAuthMechanism
    , mdsUsername
    , mdsKMSKeyId
    , mdsPassword
    , mdsNestingLevel
    , mdsDatabaseName
    , mdsDocsToInvestigate
    , mdsAuthSource
    , mdsExtractDocId
    , mdsAuthType
    , mdsPort

    -- * NeptuneSettings
    , NeptuneSettings
    , neptuneSettings
    , nsMaxFileSize
    , nsMaxRetryCount
    , nsServiceAccessRoleARN
    , nsIAMAuthEnabled
    , nsErrorRetryDuration
    , nsS3BucketName
    , nsS3BucketFolder

    -- * OrderableReplicationInstance
    , OrderableReplicationInstance
    , orderableReplicationInstance
    , oriEngineVersion
    , oriMinAllocatedStorage
    , oriReleaseStatus
    , oriIncludedAllocatedStorage
    , oriAvailabilityZones
    , oriMaxAllocatedStorage
    , oriReplicationInstanceClass
    , oriDefaultAllocatedStorage
    , oriStorageType

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- * RedshiftSettings
    , RedshiftSettings
    , redshiftSettings
    , rsEmptyAsNull
    , rsMaxFileSize
    , rsReplaceChars
    , rsServerName
    , rsConnectionTimeout
    , rsLoadTimeout
    , rsServiceAccessRoleARN
    , rsBucketFolder
    , rsTruncateColumns
    , rsReplaceInvalidChars
    , rsUsername
    , rsBucketName
    , rsEncryptionMode
    , rsDateFormat
    , rsRemoveQuotes
    , rsPassword
    , rsDatabaseName
    , rsAcceptAnyDate
    , rsAfterConnectScript
    , rsWriteBufferSize
    , rsTrimBlanks
    , rsTimeFormat
    , rsServerSideEncryptionKMSKeyId
    , rsPort
    , rsFileTransferUploadStreams

    -- * RefreshSchemasStatus
    , RefreshSchemasStatus
    , refreshSchemasStatus
    , rssStatus
    , rssLastRefreshDate
    , rssReplicationInstanceARN
    , rssEndpointARN
    , rssLastFailureMessage

    -- * ReplicationInstance
    , ReplicationInstance
    , replicationInstance
    , riEngineVersion
    , riPubliclyAccessible
    , riAutoMinorVersionUpgrade
    , riReplicationInstancePublicIPAddresses
    , riReplicationSubnetGroup
    , riInstanceCreateTime
    , riFreeUntil
    , riReplicationInstanceStatus
    , riReplicationInstancePrivateIPAddresses
    , riPreferredMaintenanceWindow
    , riReplicationInstancePrivateIPAddress
    , riKMSKeyId
    , riAvailabilityZone
    , riVPCSecurityGroups
    , riMultiAZ
    , riSecondaryAvailabilityZone
    , riReplicationInstanceARN
    , riAllocatedStorage
    , riDNSNameServers
    , riReplicationInstancePublicIPAddress
    , riReplicationInstanceClass
    , riReplicationInstanceIdentifier
    , riPendingModifiedValues

    -- * ReplicationInstanceTaskLog
    , ReplicationInstanceTaskLog
    , replicationInstanceTaskLog
    , ritlReplicationTaskName
    , ritlReplicationTaskARN
    , ritlReplicationInstanceTaskLogSize

    -- * ReplicationPendingModifiedValues
    , ReplicationPendingModifiedValues
    , replicationPendingModifiedValues
    , rpmvEngineVersion
    , rpmvMultiAZ
    , rpmvAllocatedStorage
    , rpmvReplicationInstanceClass

    -- * ReplicationSubnetGroup
    , ReplicationSubnetGroup
    , replicationSubnetGroup
    , rsgVPCId
    , rsgSubnets
    , rsgReplicationSubnetGroupIdentifier
    , rsgSubnetGroupStatus
    , rsgReplicationSubnetGroupDescription

    -- * ReplicationTask
    , ReplicationTask
    , replicationTask
    , rReplicationTaskSettings
    , rStatus
    , rStopReason
    , rTargetEndpointARN
    , rReplicationTaskIdentifier
    , rCdcStartPosition
    , rReplicationTaskStartDate
    , rSourceEndpointARN
    , rRecoveryCheckpoint
    , rTableMappings
    , rReplicationTaskCreationDate
    , rMigrationType
    , rReplicationTaskARN
    , rTaskData
    , rCdcStopPosition
    , rReplicationTaskStats
    , rReplicationInstanceARN
    , rLastFailureMessage

    -- * ReplicationTaskAssessmentResult
    , ReplicationTaskAssessmentResult
    , replicationTaskAssessmentResult
    , rtarAssessmentResults
    , rtarAssessmentResultsFile
    , rtarReplicationTaskIdentifier
    , rtarAssessmentStatus
    , rtarS3ObjectURL
    , rtarReplicationTaskLastAssessmentDate
    , rtarReplicationTaskARN

    -- * ReplicationTaskStats
    , ReplicationTaskStats
    , replicationTaskStats
    , rtsStopDate
    , rtsFullLoadProgressPercent
    , rtsFullLoadStartDate
    , rtsElapsedTimeMillis
    , rtsStartDate
    , rtsTablesErrored
    , rtsFullLoadFinishDate
    , rtsTablesLoaded
    , rtsTablesQueued
    , rtsTablesLoading
    , rtsFreshStartDate

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * S3Settings
    , S3Settings
    , s3Settings
    , ssParquetVersion
    , ssParquetTimestampInMillisecond
    , ssIncludeOpForFullLoad
    , ssCSVDelimiter
    , ssServiceAccessRoleARN
    , ssBucketFolder
    , ssDataFormat
    , ssEncodingType
    , ssExternalTableDefinition
    , ssDictPageSizeLimit
    , ssBucketName
    , ssEncryptionMode
    , ssEnableStatistics
    , ssCdcInsertsOnly
    , ssTimestampColumnName
    , ssCSVRowDelimiter
    , ssCompressionType
    , ssServerSideEncryptionKMSKeyId
    , ssDataPageSize
    , ssCdcInsertsAndUpdates
    , ssRowGroupLength

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SupportedEndpointType
    , SupportedEndpointType
    , supportedEndpointType
    , setEngineDisplayName
    , setEndpointType
    , setEngineName
    , setReplicationInstanceEngineMinimumVersion
    , setSupportsCDC

    -- * TableStatistics
    , TableStatistics
    , tableStatistics
    , tsValidationState
    , tsFullLoadRows
    , tsInserts
    , tsFullLoadEndTime
    , tsFullLoadCondtnlChkFailedRows
    , tsFullLoadReloaded
    , tsValidationFailedRecords
    , tsValidationSuspendedRecords
    , tsSchemaName
    , tsValidationStateDetails
    , tsTableState
    , tsFullLoadErrorRows
    , tsDdls
    , tsDeletes
    , tsUpdates
    , tsValidationPendingRecords
    , tsFullLoadStartTime
    , tsLastUpdateTime
    , tsTableName

    -- * TableToReload
    , TableToReload
    , tableToReload
    , ttrSchemaName
    , ttrTableName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.DMS.Types.AuthMechanismValue
import Network.AWS.DMS.Types.AuthTypeValue
import Network.AWS.DMS.Types.CompressionTypeValue
import Network.AWS.DMS.Types.DataFormatValue
import Network.AWS.DMS.Types.DmsSSLModeValue
import Network.AWS.DMS.Types.EncodingTypeValue
import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.DMS.Types.MessageFormatValue
import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.NestingLevelValue
import Network.AWS.DMS.Types.ParquetVersionValue
import Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue
import Network.AWS.DMS.Types.ReleaseStatusValues
import Network.AWS.DMS.Types.ReloadOptionValue
import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.DMS.Types.SourceType
import Network.AWS.DMS.Types.StartReplicationTaskTypeValue
import Network.AWS.DMS.Types.AccountQuota
import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.DMS.Types.Certificate
import Network.AWS.DMS.Types.Connection
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DynamoDBSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.Endpoint
import Network.AWS.DMS.Types.Event
import Network.AWS.DMS.Types.EventCategoryGroup
import Network.AWS.DMS.Types.EventSubscription
import Network.AWS.DMS.Types.Filter
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MongoDBSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.OrderableReplicationInstance
import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.RefreshSchemasStatus
import Network.AWS.DMS.Types.ReplicationInstance
import Network.AWS.DMS.Types.ReplicationInstanceTaskLog
import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.ReplicationTask
import Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.Subnet
import Network.AWS.DMS.Types.SupportedEndpointType
import Network.AWS.DMS.Types.TableStatistics
import Network.AWS.DMS.Types.TableToReload
import Network.AWS.DMS.Types.Tag
import Network.AWS.DMS.Types.VPCSecurityGroupMembership

-- | API version @2016-01-01@ of the Amazon Database Migration Service SDK configuration.
dms :: Service
dms
  = Service{_svcAbbrev = "DMS", _svcSigner = v4,
            _svcPrefix = "dms", _svcVersion = "2016-01-01",
            _svcEndpoint = defaultEndpoint dms,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "DMS", _svcRetry = retry}
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

-- | The replication subnet group does not cover enough Availability Zones (AZs). Edit the replication subnet group and add more AZs.
--
--
_ReplicationSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationSubnetGroupDoesNotCoverEnoughAZs
  = _MatchServiceError dms
      "ReplicationSubnetGroupDoesNotCoverEnoughAZs"

-- | The quota for this resource quota has been exceeded.
--
--
_ResourceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceQuotaExceededFault
  = _MatchServiceError dms "ResourceQuotaExceededFault"

-- | The subnet provided is invalid.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet
  = _MatchServiceError dms "InvalidSubnet"

-- | This request triggered AWS KMS request throttling.
--
--
_KMSThrottlingFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSThrottlingFault
  = _MatchServiceError dms "KMSThrottlingFault"

-- | The SNS topic is invalid.
--
--
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault
  = _MatchServiceError dms "SNSInvalidTopicFault"

-- | The specified AWS KMS entity or resource can't be found.
--
--
_KMSNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSNotFoundFault
  = _MatchServiceError dms "KMSNotFoundFault"

-- | AWS DMS was denied access to the endpoint. Check that the role is correctly configured.
--
--
_AccessDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedFault
  = _MatchServiceError dms "AccessDeniedFault"

-- | The resource you are attempting to create already exists.
--
--
_ResourceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsFault
  = _MatchServiceError dms "ResourceAlreadyExistsFault"

-- | The storage quota has been exceeded.
--
--
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault
  = _MatchServiceError dms "StorageQuotaExceededFault"

-- | AWS DMS cannot access the AWS KMS key.
--
--
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault
  = _MatchServiceError dms "KMSKeyNotAccessibleFault"

-- | The resource could not be found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault
  = _MatchServiceError dms "ResourceNotFoundFault"

-- | An upgrade dependency is preventing the database migration.
--
--
_UpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_UpgradeDependencyFailureFault
  = _MatchServiceError dms
      "UpgradeDependencyFailureFault"

-- | The specified master key (CMK) isn't enabled.
--
--
_KMSDisabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSDisabledFault
  = _MatchServiceError dms "KMSDisabledFault"

-- | The ciphertext references a key that doesn't exist or that the DMS account doesn't have access to.
--
--
_KMSAccessDeniedFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSAccessDeniedFault
  = _MatchServiceError dms "KMSAccessDeniedFault"

-- | There are not enough resources allocated to the database migration.
--
--
_InsufficientResourceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientResourceCapacityFault
  = _MatchServiceError dms
      "InsufficientResourceCapacityFault"

-- | The state of the specified AWS KMS resource isn't valid for this request.
--
--
_KMSInvalidStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateFault
  = _MatchServiceError dms "KMSInvalidStateFault"

-- | The specified subnet is already in use.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse
  = _MatchServiceError dms "SubnetAlreadyInUse"

-- | You are not authorized for the SNS subscription.
--
--
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault
  = _MatchServiceError dms "SNSNoAuthorizationFault"

-- | The certificate was not valid.
--
--
_InvalidCertificateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCertificateFault
  = _MatchServiceError dms "InvalidCertificateFault"

-- | The resource is in a state that prevents it from being used for database migration.
--
--
_InvalidResourceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateFault
  = _MatchServiceError dms "InvalidResourceStateFault"
