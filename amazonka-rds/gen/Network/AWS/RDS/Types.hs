{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types
    (
    -- * Service Configuration
      rds

    -- * Errors
    , _DBSubnetGroupNotFoundFault
    , _OptionGroupQuotaExceededFault
    , _DBInstanceAlreadyExistsFault
    , _InvalidDBClusterCapacityFault
    , _InvalidDBSecurityGroupStateFault
    , _DBSnapshotNotFoundFault
    , _DBClusterQuotaExceededFault
    , _DBSecurityGroupAlreadyExistsFault
    , _CustomAvailabilityZoneQuotaExceededFault
    , _InvalidOptionGroupStateFault
    , _InvalidDBClusterStateFault
    , _OptionGroupAlreadyExistsFault
    , _ExportTaskNotFoundFault
    , _GlobalClusterNotFoundFault
    , _DBSecurityGroupQuotaExceededFault
    , _CustomAvailabilityZoneAlreadyExistsFault
    , _DBClusterAlreadyExistsFault
    , _GlobalClusterAlreadyExistsFault
    , _DBProxyQuotaExceededFault
    , _InvalidGlobalClusterStateFault
    , _DBProxyTargetGroupNotFoundFault
    , _DBSubnetGroupNotAllowedFault
    , _ExportTaskAlreadyExistsFault
    , _InvalidSubnet
    , _SubscriptionCategoryNotFoundFault
    , _AuthorizationNotFoundFault
    , _SubscriptionNotFoundFault
    , _DBProxyTargetAlreadyRegisteredFault
    , _IAMRoleMissingPermissionsFault
    , _BackupPolicyNotFoundFault
    , _AuthorizationAlreadyExistsFault
    , _AuthorizationQuotaExceededFault
    , _InvalidDBClusterSnapshotStateFault
    , _SNSInvalidTopicFault
    , _PointInTimeRestoreNotEnabledFault
    , _InvalidDBParameterGroupStateFault
    , _DBClusterSnapshotAlreadyExistsFault
    , _SourceNotFoundFault
    , _ReservedDBInstancesOfferingNotFoundFault
    , _DBClusterSnapshotNotFoundFault
    , _InstanceQuotaExceededFault
    , _DBProxyAlreadyExistsFault
    , _DBInstanceNotFoundFault
    , _InvalidDBSnapshotStateFault
    , _DBSecurityGroupNotSupportedFault
    , _InvalidDBProxyStateFault
    , _DBSecurityGroupNotFoundFault
    , _GlobalClusterQuotaExceededFault
    , _StorageQuotaExceededFault
    , _DBProxyNotFoundFault
    , _SNSTopicARNNotFoundFault
    , _KMSKeyNotAccessibleFault
    , _DBClusterEndpointAlreadyExistsFault
    , _InvalidExportOnlyFault
    , _SnapshotQuotaExceededFault
    , _InvalidDBInstanceStateFault
    , _InvalidDBInstanceAutomatedBackupStateFault
    , _DBUpgradeDependencyFailureFault
    , _DBClusterParameterGroupNotFoundFault
    , _InvalidRestoreFault
    , _InvalidEventSubscriptionStateFault
    , _ResourceNotFoundFault
    , _InvalidDBClusterEndpointStateFault
    , _InsufficientStorageClusterCapacityFault
    , _StorageTypeNotSupportedFault
    , _EventSubscriptionQuotaExceededFault
    , _DBInstanceAutomatedBackupQuotaExceededFault
    , _IAMRoleNotFoundFault
    , _DBClusterEndpointQuotaExceededFault
    , _DBSubnetGroupQuotaExceededFault
    , _InvalidExportTaskStateFault
    , _DBClusterNotFoundFault
    , _DBLogFileNotFoundFault
    , _InvalidS3BucketFault
    , _CustomAvailabilityZoneNotFoundFault
    , _InvalidExportSourceStateFault
    , _OptionGroupNotFoundFault
    , _DBProxyTargetNotFoundFault
    , _SharedSnapshotQuotaExceededFault
    , _ReservedDBInstanceNotFoundFault
    , _DBSubnetQuotaExceededFault
    , _DBInstanceRoleNotFoundFault
    , _ProvisionedIOPSNotAvailableInAZFault
    , _ReservedDBInstanceAlreadyExistsFault
    , _DBInstanceRoleAlreadyExistsFault
    , _DBClusterRoleQuotaExceededFault
    , _InsufficientDBClusterCapacityFault
    , _InvalidVPCNetworkStateFault
    , _DBParameterGroupQuotaExceededFault
    , _InsufficientDBInstanceCapacityFault
    , _DBParameterGroupAlreadyExistsFault
    , _ReservedDBInstanceQuotaExceededFault
    , _DBInstanceRoleQuotaExceededFault
    , _CertificateNotFoundFault
    , _DBClusterRoleAlreadyExistsFault
    , _SubscriptionAlreadyExistFault
    , _InstallationMediaAlreadyExistsFault
    , _InvalidDBSubnetGroupFault
    , _InvalidDBSubnetStateFault
    , _DBClusterBacktrackNotFoundFault
    , _DBClusterRoleNotFoundFault
    , _DomainNotFoundFault
    , _InstallationMediaNotFoundFault
    , _SubnetAlreadyInUse
    , _DBParameterGroupNotFoundFault
    , _DBSubnetGroupDoesNotCoverEnoughAZs
    , _SNSNoAuthorizationFault
    , _DBInstanceAutomatedBackupNotFoundFault
    , _DBSubnetGroupAlreadyExistsFault
    , _DBClusterEndpointNotFoundFault
    , _DBSnapshotAlreadyExistsFault
    , _InvalidDBSubnetGroupStateFault

    -- * ActivityStreamMode
    , ActivityStreamMode (..)

    -- * ActivityStreamStatus
    , ActivityStreamStatus (..)

    -- * ApplyMethod
    , ApplyMethod (..)

    -- * AuthScheme
    , AuthScheme (..)

    -- * DBProxyStatus
    , DBProxyStatus (..)

    -- * EngineFamily
    , EngineFamily (..)

    -- * IAMAuthMode
    , IAMAuthMode (..)

    -- * SourceType
    , SourceType (..)

    -- * TargetHealthReason
    , TargetHealthReason (..)

    -- * TargetState
    , TargetState (..)

    -- * TargetType
    , TargetType (..)

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

    -- * AvailableProcessorFeature
    , AvailableProcessorFeature
    , availableProcessorFeature
    , apfName
    , apfDefaultValue
    , apfAllowedValues

    -- * Certificate
    , Certificate
    , certificate
    , cCertificateType
    , cCustomerOverride
    , cCertificateARN
    , cCustomerOverrideValidTill
    , cValidTill
    , cCertificateIdentifier
    , cThumbprint
    , cValidFrom

    -- * CharacterSet
    , CharacterSet
    , characterSet
    , csCharacterSetName
    , csCharacterSetDescription

    -- * CloudwatchLogsExportConfiguration
    , CloudwatchLogsExportConfiguration
    , cloudwatchLogsExportConfiguration
    , clecDisableLogTypes
    , clecEnableLogTypes

    -- * ConnectionPoolConfiguration
    , ConnectionPoolConfiguration
    , connectionPoolConfiguration
    , cpcMaxIdleConnectionsPercent
    , cpcSessionPinningFilters
    , cpcMaxConnectionsPercent
    , cpcConnectionBorrowTimeout
    , cpcInitQuery

    -- * ConnectionPoolConfigurationInfo
    , ConnectionPoolConfigurationInfo
    , connectionPoolConfigurationInfo
    , cpciMaxIdleConnectionsPercent
    , cpciSessionPinningFilters
    , cpciMaxConnectionsPercent
    , cpciConnectionBorrowTimeout
    , cpciInitQuery

    -- * CustomAvailabilityZone
    , CustomAvailabilityZone
    , customAvailabilityZone
    , cazVPNDetails
    , cazCustomAvailabilityZoneName
    , cazCustomAvailabilityZoneId
    , cazCustomAvailabilityZoneStatus

    -- * DBCluster
    , DBCluster
    , dbCluster
    , dcBacktrackConsumedChangeRecords
    , dcEngineVersion
    , dcStatus
    , dcDeletionProtection
    , dcStorageEncrypted
    , dcDBClusterIdentifier
    , dcDBClusterMembers
    , dcReadReplicaIdentifiers
    , dcReplicationSourceIdentifier
    , dcActivityStreamKinesisStreamName
    , dcHostedZoneId
    , dcDBClusterParameterGroup
    , dcMasterUsername
    , dcIAMDatabaseAuthenticationEnabled
    , dcEarliestBacktrackTime
    , dcBacktrackWindow
    , dcDBClusterResourceId
    , dcEarliestRestorableTime
    , dcCustomEndpoints
    , dcEngine
    , dcHTTPEndpointEnabled
    , dcDBClusterARN
    , dcCloneGroupId
    , dcLatestRestorableTime
    , dcCrossAccountClone
    , dcCapacity
    , dcPreferredMaintenanceWindow
    , dcAvailabilityZones
    , dcCharacterSetName
    , dcKMSKeyId
    , dcPreferredBackupWindow
    , dcAssociatedRoles
    , dcVPCSecurityGroups
    , dcBackupRetentionPeriod
    , dcDBSubnetGroup
    , dcActivityStreamMode
    , dcDatabaseName
    , dcMultiAZ
    , dcEngineMode
    , dcEnabledCloudwatchLogsExports
    , dcActivityStreamStatus
    , dcAllocatedStorage
    , dcCopyTagsToSnapshot
    , dcClusterCreateTime
    , dcEndpoint
    , dcScalingConfigurationInfo
    , dcActivityStreamKMSKeyId
    , dcPercentProgress
    , dcReaderEndpoint
    , dcPort
    , dcDomainMemberships
    , dcDBClusterOptionGroupMemberships

    -- * DBClusterBacktrack
    , DBClusterBacktrack
    , dbClusterBacktrack
    , dcbStatus
    , dcbBacktrackIdentifier
    , dcbBacktrackTo
    , dcbDBClusterIdentifier
    , dcbBacktrackedFrom
    , dcbBacktrackRequestCreationTime

    -- * DBClusterEndpoint
    , DBClusterEndpoint
    , dbClusterEndpoint
    , dceStatus
    , dceDBClusterIdentifier
    , dceDBClusterEndpointARN
    , dceCustomEndpointType
    , dceStaticMembers
    , dceEndpointType
    , dceDBClusterEndpointIdentifier
    , dceEndpoint
    , dceDBClusterEndpointResourceIdentifier
    , dceExcludedMembers

    -- * DBClusterMember
    , DBClusterMember
    , dbClusterMember
    , dcmPromotionTier
    , dcmDBInstanceIdentifier
    , dcmIsClusterWriter
    , dcmDBClusterParameterGroupStatus

    -- * DBClusterOptionGroupStatus
    , DBClusterOptionGroupStatus
    , dbClusterOptionGroupStatus
    , dcogsStatus
    , dcogsDBClusterOptionGroupName

    -- * DBClusterParameterGroup
    , DBClusterParameterGroup
    , dbClusterParameterGroup
    , dcpgDBClusterParameterGroupARN
    , dcpgDBParameterGroupFamily
    , dcpgDBClusterParameterGroupName
    , dcpgDescription

    -- * DBClusterParameterGroupNameMessage
    , DBClusterParameterGroupNameMessage
    , dbClusterParameterGroupNameMessage
    , dcpgnmDBClusterParameterGroupName

    -- * DBClusterRole
    , DBClusterRole
    , dbClusterRole
    , dcrStatus
    , dcrFeatureName
    , dcrRoleARN

    -- * DBClusterSnapshot
    , DBClusterSnapshot
    , dbClusterSnapshot
    , dcsEngineVersion
    , dcsStatus
    , dcsStorageEncrypted
    , dcsDBClusterIdentifier
    , dcsMasterUsername
    , dcsIAMDatabaseAuthenticationEnabled
    , dcsDBClusterSnapshotARN
    , dcsVPCId
    , dcsDBClusterSnapshotIdentifier
    , dcsEngine
    , dcsLicenseModel
    , dcsAvailabilityZones
    , dcsSnapshotType
    , dcsKMSKeyId
    , dcsSnapshotCreateTime
    , dcsAllocatedStorage
    , dcsSourceDBClusterSnapshotARN
    , dcsClusterCreateTime
    , dcsPercentProgress
    , dcsPort

    -- * DBClusterSnapshotAttribute
    , DBClusterSnapshotAttribute
    , dbClusterSnapshotAttribute
    , dcsaAttributeValues
    , dcsaAttributeName

    -- * DBClusterSnapshotAttributesResult
    , DBClusterSnapshotAttributesResult
    , dbClusterSnapshotAttributesResult
    , dcsarDBClusterSnapshotIdentifier
    , dcsarDBClusterSnapshotAttributes

    -- * DBEngineVersion
    , DBEngineVersion
    , dbEngineVersion
    , devEngineVersion
    , devStatus
    , devDBEngineVersionDescription
    , devSupportedEngineModes
    , devDefaultCharacterSet
    , devEngine
    , devDBParameterGroupFamily
    , devSupportedCharacterSets
    , devDBEngineDescription
    , devValidUpgradeTarget
    , devSupportsLogExportsToCloudwatchLogs
    , devSupportsReadReplica
    , devSupportedFeatureNames
    , devSupportedTimezones
    , devExportableLogTypes

    -- * DBInstance
    , DBInstance
    , dbInstance
    , diEngineVersion
    , diDBSecurityGroups
    , diDeletionProtection
    , diStorageEncrypted
    , diDBClusterIdentifier
    , diPubliclyAccessible
    , diAutoMinorVersionUpgrade
    , diDBInstanceARN
    , diMasterUsername
    , diReadReplicaDBInstanceIdentifiers
    , diIAMDatabaseAuthenticationEnabled
    , diMonitoringRoleARN
    , diIOPS
    , diInstanceCreateTime
    , diReadReplicaSourceDBInstanceIdentifier
    , diMonitoringInterval
    , diEngine
    , diProcessorFeatures
    , diLatestRestorableTime
    , diDBInstanceClass
    , diPromotionTier
    , diLicenseModel
    , diPreferredMaintenanceWindow
    , diPerformanceInsightsRetentionPeriod
    , diCACertificateIdentifier
    , diDBInstanceIdentifier
    , diCharacterSetName
    , diMaxAllocatedStorage
    , diKMSKeyId
    , diPreferredBackupWindow
    , diAssociatedRoles
    , diAvailabilityZone
    , diVPCSecurityGroups
    , diBackupRetentionPeriod
    , diPerformanceInsightsKMSKeyId
    , diDBSubnetGroup
    , diMultiAZ
    , diListenerEndpoint
    , diOptionGroupMemberships
    , diEnabledCloudwatchLogsExports
    , diEnhancedMonitoringResourceARN
    , diSecondaryAvailabilityZone
    , diPerformanceInsightsEnabled
    , diAllocatedStorage
    , diDBiResourceId
    , diDBParameterGroups
    , diCopyTagsToSnapshot
    , diTimezone
    , diTDECredentialARN
    , diEndpoint
    , diDBInstanceStatus
    , diDBInstancePort
    , diPendingModifiedValues
    , diReadReplicaDBClusterIdentifiers
    , diStorageType
    , diStatusInfos
    , diDomainMemberships
    , diDBName

    -- * DBInstanceAutomatedBackup
    , DBInstanceAutomatedBackup
    , dbInstanceAutomatedBackup
    , diabRestoreWindow
    , diabEngineVersion
    , diabStatus
    , diabDBInstanceARN
    , diabMasterUsername
    , diabIAMDatabaseAuthenticationEnabled
    , diabIOPS
    , diabVPCId
    , diabInstanceCreateTime
    , diabEngine
    , diabEncrypted
    , diabLicenseModel
    , diabDBInstanceIdentifier
    , diabKMSKeyId
    , diabAvailabilityZone
    , diabRegion
    , diabAllocatedStorage
    , diabDBiResourceId
    , diabOptionGroupName
    , diabTimezone
    , diabTDECredentialARN
    , diabPort
    , diabStorageType

    -- * DBInstanceRole
    , DBInstanceRole
    , dbInstanceRole
    , dirStatus
    , dirFeatureName
    , dirRoleARN

    -- * DBInstanceStatusInfo
    , DBInstanceStatusInfo
    , dbInstanceStatusInfo
    , disiStatus
    , disiNormal
    , disiStatusType
    , disiMessage

    -- * DBParameterGroup
    , DBParameterGroup
    , dbParameterGroup
    , dpgDBParameterGroupARN
    , dpgDBParameterGroupFamily
    , dpgDBParameterGroupName
    , dpgDescription

    -- * DBParameterGroupNameMessage
    , DBParameterGroupNameMessage
    , dbParameterGroupNameMessage
    , dpgnmDBParameterGroupName

    -- * DBParameterGroupStatus
    , DBParameterGroupStatus
    , dbParameterGroupStatus
    , dpgsDBParameterGroupName
    , dpgsParameterApplyStatus

    -- * DBProxy
    , DBProxy
    , dbProxy
    , dpStatus
    , dpDBProxyARN
    , dpDebugLogging
    , dpVPCSubnetIds
    , dpEngineFamily
    , dpAuth
    , dpRequireTLS
    , dpIdleClientTimeout
    , dpUpdatedDate
    , dpCreatedDate
    , dpVPCSecurityGroupIds
    , dpDBProxyName
    , dpEndpoint
    , dpRoleARN

    -- * DBProxyTarget
    , DBProxyTarget
    , dbProxyTarget
    , dptTargetARN
    , dptTargetHealth
    , dptTrackedClusterId
    , dptRDSResourceId
    , dptType
    , dptEndpoint
    , dptPort

    -- * DBProxyTargetGroup
    , DBProxyTargetGroup
    , dbProxyTargetGroup
    , dptgStatus
    , dptgConnectionPoolConfig
    , dptgTargetGroupARN
    , dptgUpdatedDate
    , dptgCreatedDate
    , dptgDBProxyName
    , dptgTargetGroupName
    , dptgIsDefault

    -- * DBSecurityGroup
    , DBSecurityGroup
    , dbSecurityGroup
    , dbsgVPCId
    , dbsgOwnerId
    , dbsgDBSecurityGroupARN
    , dbsgIPRanges
    , dbsgDBSecurityGroupName
    , dbsgEC2SecurityGroups
    , dbsgDBSecurityGroupDescription

    -- * DBSecurityGroupMembership
    , DBSecurityGroupMembership
    , dbSecurityGroupMembership
    , dsgmStatus
    , dsgmDBSecurityGroupName

    -- * DBSnapshot
    , DBSnapshot
    , dbSnapshot
    , dsEngineVersion
    , dsStatus
    , dsDBSnapshotARN
    , dsMasterUsername
    , dsSourceRegion
    , dsIAMDatabaseAuthenticationEnabled
    , dsIOPS
    , dsVPCId
    , dsInstanceCreateTime
    , dsEngine
    , dsEncrypted
    , dsDBSnapshotIdentifier
    , dsProcessorFeatures
    , dsLicenseModel
    , dsSourceDBSnapshotIdentifier
    , dsSnapshotType
    , dsDBInstanceIdentifier
    , dsKMSKeyId
    , dsAvailabilityZone
    , dsSnapshotCreateTime
    , dsAllocatedStorage
    , dsDBiResourceId
    , dsOptionGroupName
    , dsTimezone
    , dsTDECredentialARN
    , dsPercentProgress
    , dsPort
    , dsStorageType

    -- * DBSnapshotAttribute
    , DBSnapshotAttribute
    , dbSnapshotAttribute
    , dsaAttributeValues
    , dsaAttributeName

    -- * DBSnapshotAttributesResult
    , DBSnapshotAttributesResult
    , dbSnapshotAttributesResult
    , dsarDBSnapshotIdentifier
    , dsarDBSnapshotAttributes

    -- * DBSubnetGroup
    , DBSubnetGroup
    , dbSubnetGroup
    , dsgDBSubnetGroupName
    , dsgVPCId
    , dsgSubnets
    , dsgDBSubnetGroupDescription
    , dsgDBSubnetGroupARN
    , dsgSubnetGroupStatus

    -- * DescribeDBLogFilesDetails
    , DescribeDBLogFilesDetails
    , describeDBLogFilesDetails
    , ddlfdLastWritten
    , ddlfdSize
    , ddlfdLogFileName

    -- * DomainMembership
    , DomainMembership
    , domainMembership
    , dmStatus
    , dmFQDN
    , dmDomain
    , dmIAMRoleName

    -- * DoubleRange
    , DoubleRange
    , doubleRange
    , drTo
    , drFrom

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName
    , esgEC2SecurityGroupId

    -- * Endpoint
    , Endpoint
    , endpoint
    , eHostedZoneId
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edDBParameterGroupFamily
    , edMarker
    , edParameters

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceARN
    , eSourceIdentifier
    , eDate
    , eEventCategories
    , eMessage

    -- * EventCategoriesMap
    , EventCategoriesMap
    , eventCategoriesMap
    , ecmSourceType
    , ecmEventCategories

    -- * EventSubscription
    , EventSubscription
    , eventSubscription
    , esStatus
    , esCustomerAWSId
    , esCustSubscriptionId
    , esSNSTopicARN
    , esEventSubscriptionARN
    , esEnabled
    , esSourceType
    , esSubscriptionCreationTime
    , esEventCategoriesList
    , esSourceIdsList

    -- * ExportTask
    , ExportTask
    , exportTask
    , etTotalExtractedDataInGB
    , etStatus
    , etIAMRoleARN
    , etSourceARN
    , etExportOnly
    , etTaskStartTime
    , etWarningMessage
    , etSnapshotTime
    , etKMSKeyId
    , etTaskEndTime
    , etExportTaskIdentifier
    , etS3Prefix
    , etPercentProgress
    , etS3Bucket
    , etFailureCause

    -- * Filter
    , Filter
    , filter'
    , fName
    , fValues

    -- * GlobalCluster
    , GlobalCluster
    , globalCluster
    , gcEngineVersion
    , gcStatus
    , gcDeletionProtection
    , gcStorageEncrypted
    , gcGlobalClusterIdentifier
    , gcEngine
    , gcGlobalClusterARN
    , gcDatabaseName
    , gcGlobalClusterMembers
    , gcGlobalClusterResourceId

    -- * GlobalClusterMember
    , GlobalClusterMember
    , globalClusterMember
    , gcmReaders
    , gcmDBClusterARN
    , gcmIsWriter

    -- * IPRange
    , IPRange
    , ipRange
    , irStatus
    , irCIdRIP

    -- * InstallationMedia
    , InstallationMedia
    , installationMedia
    , imEngineVersion
    , imStatus
    , imInstallationMediaId
    , imEngineInstallationMediaPath
    , imEngine
    , imOSInstallationMediaPath
    , imCustomAvailabilityZoneId
    , imFailureCause

    -- * InstallationMediaFailureCause
    , InstallationMediaFailureCause
    , installationMediaFailureCause
    , imfcMessage

    -- * MinimumEngineVersionPerAllowedValue
    , MinimumEngineVersionPerAllowedValue
    , minimumEngineVersionPerAllowedValue
    , mevpavMinimumEngineVersion
    , mevpavAllowedValue

    -- * Option
    , Option
    , option
    , oOptionName
    , oPermanent
    , oPersistent
    , oOptionDescription
    , oOptionSettings
    , oVPCSecurityGroupMemberships
    , oDBSecurityGroupMemberships
    , oOptionVersion
    , oPort

    -- * OptionConfiguration
    , OptionConfiguration
    , optionConfiguration
    , ocOptionSettings
    , ocVPCSecurityGroupMemberships
    , ocDBSecurityGroupMemberships
    , ocOptionVersion
    , ocPort
    , ocOptionName

    -- * OptionGroup
    , OptionGroup
    , optionGroup
    , ogOptionGroupDescription
    , ogVPCId
    , ogAllowsVPCAndNonVPCInstanceMemberships
    , ogEngineName
    , ogOptionGroupARN
    , ogMajorEngineVersion
    , ogOptions
    , ogOptionGroupName

    -- * OptionGroupMembership
    , OptionGroupMembership
    , optionGroupMembership
    , ogmStatus
    , ogmOptionGroupName

    -- * OptionGroupOption
    , OptionGroupOption
    , optionGroupOption
    , ogoMinimumRequiredMinorEngineVersion
    , ogoOptionsConflictsWith
    , ogoPermanent
    , ogoPersistent
    , ogoOptionGroupOptionVersions
    , ogoEngineName
    , ogoMajorEngineVersion
    , ogoName
    , ogoSupportsOptionVersionDowngrade
    , ogoDefaultPort
    , ogoOptionGroupOptionSettings
    , ogoRequiresAutoMinorEngineVersionUpgrade
    , ogoPortRequired
    , ogoDescription
    , ogoOptionsDependedOn
    , ogoVPCOnly

    -- * OptionGroupOptionSetting
    , OptionGroupOptionSetting
    , optionGroupOptionSetting
    , ogosApplyType
    , ogosMinimumEngineVersionPerAllowedValue
    , ogosSettingName
    , ogosDefaultValue
    , ogosIsModifiable
    , ogosSettingDescription
    , ogosAllowedValues
    , ogosIsRequired

    -- * OptionSetting
    , OptionSetting
    , optionSetting
    , osIsCollection
    , osApplyType
    , osValue
    , osName
    , osDefaultValue
    , osIsModifiable
    , osDataType
    , osAllowedValues
    , osDescription

    -- * OptionVersion
    , OptionVersion
    , optionVersion
    , ovVersion
    , ovIsDefault

    -- * OrderableDBInstanceOption
    , OrderableDBInstanceOption
    , orderableDBInstanceOption
    , odioEngineVersion
    , odioMinIOPSPerGib
    , odioSupportsIAMDatabaseAuthentication
    , odioMinIOPSPerDBInstance
    , odioMultiAZCapable
    , odioMaxStorageSize
    , odioSupportedEngineModes
    , odioAvailabilityZoneGroup
    , odioAvailableProcessorFeatures
    , odioEngine
    , odioMinStorageSize
    , odioSupportsIOPS
    , odioSupportsKerberosAuthentication
    , odioSupportsPerformanceInsights
    , odioDBInstanceClass
    , odioLicenseModel
    , odioAvailabilityZones
    , odioSupportsStorageAutoscaling
    , odioSupportsStorageEncryption
    , odioReadReplicaCapable
    , odioMaxIOPSPerGib
    , odioVPC
    , odioSupportsEnhancedMonitoring
    , odioMaxIOPSPerDBInstance
    , odioStorageType

    -- * Parameter
    , Parameter
    , parameter
    , pApplyType
    , pParameterValue
    , pSupportedEngineModes
    , pApplyMethod
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription

    -- * PendingCloudwatchLogsExports
    , PendingCloudwatchLogsExports
    , pendingCloudwatchLogsExports
    , pcleLogTypesToEnable
    , pcleLogTypesToDisable

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAutoAppliedAfterDate
    , pmaAction
    , pmaOptInStatus
    , pmaDescription
    , pmaForcedApplyDate
    , pmaCurrentApplyDate

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvMasterUserPassword
    , pmvDBSubnetGroupName
    , pmvIOPS
    , pmvProcessorFeatures
    , pmvDBInstanceClass
    , pmvLicenseModel
    , pmvCACertificateIdentifier
    , pmvDBInstanceIdentifier
    , pmvPendingCloudwatchLogsExports
    , pmvBackupRetentionPeriod
    , pmvMultiAZ
    , pmvAllocatedStorage
    , pmvPort
    , pmvStorageType

    -- * ProcessorFeature
    , ProcessorFeature
    , processorFeature
    , pfValue
    , pfName

    -- * Range
    , Range
    , range
    , rTo
    , rFrom
    , rStep

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReservedDBInstance
    , ReservedDBInstance
    , reservedDBInstance
    , rdiDBInstanceCount
    , rdiState
    , rdiCurrencyCode
    , rdiStartTime
    , rdiProductDescription
    , rdiLeaseId
    , rdiReservedDBInstanceId
    , rdiReservedDBInstanceARN
    , rdiDBInstanceClass
    , rdiMultiAZ
    , rdiReservedDBInstancesOfferingId
    , rdiRecurringCharges
    , rdiOfferingType
    , rdiUsagePrice
    , rdiFixedPrice
    , rdiDuration

    -- * ReservedDBInstancesOffering
    , ReservedDBInstancesOffering
    , reservedDBInstancesOffering
    , rdioCurrencyCode
    , rdioProductDescription
    , rdioDBInstanceClass
    , rdioMultiAZ
    , rdioReservedDBInstancesOfferingId
    , rdioRecurringCharges
    , rdioOfferingType
    , rdioUsagePrice
    , rdioFixedPrice
    , rdioDuration

    -- * ResourcePendingMaintenanceActions
    , ResourcePendingMaintenanceActions
    , resourcePendingMaintenanceActions
    , rpmaPendingMaintenanceActionDetails
    , rpmaResourceIdentifier

    -- * RestoreWindow
    , RestoreWindow
    , restoreWindow
    , rwLatestTime
    , rwEarliestTime

    -- * ScalingConfiguration
    , ScalingConfiguration
    , scalingConfiguration
    , scSecondsUntilAutoPause
    , scTimeoutAction
    , scAutoPause
    , scMaxCapacity
    , scMinCapacity

    -- * ScalingConfigurationInfo
    , ScalingConfigurationInfo
    , scalingConfigurationInfo
    , sciSecondsUntilAutoPause
    , sciTimeoutAction
    , sciAutoPause
    , sciMaxCapacity
    , sciMinCapacity

    -- * SourceRegion
    , SourceRegion
    , sourceRegion
    , srStatus
    , srRegionName
    , srEndpoint

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetStatus
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TargetHealth
    , TargetHealth
    , targetHealth
    , thState
    , thReason
    , thDescription

    -- * Timezone
    , Timezone
    , timezone
    , tTimezoneName

    -- * UpgradeTarget
    , UpgradeTarget
    , upgradeTarget
    , utEngineVersion
    , utIsMajorVersionUpgrade
    , utEngine
    , utAutoUpgrade
    , utDescription

    -- * UserAuthConfig
    , UserAuthConfig
    , userAuthConfig
    , uacIAMAuth
    , uacUserName
    , uacAuthScheme
    , uacSecretARN
    , uacDescription

    -- * UserAuthConfigInfo
    , UserAuthConfigInfo
    , userAuthConfigInfo
    , uaciIAMAuth
    , uaciUserName
    , uaciAuthScheme
    , uaciSecretARN
    , uaciDescription

    -- * VPCSecurityGroupMembership
    , VPCSecurityGroupMembership
    , vpcSecurityGroupMembership
    , vsgmStatus
    , vsgmVPCSecurityGroupId

    -- * VPNDetails
    , VPNDetails
    , vpnDetails
    , vdVPNName
    , vdVPNTunnelOriginatorIP
    , vdVPNId
    , vdVPNState
    , vdVPNPSK
    , vdVPNGatewayIP

    -- * ValidDBInstanceModificationsMessage
    , ValidDBInstanceModificationsMessage
    , validDBInstanceModificationsMessage
    , vdimmValidProcessorFeatures
    , vdimmStorage

    -- * ValidStorageOptions
    , ValidStorageOptions
    , validStorageOptions
    , vsoStorageSize
    , vsoProvisionedIOPS
    , vsoIOPSToStorageRatio
    , vsoSupportsStorageAutoscaling
    , vsoStorageType
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.RDS.Types.ActivityStreamMode
import Network.AWS.RDS.Types.ActivityStreamStatus
import Network.AWS.RDS.Types.ApplyMethod
import Network.AWS.RDS.Types.AuthScheme
import Network.AWS.RDS.Types.DBProxyStatus
import Network.AWS.RDS.Types.EngineFamily
import Network.AWS.RDS.Types.IAMAuthMode
import Network.AWS.RDS.Types.SourceType
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState
import Network.AWS.RDS.Types.TargetType
import Network.AWS.RDS.Types.AccountQuota
import Network.AWS.RDS.Types.AvailabilityZone
import Network.AWS.RDS.Types.AvailableProcessorFeature
import Network.AWS.RDS.Types.Certificate
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.CloudwatchLogsExportConfiguration
import Network.AWS.RDS.Types.ConnectionPoolConfiguration
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo
import Network.AWS.RDS.Types.CustomAvailabilityZone
import Network.AWS.RDS.Types.DBCluster
import Network.AWS.RDS.Types.DBClusterBacktrack
import Network.AWS.RDS.Types.DBClusterEndpoint
import Network.AWS.RDS.Types.DBClusterMember
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
import Network.AWS.RDS.Types.DBClusterParameterGroup
import Network.AWS.RDS.Types.DBClusterParameterGroupNameMessage
import Network.AWS.RDS.Types.DBClusterRole
import Network.AWS.RDS.Types.DBClusterSnapshot
import Network.AWS.RDS.Types.DBClusterSnapshotAttribute
import Network.AWS.RDS.Types.DBClusterSnapshotAttributesResult
import Network.AWS.RDS.Types.DBEngineVersion
import Network.AWS.RDS.Types.DBInstance
import Network.AWS.RDS.Types.DBInstanceAutomatedBackup
import Network.AWS.RDS.Types.DBInstanceRole
import Network.AWS.RDS.Types.DBInstanceStatusInfo
import Network.AWS.RDS.Types.DBParameterGroup
import Network.AWS.RDS.Types.DBParameterGroupNameMessage
import Network.AWS.RDS.Types.DBParameterGroupStatus
import Network.AWS.RDS.Types.DBProxy
import Network.AWS.RDS.Types.DBProxyTarget
import Network.AWS.RDS.Types.DBProxyTargetGroup
import Network.AWS.RDS.Types.DBSecurityGroup
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.DBSnapshot
import Network.AWS.RDS.Types.DBSnapshotAttribute
import Network.AWS.RDS.Types.DBSnapshotAttributesResult
import Network.AWS.RDS.Types.DBSubnetGroup
import Network.AWS.RDS.Types.DescribeDBLogFilesDetails
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.DoubleRange
import Network.AWS.RDS.Types.EC2SecurityGroup
import Network.AWS.RDS.Types.Endpoint
import Network.AWS.RDS.Types.EngineDefaults
import Network.AWS.RDS.Types.Event
import Network.AWS.RDS.Types.EventCategoriesMap
import Network.AWS.RDS.Types.EventSubscription
import Network.AWS.RDS.Types.ExportTask
import Network.AWS.RDS.Types.Filter
import Network.AWS.RDS.Types.GlobalCluster
import Network.AWS.RDS.Types.GlobalClusterMember
import Network.AWS.RDS.Types.IPRange
import Network.AWS.RDS.Types.InstallationMedia
import Network.AWS.RDS.Types.InstallationMediaFailureCause
import Network.AWS.RDS.Types.MinimumEngineVersionPerAllowedValue
import Network.AWS.RDS.Types.Option
import Network.AWS.RDS.Types.OptionConfiguration
import Network.AWS.RDS.Types.OptionGroup
import Network.AWS.RDS.Types.OptionGroupMembership
import Network.AWS.RDS.Types.OptionGroupOption
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionSetting
import Network.AWS.RDS.Types.OptionVersion
import Network.AWS.RDS.Types.OrderableDBInstanceOption
import Network.AWS.RDS.Types.Parameter
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports
import Network.AWS.RDS.Types.PendingMaintenanceAction
import Network.AWS.RDS.Types.PendingModifiedValues
import Network.AWS.RDS.Types.ProcessorFeature
import Network.AWS.RDS.Types.Range
import Network.AWS.RDS.Types.RecurringCharge
import Network.AWS.RDS.Types.ReservedDBInstance
import Network.AWS.RDS.Types.ReservedDBInstancesOffering
import Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
import Network.AWS.RDS.Types.RestoreWindow
import Network.AWS.RDS.Types.ScalingConfiguration
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.SourceRegion
import Network.AWS.RDS.Types.Subnet
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget
import Network.AWS.RDS.Types.UserAuthConfig
import Network.AWS.RDS.Types.UserAuthConfigInfo
import Network.AWS.RDS.Types.VPCSecurityGroupMembership
import Network.AWS.RDS.Types.VPNDetails
import Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
import Network.AWS.RDS.Types.ValidStorageOptions

-- | API version @2014-10-31@ of the Amazon Relational Database Service SDK configuration.
rds :: Service
rds
  = Service{_svcAbbrev = "RDS", _svcSigner = v4,
            _svcPrefix = "rds", _svcVersion = "2014-10-31",
            _svcEndpoint = defaultEndpoint rds,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseXMLError "RDS", _svcRetry = retry}
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

-- | @DBSubnetGroupName@ doesn't refer to an existing DB subnet group. 
--
--
_DBSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotFoundFault
  = _MatchServiceError rds "DBSubnetGroupNotFoundFault"
      . hasStatus 404

-- | The quota of 20 option groups was exceeded for this AWS account.
--
--
_OptionGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupQuotaExceededFault
  = _MatchServiceError rds
      "OptionGroupQuotaExceededFault"
      . hasStatus 400

-- | The user already has a DB instance with the given identifier.
--
--
_DBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAlreadyExistsFault
  = _MatchServiceError rds "DBInstanceAlreadyExists" .
      hasStatus 400

-- | @Capacity@ isn't a valid Aurora Serverless DB cluster capacity. Valid capacity values are @2@ , @4@ , @8@ , @16@ , @32@ , @64@ , @128@ , and @256@ .
--
--
_InvalidDBClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterCapacityFault
  = _MatchServiceError rds
      "InvalidDBClusterCapacityFault"
      . hasStatus 400

-- | The state of the DB security group doesn't allow deletion.
--
--
_InvalidDBSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSecurityGroupStateFault
  = _MatchServiceError rds
      "InvalidDBSecurityGroupState"
      . hasStatus 400

-- | @DBSnapshotIdentifier@ doesn't refer to an existing DB snapshot. 
--
--
_DBSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotNotFoundFault
  = _MatchServiceError rds "DBSnapshotNotFound" .
      hasStatus 404

-- | The user attempted to create a new DB cluster and the user has already reached the maximum allowed DB cluster quota.
--
--
_DBClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterQuotaExceededFault
  = _MatchServiceError rds
      "DBClusterQuotaExceededFault"
      . hasStatus 403

-- | A DB security group with the name specified in @DBSecurityGroupName@ already exists. 
--
--
_DBSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupAlreadyExistsFault
  = _MatchServiceError rds
      "DBSecurityGroupAlreadyExists"
      . hasStatus 400

-- | You have exceeded the maximum number of custom Availability Zones.
--
--
_CustomAvailabilityZoneQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CustomAvailabilityZoneQuotaExceededFault
  = _MatchServiceError rds
      "CustomAvailabilityZoneQuotaExceeded"
      . hasStatus 400

-- | The option group isn't in the /available/ state. 
--
--
_InvalidOptionGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOptionGroupStateFault
  = _MatchServiceError rds
      "InvalidOptionGroupStateFault"
      . hasStatus 400

-- | The requested operation can't be performed while the cluster is in this state.
--
--
_InvalidDBClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterStateFault
  = _MatchServiceError rds "InvalidDBClusterStateFault"
      . hasStatus 400

-- | The option group you are trying to create already exists.
--
--
_OptionGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupAlreadyExistsFault
  = _MatchServiceError rds
      "OptionGroupAlreadyExistsFault"
      . hasStatus 400

-- | The export task doesn't exist.
--
--
_ExportTaskNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ExportTaskNotFoundFault
  = _MatchServiceError rds "ExportTaskNotFound" .
      hasStatus 404

-- | 
--
--
_GlobalClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalClusterNotFoundFault
  = _MatchServiceError rds "GlobalClusterNotFoundFault"
      . hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB security groups.
--
--
_DBSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupQuotaExceededFault
  = _MatchServiceError rds
      "QuotaExceeded.DBSecurityGroup"
      . hasStatus 400

-- | @CustomAvailabilityZoneName@ is already used by an existing custom Availability Zone.
--
--
_CustomAvailabilityZoneAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CustomAvailabilityZoneAlreadyExistsFault
  = _MatchServiceError rds
      "CustomAvailabilityZoneAlreadyExists"
      . hasStatus 400

-- | The user already has a DB cluster with the given identifier.
--
--
_DBClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterAlreadyExistsFault
  = _MatchServiceError rds
      "DBClusterAlreadyExistsFault"
      . hasStatus 400

-- | 
--
--
_GlobalClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalClusterAlreadyExistsFault
  = _MatchServiceError rds
      "GlobalClusterAlreadyExistsFault"
      . hasStatus 400

-- | Your AWS account already has the maximum number of proxies in the specified AWS Region.
--
--
_DBProxyQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyQuotaExceededFault
  = _MatchServiceError rds "DBProxyQuotaExceededFault"
      . hasStatus 400

-- | 
--
--
_InvalidGlobalClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGlobalClusterStateFault
  = _MatchServiceError rds
      "InvalidGlobalClusterStateFault"
      . hasStatus 400

-- | The specified target group isn't available for a proxy owned by your AWS account in the specified AWS Region.
--
--
_DBProxyTargetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyTargetGroupNotFoundFault
  = _MatchServiceError rds
      "DBProxyTargetGroupNotFoundFault"
      . hasStatus 404

-- | The DBSubnetGroup shouldn't be specified while creating read replicas that lie in the same region as the source instance.
--
--
_DBSubnetGroupNotAllowedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupNotAllowedFault
  = _MatchServiceError rds
      "DBSubnetGroupNotAllowedFault"
      . hasStatus 400

-- | You can't start an export task that's already running.
--
--
_ExportTaskAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ExportTaskAlreadyExistsFault
  = _MatchServiceError rds "ExportTaskAlreadyExists" .
      hasStatus 400

-- | The requested subnet is invalid, or multiple subnets were requested that are not all in a common VPC.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet
  = _MatchServiceError rds "InvalidSubnet" .
      hasStatus 400

-- | The supplied category does not exist.
--
--
_SubscriptionCategoryNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionCategoryNotFoundFault
  = _MatchServiceError rds
      "SubscriptionCategoryNotFound"
      . hasStatus 404

-- | The specified CIDR IP range or Amazon EC2 security group might not be authorized for the specified DB security group.
--
--
-- Or, RDS might not be authorized to perform necessary actions using IAM on your behalf.
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault
  = _MatchServiceError rds "AuthorizationNotFound" .
      hasStatus 404

-- | The subscription name does not exist.
--
--
_SubscriptionNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionNotFoundFault
  = _MatchServiceError rds "SubscriptionNotFound" .
      hasStatus 404

-- | The proxy is already associated with the specified RDS DB instance or Aurora DB cluster.
--
--
_DBProxyTargetAlreadyRegisteredFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyTargetAlreadyRegisteredFault
  = _MatchServiceError rds
      "DBProxyTargetAlreadyRegisteredFault"
      . hasStatus 400

-- | The IAM role requires additional permissions to export to an Amazon S3 bucket.
--
--
_IAMRoleMissingPermissionsFault :: AsError a => Getting (First ServiceError) a ServiceError
_IAMRoleMissingPermissionsFault
  = _MatchServiceError rds "IamRoleMissingPermissions"
      . hasStatus 400

-- | Prism for BackupPolicyNotFoundFault' errors.
_BackupPolicyNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_BackupPolicyNotFoundFault
  = _MatchServiceError rds "BackupPolicyNotFoundFault"
      . hasStatus 404

-- | The specified CIDR IP range or Amazon EC2 security group is already authorized for the specified DB security group.
--
--
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault
  = _MatchServiceError rds "AuthorizationAlreadyExists"
      . hasStatus 400

-- | The DB security group authorization quota has been reached.
--
--
_AuthorizationQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationQuotaExceededFault
  = _MatchServiceError rds "AuthorizationQuotaExceeded"
      . hasStatus 400

-- | The supplied value isn't a valid DB cluster snapshot state.
--
--
_InvalidDBClusterSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterSnapshotStateFault
  = _MatchServiceError rds
      "InvalidDBClusterSnapshotStateFault"
      . hasStatus 400

-- | SNS has responded that there is a problem with the SND topic specified.
--
--
_SNSInvalidTopicFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSInvalidTopicFault
  = _MatchServiceError rds "SNSInvalidTopic" .
      hasStatus 400

-- | @SourceDBInstanceIdentifier@ refers to a DB instance with @BackupRetentionPeriod@ equal to 0. 
--
--
_PointInTimeRestoreNotEnabledFault :: AsError a => Getting (First ServiceError) a ServiceError
_PointInTimeRestoreNotEnabledFault
  = _MatchServiceError rds
      "PointInTimeRestoreNotEnabled"
      . hasStatus 400

-- | The DB parameter group is in use or is in an invalid state. If you are attempting to delete the parameter group, you can't delete it when the parameter group is in this state.
--
--
_InvalidDBParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBParameterGroupStateFault
  = _MatchServiceError rds
      "InvalidDBParameterGroupState"
      . hasStatus 400

-- | The user already has a DB cluster snapshot with the given identifier.
--
--
_DBClusterSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotAlreadyExistsFault
  = _MatchServiceError rds
      "DBClusterSnapshotAlreadyExistsFault"
      . hasStatus 400

-- | The requested source could not be found.
--
--
_SourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SourceNotFoundFault
  = _MatchServiceError rds "SourceNotFound" .
      hasStatus 404

-- | Specified offering does not exist.
--
--
_ReservedDBInstancesOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstancesOfferingNotFoundFault
  = _MatchServiceError rds
      "ReservedDBInstancesOfferingNotFound"
      . hasStatus 404

-- | @DBClusterSnapshotIdentifier@ doesn't refer to an existing DB cluster snapshot. 
--
--
_DBClusterSnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterSnapshotNotFoundFault
  = _MatchServiceError rds
      "DBClusterSnapshotNotFoundFault"
      . hasStatus 404

-- | The request would result in the user exceeding the allowed number of DB instances.
--
--
_InstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceQuotaExceededFault
  = _MatchServiceError rds "InstanceQuotaExceeded" .
      hasStatus 400

-- | The specified proxy name must be unique for all proxies owned by your AWS account in the specified AWS Region.
--
--
_DBProxyAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyAlreadyExistsFault
  = _MatchServiceError rds "DBProxyTargetExistsFault" .
      hasStatus 400

-- | @DBInstanceIdentifier@ doesn't refer to an existing DB instance. 
--
--
_DBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceNotFoundFault
  = _MatchServiceError rds "DBInstanceNotFound" .
      hasStatus 404

-- | The state of the DB snapshot doesn't allow deletion.
--
--
_InvalidDBSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSnapshotStateFault
  = _MatchServiceError rds "InvalidDBSnapshotState" .
      hasStatus 400

-- | A DB security group isn't allowed for this action.
--
--
_DBSecurityGroupNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotSupportedFault
  = _MatchServiceError rds
      "DBSecurityGroupNotSupported"
      . hasStatus 400

-- | The requested operation can't be performed while the proxy is in this state.
--
--
_InvalidDBProxyStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBProxyStateFault
  = _MatchServiceError rds "InvalidDBProxyStateFault" .
      hasStatus 400

-- | @DBSecurityGroupName@ doesn't refer to an existing DB security group. 
--
--
_DBSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSecurityGroupNotFoundFault
  = _MatchServiceError rds "DBSecurityGroupNotFound" .
      hasStatus 404

-- | 
--
--
_GlobalClusterQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalClusterQuotaExceededFault
  = _MatchServiceError rds
      "GlobalClusterQuotaExceededFault"
      . hasStatus 400

-- | The request would result in the user exceeding the allowed amount of storage available across all DB instances.
--
--
_StorageQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageQuotaExceededFault
  = _MatchServiceError rds "StorageQuotaExceeded" .
      hasStatus 400

-- | The specified proxy name doesn't correspond to a proxy owned by your AWS accoutn in the specified AWS Region.
--
--
_DBProxyNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyNotFoundFault
  = _MatchServiceError rds "DBProxyNotFoundFault" .
      hasStatus 404

-- | The SNS topic ARN does not exist.
--
--
_SNSTopicARNNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSTopicARNNotFoundFault
  = _MatchServiceError rds "SNSTopicArnNotFound" .
      hasStatus 404

-- | An error occurred accessing an AWS KMS key.
--
--
_KMSKeyNotAccessibleFault :: AsError a => Getting (First ServiceError) a ServiceError
_KMSKeyNotAccessibleFault
  = _MatchServiceError rds "KMSKeyNotAccessibleFault" .
      hasStatus 400

-- | The specified custom endpoint can't be created because it already exists.
--
--
_DBClusterEndpointAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterEndpointAlreadyExistsFault
  = _MatchServiceError rds
      "DBClusterEndpointAlreadyExistsFault"
      . hasStatus 400

-- | The export is invalid for exporting to an Amazon S3 bucket.
--
--
_InvalidExportOnlyFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidExportOnlyFault
  = _MatchServiceError rds "InvalidExportOnly" .
      hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB snapshots.
--
--
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault
  = _MatchServiceError rds "SnapshotQuotaExceeded" .
      hasStatus 400

-- | The DB instance isn't in a valid state.
--
--
_InvalidDBInstanceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceStateFault
  = _MatchServiceError rds "InvalidDBInstanceState" .
      hasStatus 400

-- | The automated backup is in an invalid state. For example, this automated backup is associated with an active instance. 
--
--
_InvalidDBInstanceAutomatedBackupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBInstanceAutomatedBackupStateFault
  = _MatchServiceError rds
      "InvalidDBInstanceAutomatedBackupState"
      . hasStatus 400

-- | The DB upgrade failed because a resource the DB depends on can't be modified.
--
--
_DBUpgradeDependencyFailureFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBUpgradeDependencyFailureFault
  = _MatchServiceError rds "DBUpgradeDependencyFailure"
      . hasStatus 400

-- | @DBClusterParameterGroupName@ doesn't refer to an existing DB cluster parameter group. 
--
--
_DBClusterParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterParameterGroupNotFoundFault
  = _MatchServiceError rds
      "DBClusterParameterGroupNotFound"
      . hasStatus 404

-- | Cannot restore from VPC backup to non-VPC DB instance.
--
--
_InvalidRestoreFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreFault
  = _MatchServiceError rds "InvalidRestoreFault" .
      hasStatus 400

-- | This error can occur if someone else is modifying a subscription. You should retry the action.
--
--
_InvalidEventSubscriptionStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEventSubscriptionStateFault
  = _MatchServiceError rds
      "InvalidEventSubscriptionState"
      . hasStatus 400

-- | The specified resource ID was not found.
--
--
_ResourceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundFault
  = _MatchServiceError rds "ResourceNotFoundFault" .
      hasStatus 404

-- | The requested operation can't be performed on the endpoint while the endpoint is in this state.
--
--
_InvalidDBClusterEndpointStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBClusterEndpointStateFault
  = _MatchServiceError rds
      "InvalidDBClusterEndpointStateFault"
      . hasStatus 400

-- | There is insufficient storage available for the current action. You might be able to resolve this error by updating your subnet group to use different Availability Zones that have more storage available.
--
--
_InsufficientStorageClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientStorageClusterCapacityFault
  = _MatchServiceError rds
      "InsufficientStorageClusterCapacity"
      . hasStatus 400

-- | Storage of the @StorageType@ specified can't be associated with the DB instance. 
--
--
_StorageTypeNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_StorageTypeNotSupportedFault
  = _MatchServiceError rds "StorageTypeNotSupported" .
      hasStatus 400

-- | You have reached the maximum number of event subscriptions.
--
--
_EventSubscriptionQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_EventSubscriptionQuotaExceededFault
  = _MatchServiceError rds
      "EventSubscriptionQuotaExceeded"
      . hasStatus 400

-- | The quota for retained automated backups was exceeded. This prevents you from retaining any additional automated backups. The retained automated backups quota is the same as your DB Instance quota.
--
--
_DBInstanceAutomatedBackupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAutomatedBackupQuotaExceededFault
  = _MatchServiceError rds
      "DBInstanceAutomatedBackupQuotaExceeded"
      . hasStatus 400

-- | The IAM role is missing for exporting to an Amazon S3 bucket.
--
--
_IAMRoleNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_IAMRoleNotFoundFault
  = _MatchServiceError rds "IamRoleNotFound" .
      hasStatus 404

-- | The cluster already has the maximum number of custom endpoints.
--
--
_DBClusterEndpointQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterEndpointQuotaExceededFault
  = _MatchServiceError rds
      "DBClusterEndpointQuotaExceededFault"
      . hasStatus 403

-- | The request would result in the user exceeding the allowed number of DB subnet groups.
--
--
_DBSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupQuotaExceededFault
  = _MatchServiceError rds "DBSubnetGroupQuotaExceeded"
      . hasStatus 400

-- | You can't cancel an export task that has completed.
--
--
_InvalidExportTaskStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidExportTaskStateFault
  = _MatchServiceError rds
      "InvalidExportTaskStateFault"
      . hasStatus 400

-- | @DBClusterIdentifier@ doesn't refer to an existing DB cluster. 
--
--
_DBClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterNotFoundFault
  = _MatchServiceError rds "DBClusterNotFoundFault" .
      hasStatus 404

-- | @LogFileName@ doesn't refer to an existing DB log file.
--
--
_DBLogFileNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBLogFileNotFoundFault
  = _MatchServiceError rds "DBLogFileNotFoundFault" .
      hasStatus 404

-- | The specified Amazon S3 bucket name can't be found or Amazon RDS isn't authorized to access the specified Amazon S3 bucket. Verify the __SourceS3BucketName__ and __S3IngestionRoleArn__ values and try again.
--
--
_InvalidS3BucketFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketFault
  = _MatchServiceError rds "InvalidS3BucketFault" .
      hasStatus 400

-- | @CustomAvailabilityZoneId@ doesn't refer to an existing custom Availability Zone identifier.
--
--
_CustomAvailabilityZoneNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CustomAvailabilityZoneNotFoundFault
  = _MatchServiceError rds
      "CustomAvailabilityZoneNotFound"
      . hasStatus 404

-- | The state of the export snapshot is invalid for exporting to an Amazon S3 bucket.
--
--
_InvalidExportSourceStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidExportSourceStateFault
  = _MatchServiceError rds "InvalidExportSourceState" .
      hasStatus 400

-- | The specified option group could not be found.
--
--
_OptionGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_OptionGroupNotFoundFault
  = _MatchServiceError rds "OptionGroupNotFoundFault" .
      hasStatus 404

-- | The specified RDS DB instance or Aurora DB cluster isn't available for a proxy owned by your AWS account in the specified AWS Region.
--
--
_DBProxyTargetNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBProxyTargetNotFoundFault
  = _MatchServiceError rds "DBProxyTargetNotFoundFault"
      . hasStatus 404

-- | You have exceeded the maximum number of accounts that you can share a manual DB snapshot with.
--
--
_SharedSnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SharedSnapshotQuotaExceededFault
  = _MatchServiceError rds
      "SharedSnapshotQuotaExceeded"
      . hasStatus 400

-- | The specified reserved DB Instance not found.
--
--
_ReservedDBInstanceNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceNotFoundFault
  = _MatchServiceError rds "ReservedDBInstanceNotFound"
      . hasStatus 404

-- | The request would result in the user exceeding the allowed number of subnets in a DB subnet groups.
--
--
_DBSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetQuotaExceededFault
  = _MatchServiceError rds "DBSubnetQuotaExceededFault"
      . hasStatus 400

-- | The specified @RoleArn@ value doesn't match the specified feature for the DB instance.
--
--
_DBInstanceRoleNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceRoleNotFoundFault
  = _MatchServiceError rds "DBInstanceRoleNotFound" .
      hasStatus 404

-- | Provisioned IOPS not available in the specified Availability Zone.
--
--
_ProvisionedIOPSNotAvailableInAZFault :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedIOPSNotAvailableInAZFault
  = _MatchServiceError rds
      "ProvisionedIopsNotAvailableInAZFault"
      . hasStatus 400

-- | User already has a reservation with the given identifier.
--
--
_ReservedDBInstanceAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceAlreadyExistsFault
  = _MatchServiceError rds
      "ReservedDBInstanceAlreadyExists"
      . hasStatus 404

-- | The specified @RoleArn@ or @FeatureName@ value is already associated with the DB instance.
--
--
_DBInstanceRoleAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceRoleAlreadyExistsFault
  = _MatchServiceError rds
      "DBInstanceRoleAlreadyExists"
      . hasStatus 400

-- | You have exceeded the maximum number of IAM roles that can be associated with the specified DB cluster.
--
--
_DBClusterRoleQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleQuotaExceededFault
  = _MatchServiceError rds "DBClusterRoleQuotaExceeded"
      . hasStatus 400

-- | The DB cluster doesn't have enough capacity for the current operation.
--
--
_InsufficientDBClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBClusterCapacityFault
  = _MatchServiceError rds
      "InsufficientDBClusterCapacityFault"
      . hasStatus 403

-- | The DB subnet group doesn't cover all Availability Zones after it's created because of users' change.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault
  = _MatchServiceError rds
      "InvalidVPCNetworkStateFault"
      . hasStatus 400

-- | The request would result in the user exceeding the allowed number of DB parameter groups.
--
--
_DBParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupQuotaExceededFault
  = _MatchServiceError rds
      "DBParameterGroupQuotaExceeded"
      . hasStatus 400

-- | The specified DB instance class isn't available in the specified Availability Zone.
--
--
_InsufficientDBInstanceCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDBInstanceCapacityFault
  = _MatchServiceError rds
      "InsufficientDBInstanceCapacity"
      . hasStatus 400

-- | A DB parameter group with the same name exists.
--
--
_DBParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupAlreadyExistsFault
  = _MatchServiceError rds
      "DBParameterGroupAlreadyExists"
      . hasStatus 400

-- | Request would exceed the user's DB Instance quota.
--
--
_ReservedDBInstanceQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedDBInstanceQuotaExceededFault
  = _MatchServiceError rds
      "ReservedDBInstanceQuotaExceeded"
      . hasStatus 400

-- | You can't associate any more AWS Identity and Access Management (IAM) roles with the DB instance because the quota has been reached.
--
--
_DBInstanceRoleQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceRoleQuotaExceededFault
  = _MatchServiceError rds
      "DBInstanceRoleQuotaExceeded"
      . hasStatus 400

-- | @CertificateIdentifier@ doesn't refer to an existing certificate. 
--
--
_CertificateNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CertificateNotFoundFault
  = _MatchServiceError rds "CertificateNotFound" .
      hasStatus 404

-- | The specified IAM role Amazon Resource Name (ARN) is already associated with the specified DB cluster.
--
--
_DBClusterRoleAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleAlreadyExistsFault
  = _MatchServiceError rds "DBClusterRoleAlreadyExists"
      . hasStatus 400

-- | The supplied subscription name already exists.
--
--
_SubscriptionAlreadyExistFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubscriptionAlreadyExistFault
  = _MatchServiceError rds "SubscriptionAlreadyExist" .
      hasStatus 400

-- | The specified installation medium has already been imported.
--
--
_InstallationMediaAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstallationMediaAlreadyExistsFault
  = _MatchServiceError rds
      "InstallationMediaAlreadyExists"
      . hasStatus 400

-- | The DBSubnetGroup doesn't belong to the same VPC as that of an existing cross-region read replica of the same source instance.
--
--
_InvalidDBSubnetGroupFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupFault
  = _MatchServiceError rds "InvalidDBSubnetGroupFault"
      . hasStatus 400

-- | The DB subnet isn't in the /available/ state. 
--
--
_InvalidDBSubnetStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetStateFault
  = _MatchServiceError rds "InvalidDBSubnetStateFault"
      . hasStatus 400

-- | @BacktrackIdentifier@ doesn't refer to an existing backtrack. 
--
--
_DBClusterBacktrackNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterBacktrackNotFoundFault
  = _MatchServiceError rds
      "DBClusterBacktrackNotFoundFault"
      . hasStatus 404

-- | The specified IAM role Amazon Resource Name (ARN) isn't associated with the specified DB cluster.
--
--
_DBClusterRoleNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterRoleNotFoundFault
  = _MatchServiceError rds "DBClusterRoleNotFound" .
      hasStatus 404

-- | @Domain@ doesn't refer to an existing Active Directory domain. 
--
--
_DomainNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DomainNotFoundFault
  = _MatchServiceError rds "DomainNotFoundFault" .
      hasStatus 404

-- | @InstallationMediaID@ doesn't refer to an existing installation medium.
--
--
_InstallationMediaNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_InstallationMediaNotFoundFault
  = _MatchServiceError rds "InstallationMediaNotFound"
      . hasStatus 404

-- | The DB subnet is already in use in the Availability Zone.
--
--
_SubnetAlreadyInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetAlreadyInUse
  = _MatchServiceError rds "SubnetAlreadyInUse" .
      hasStatus 400

-- | @DBParameterGroupName@ doesn't refer to an existing DB parameter group. 
--
--
_DBParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBParameterGroupNotFoundFault
  = _MatchServiceError rds "DBParameterGroupNotFound" .
      hasStatus 404

-- | Subnets in the DB subnet group should cover at least two Availability Zones unless there is only one Availability Zone.
--
--
_DBSubnetGroupDoesNotCoverEnoughAZs :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs
  = _MatchServiceError rds
      "DBSubnetGroupDoesNotCoverEnoughAZs"
      . hasStatus 400

-- | You do not have permission to publish to the SNS topic ARN.
--
--
_SNSNoAuthorizationFault :: AsError a => Getting (First ServiceError) a ServiceError
_SNSNoAuthorizationFault
  = _MatchServiceError rds "SNSNoAuthorization" .
      hasStatus 400

-- | No automated backup for this DB instance was found.
--
--
_DBInstanceAutomatedBackupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBInstanceAutomatedBackupNotFoundFault
  = _MatchServiceError rds
      "DBInstanceAutomatedBackupNotFound"
      . hasStatus 404

-- | @DBSubnetGroupName@ is already used by an existing DB subnet group. 
--
--
_DBSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSubnetGroupAlreadyExistsFault
  = _MatchServiceError rds "DBSubnetGroupAlreadyExists"
      . hasStatus 400

-- | The specified custom endpoint doesn't exist.
--
--
_DBClusterEndpointNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBClusterEndpointNotFoundFault
  = _MatchServiceError rds
      "DBClusterEndpointNotFoundFault"
      . hasStatus 400

-- | @DBSnapshotIdentifier@ is already used by an existing snapshot. 
--
--
_DBSnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_DBSnapshotAlreadyExistsFault
  = _MatchServiceError rds "DBSnapshotAlreadyExists" .
      hasStatus 400

-- | The DB subnet group cannot be deleted because it's in use.
--
--
_InvalidDBSubnetGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDBSubnetGroupStateFault
  = _MatchServiceError rds
      "InvalidDBSubnetGroupStateFault"
      . hasStatus 400
