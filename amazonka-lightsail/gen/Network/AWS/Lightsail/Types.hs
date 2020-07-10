{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types
    (
    -- * Service Configuration
      lightsail

    -- * Errors
    , _AccountSetupInProgressException
    , _UnauthenticatedException
    , _ServiceException
    , _AccessDeniedException
    , _InvalidInputException
    , _OperationFailureException
    , _NotFoundException

    -- * AccessDirection
    , AccessDirection (..)

    -- * AddOnType
    , AddOnType (..)

    -- * AlarmState
    , AlarmState (..)

    -- * AutoSnapshotStatus
    , AutoSnapshotStatus (..)

    -- * BlueprintType
    , BlueprintType (..)

    -- * CloudFormationStackRecordSourceType
    , CloudFormationStackRecordSourceType (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ContactMethodStatus
    , ContactMethodStatus (..)

    -- * ContactMethodVerificationProtocol
    , ContactMethodVerificationProtocol (..)

    -- * ContactProtocol
    , ContactProtocol (..)

    -- * DiskSnapshotState
    , DiskSnapshotState (..)

    -- * DiskState
    , DiskState (..)

    -- * ExportSnapshotRecordSourceType
    , ExportSnapshotRecordSourceType (..)

    -- * InstanceAccessProtocol
    , InstanceAccessProtocol (..)

    -- * InstanceHealthReason
    , InstanceHealthReason (..)

    -- * InstanceHealthState
    , InstanceHealthState (..)

    -- * InstanceMetricName
    , InstanceMetricName (..)

    -- * InstancePlatform
    , InstancePlatform (..)

    -- * InstanceSnapshotState
    , InstanceSnapshotState (..)

    -- * LoadBalancerAttributeName
    , LoadBalancerAttributeName (..)

    -- * LoadBalancerMetricName
    , LoadBalancerMetricName (..)

    -- * LoadBalancerProtocol
    , LoadBalancerProtocol (..)

    -- * LoadBalancerState
    , LoadBalancerState (..)

    -- * LoadBalancerTLSCertificateDomainStatus
    , LoadBalancerTLSCertificateDomainStatus (..)

    -- * LoadBalancerTLSCertificateFailureReason
    , LoadBalancerTLSCertificateFailureReason (..)

    -- * LoadBalancerTLSCertificateRenewalStatus
    , LoadBalancerTLSCertificateRenewalStatus (..)

    -- * LoadBalancerTLSCertificateRevocationReason
    , LoadBalancerTLSCertificateRevocationReason (..)

    -- * LoadBalancerTLSCertificateStatus
    , LoadBalancerTLSCertificateStatus (..)

    -- * MetricName
    , MetricName (..)

    -- * MetricStatistic
    , MetricStatistic (..)

    -- * MetricUnit
    , MetricUnit (..)

    -- * NetworkProtocol
    , NetworkProtocol (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * OperationType
    , OperationType (..)

    -- * PortAccessType
    , PortAccessType (..)

    -- * PortInfoSourceType
    , PortInfoSourceType (..)

    -- * PortState
    , PortState (..)

    -- * RecordState
    , RecordState (..)

    -- * RegionName
    , RegionName (..)

    -- * RelationalDatabaseEngine
    , RelationalDatabaseEngine (..)

    -- * RelationalDatabaseMetricName
    , RelationalDatabaseMetricName (..)

    -- * RelationalDatabasePasswordVersion
    , RelationalDatabasePasswordVersion (..)

    -- * ResourceType
    , ResourceType (..)

    -- * TreatMissingData
    , TreatMissingData (..)

    -- * AddOn
    , AddOn
    , addOn
    , aoStatus
    , aoNextSnapshotTimeOfDay
    , aoSnapshotTimeOfDay
    , aoName

    -- * AddOnRequest
    , AddOnRequest
    , addOnRequest
    , aorAutoSnapshotAddOnRequest
    , aorAddOnType

    -- * Alarm
    , Alarm
    , alarm
    , aState
    , aTreatMissingData
    , aResourceType
    , aArn
    , aCreatedAt
    , aLocation
    , aContactProtocols
    , aPeriod
    , aEvaluationPeriods
    , aMetricName
    , aComparisonOperator
    , aName
    , aThreshold
    , aDatapointsToAlarm
    , aSupportCode
    , aNotificationEnabled
    , aNotificationTriggers
    , aStatistic
    , aUnit
    , aMonitoredResourceInfo

    -- * AttachedDisk
    , AttachedDisk
    , attachedDisk
    , adPath
    , adSizeInGb

    -- * AutoSnapshotAddOnRequest
    , AutoSnapshotAddOnRequest
    , autoSnapshotAddOnRequest
    , asaorSnapshotTimeOfDay

    -- * AutoSnapshotDetails
    , AutoSnapshotDetails
    , autoSnapshotDetails
    , asdStatus
    , asdFromAttachedDisks
    , asdCreatedAt
    , asdDate

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azState
    , azZoneName

    -- * Blueprint
    , Blueprint
    , blueprint
    , bVersionCode
    , bPlatform
    , bGroup
    , bMinPower
    , bProductURL
    , bLicenseURL
    , bName
    , bVersion
    , bBlueprintId
    , bType
    , bIsActive
    , bDescription

    -- * Bundle
    , Bundle
    , bundle
    , bunCpuCount
    , bunTransferPerMonthInGb
    , bunBundleId
    , bunInstanceType
    , bunName
    , bunPower
    , bunDiskSizeInGb
    , bunSupportedPlatforms
    , bunPrice
    , bunIsActive
    , bunRamSizeInGb

    -- * CloudFormationStackRecord
    , CloudFormationStackRecord
    , cloudFormationStackRecord
    , cfsrState
    , cfsrDestinationInfo
    , cfsrResourceType
    , cfsrArn
    , cfsrCreatedAt
    , cfsrLocation
    , cfsrName
    , cfsrSourceInfo

    -- * CloudFormationStackRecordSourceInfo
    , CloudFormationStackRecordSourceInfo
    , cloudFormationStackRecordSourceInfo
    , cfsrsiResourceType
    , cfsrsiArn
    , cfsrsiName

    -- * ContactMethod
    , ContactMethod
    , contactMethod
    , cmStatus
    , cmResourceType
    , cmArn
    , cmCreatedAt
    , cmLocation
    , cmProtocol
    , cmName
    , cmSupportCode
    , cmContactEndpoint

    -- * DestinationInfo
    , DestinationInfo
    , destinationInfo
    , diService
    , diId

    -- * Disk
    , Disk
    , disk
    , dState
    , dResourceType
    , dArn
    , dPath
    , dCreatedAt
    , dLocation
    , dIops
    , dIsAttached
    , dAddOns
    , dAttachmentState
    , dName
    , dSizeInGb
    , dSupportCode
    , dIsSystemDisk
    , dAttachedTo
    , dGbInUse
    , dTags

    -- * DiskInfo
    , DiskInfo
    , diskInfo
    , diPath
    , diName
    , diSizeInGb
    , diIsSystemDisk

    -- * DiskMap
    , DiskMap
    , diskMap
    , dmNewDiskName
    , dmOriginalDiskPath

    -- * DiskSnapshot
    , DiskSnapshot
    , diskSnapshot
    , dsFromDiskName
    , dsIsFromAutoSnapshot
    , dsState
    , dsResourceType
    , dsArn
    , dsCreatedAt
    , dsLocation
    , dsProgress
    , dsName
    , dsSizeInGb
    , dsSupportCode
    , dsFromInstanceARN
    , dsFromInstanceName
    , dsFromDiskARN
    , dsTags

    -- * DiskSnapshotInfo
    , DiskSnapshotInfo
    , diskSnapshotInfo
    , dsiSizeInGb

    -- * Domain
    , Domain
    , domain
    , domResourceType
    , domDomainEntries
    , domArn
    , domCreatedAt
    , domLocation
    , domName
    , domSupportCode
    , domTags

    -- * DomainEntry
    , DomainEntry
    , domainEntry
    , deIsAlias
    , deName
    , deId
    , deOptions
    , deType
    , deTarget

    -- * ExportSnapshotRecord
    , ExportSnapshotRecord
    , exportSnapshotRecord
    , esrState
    , esrDestinationInfo
    , esrResourceType
    , esrArn
    , esrCreatedAt
    , esrLocation
    , esrName
    , esrSourceInfo

    -- * ExportSnapshotRecordSourceInfo
    , ExportSnapshotRecordSourceInfo
    , exportSnapshotRecordSourceInfo
    , esrsiDiskSnapshotInfo
    , esrsiResourceType
    , esrsiArn
    , esrsiCreatedAt
    , esrsiFromResourceARN
    , esrsiName
    , esrsiInstanceSnapshotInfo
    , esrsiFromResourceName

    -- * HostKeyAttributes
    , HostKeyAttributes
    , hostKeyAttributes
    , hkaNotValidAfter
    , hkaNotValidBefore
    , hkaFingerprintSHA1
    , hkaPublicKey
    , hkaAlgorithm
    , hkaWitnessedAt
    , hkaFingerprintSHA256

    -- * Instance
    , Instance
    , instance'
    , iState
    , iIpv6Address
    , iResourceType
    , iArn
    , iCreatedAt
    , iLocation
    , iSshKeyName
    , iAddOns
    , iUsername
    , iNetworking
    , iBundleId
    , iName
    , iSupportCode
    , iBlueprintId
    , iPrivateIPAddress
    , iBlueprintName
    , iIsStaticIP
    , iPublicIPAddress
    , iHardware
    , iTags

    -- * InstanceAccessDetails
    , InstanceAccessDetails
    , instanceAccessDetails
    , iadHostKeys
    , iadCertKey
    , iadIpAddress
    , iadPrivateKey
    , iadExpiresAt
    , iadUsername
    , iadProtocol
    , iadPasswordData
    , iadPassword
    , iadInstanceName

    -- * InstanceEntry
    , InstanceEntry
    , instanceEntry
    , ieUserData
    , ieSourceName
    , ieInstanceType
    , iePortInfoSource
    , ieAvailabilityZone

    -- * InstanceHardware
    , InstanceHardware
    , instanceHardware
    , ihCpuCount
    , ihDisks
    , ihRamSizeInGb

    -- * InstanceHealthSummary
    , InstanceHealthSummary
    , instanceHealthSummary
    , ihsInstanceHealth
    , ihsInstanceName
    , ihsInstanceHealthReason

    -- * InstanceNetworking
    , InstanceNetworking
    , instanceNetworking
    , inMonthlyTransfer
    , inPorts

    -- * InstancePortInfo
    , InstancePortInfo
    , instancePortInfo
    , ipiFromPort
    , ipiCommonName
    , ipiProtocol
    , ipiAccessDirection
    , ipiAccessType
    , ipiToPort
    , ipiAccessFrom

    -- * InstancePortState
    , InstancePortState
    , instancePortState
    , ipsFromPort
    , ipsState
    , ipsProtocol
    , ipsToPort

    -- * InstanceSnapshot
    , InstanceSnapshot
    , instanceSnapshot
    , insFromBlueprintId
    , insIsFromAutoSnapshot
    , insState
    , insResourceType
    , insFromAttachedDisks
    , insArn
    , insCreatedAt
    , insLocation
    , insProgress
    , insName
    , insFromBundleId
    , insSizeInGb
    , insSupportCode
    , insFromInstanceARN
    , insFromInstanceName
    , insTags

    -- * InstanceSnapshotInfo
    , InstanceSnapshotInfo
    , instanceSnapshotInfo
    , isiFromBlueprintId
    , isiFromBundleId
    , isiFromDiskInfo

    -- * InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- * KeyPair
    , KeyPair
    , keyPair
    , kpResourceType
    , kpArn
    , kpCreatedAt
    , kpLocation
    , kpFingerprint
    , kpName
    , kpSupportCode
    , kpTags

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbHealthCheckPath
    , lbState
    , lbResourceType
    , lbArn
    , lbCreatedAt
    , lbLocation
    , lbInstancePort
    , lbConfigurationOptions
    , lbProtocol
    , lbTlsCertificateSummaries
    , lbName
    , lbSupportCode
    , lbPublicPorts
    , lbDnsName
    , lbInstanceHealthSummary
    , lbTags

    -- * LoadBalancerTLSCertificate
    , LoadBalancerTLSCertificate
    , loadBalancerTLSCertificate
    , lbtcFailureReason
    , lbtcSubject
    , lbtcStatus
    , lbtcSubjectAlternativeNames
    , lbtcResourceType
    , lbtcArn
    , lbtcCreatedAt
    , lbtcLocation
    , lbtcLoadBalancerName
    , lbtcSerial
    , lbtcIsAttached
    , lbtcRevokedAt
    , lbtcNotBefore
    , lbtcRevocationReason
    , lbtcDomainName
    , lbtcName
    , lbtcRenewalSummary
    , lbtcSupportCode
    , lbtcDomainValidationRecords
    , lbtcIssuedAt
    , lbtcKeyAlgorithm
    , lbtcSignatureAlgorithm
    , lbtcIssuer
    , lbtcTags
    , lbtcNotAfter

    -- * LoadBalancerTLSCertificateDomainValidationOption
    , LoadBalancerTLSCertificateDomainValidationOption
    , loadBalancerTLSCertificateDomainValidationOption
    , lbtcdvoDomainName
    , lbtcdvoValidationStatus

    -- * LoadBalancerTLSCertificateDomainValidationRecord
    , LoadBalancerTLSCertificateDomainValidationRecord
    , loadBalancerTLSCertificateDomainValidationRecord
    , lbtcdvrValue
    , lbtcdvrDomainName
    , lbtcdvrName
    , lbtcdvrValidationStatus
    , lbtcdvrType

    -- * LoadBalancerTLSCertificateRenewalSummary
    , LoadBalancerTLSCertificateRenewalSummary
    , loadBalancerTLSCertificateRenewalSummary
    , lbtcrsRenewalStatus
    , lbtcrsDomainValidationOptions

    -- * LoadBalancerTLSCertificateSummary
    , LoadBalancerTLSCertificateSummary
    , loadBalancerTLSCertificateSummary
    , lbtcsIsAttached
    , lbtcsName

    -- * LogEvent
    , LogEvent
    , logEvent
    , leCreatedAt
    , leMessage

    -- * MetricDatapoint
    , MetricDatapoint
    , metricDatapoint
    , mdSampleCount
    , mdMaximum
    , mdAverage
    , mdMinimum
    , mdSum
    , mdTimestamp
    , mdUnit

    -- * MonitoredResourceInfo
    , MonitoredResourceInfo
    , monitoredResourceInfo
    , mriResourceType
    , mriArn
    , mriName

    -- * MonthlyTransfer
    , MonthlyTransfer
    , monthlyTransfer
    , mtGbPerMonthAllocated

    -- * Operation
    , Operation
    , operation
    , oStatus
    , oOperationDetails
    , oResourceType
    , oCreatedAt
    , oResourceName
    , oLocation
    , oStatusChangedAt
    , oErrorDetails
    , oErrorCode
    , oId
    , oOperationType
    , oIsTerminal

    -- * PasswordData
    , PasswordData
    , passwordData
    , pdKeyPairName
    , pdCiphertext

    -- * PendingMaintenanceAction
    , PendingMaintenanceAction
    , pendingMaintenanceAction
    , pmaAction
    , pmaDescription
    , pmaCurrentApplyDate

    -- * PendingModifiedRelationalDatabaseValues
    , PendingModifiedRelationalDatabaseValues
    , pendingModifiedRelationalDatabaseValues
    , pmrdvEngineVersion
    , pmrdvMasterUserPassword
    , pmrdvBackupRetentionEnabled

    -- * PortInfo
    , PortInfo
    , portInfo
    , piFromPort
    , piProtocol
    , piToPort

    -- * RegionInfo
    , RegionInfo
    , regionInfo
    , riAvailabilityZones
    , riName
    , riRelationalDatabaseAvailabilityZones
    , riDisplayName
    , riContinentCode
    , riDescription

    -- * RelationalDatabase
    , RelationalDatabase
    , relationalDatabase
    , rdEngineVersion
    , rdRelationalDatabaseBundleId
    , rdMasterEndpoint
    , rdState
    , rdResourceType
    , rdPubliclyAccessible
    , rdMasterUsername
    , rdArn
    , rdCreatedAt
    , rdLocation
    , rdEngine
    , rdLatestRestorableTime
    , rdPreferredMaintenanceWindow
    , rdRelationalDatabaseBlueprintId
    , rdCaCertificateIdentifier
    , rdName
    , rdBackupRetentionEnabled
    , rdPreferredBackupWindow
    , rdPendingMaintenanceActions
    , rdSupportCode
    , rdSecondaryAvailabilityZone
    , rdPendingModifiedValues
    , rdMasterDatabaseName
    , rdHardware
    , rdParameterApplyStatus
    , rdTags

    -- * RelationalDatabaseBlueprint
    , RelationalDatabaseBlueprint
    , relationalDatabaseBlueprint
    , rdbEngineVersion
    , rdbIsEngineDefault
    , rdbEngineVersionDescription
    , rdbEngine
    , rdbBlueprintId
    , rdbEngineDescription

    -- * RelationalDatabaseBundle
    , RelationalDatabaseBundle
    , relationalDatabaseBundle
    , rdbIsEncrypted
    , rdbCpuCount
    , rdbTransferPerMonthInGb
    , rdbBundleId
    , rdbName
    , rdbDiskSizeInGb
    , rdbPrice
    , rdbIsActive
    , rdbRamSizeInGb

    -- * RelationalDatabaseEndpoint
    , RelationalDatabaseEndpoint
    , relationalDatabaseEndpoint
    , rdeAddress
    , rdePort

    -- * RelationalDatabaseEvent
    , RelationalDatabaseEvent
    , relationalDatabaseEvent
    , rdeCreatedAt
    , rdeEventCategories
    , rdeResource
    , rdeMessage

    -- * RelationalDatabaseHardware
    , RelationalDatabaseHardware
    , relationalDatabaseHardware
    , rdhCpuCount
    , rdhDiskSizeInGb
    , rdhRamSizeInGb

    -- * RelationalDatabaseParameter
    , RelationalDatabaseParameter
    , relationalDatabaseParameter
    , rdpApplyType
    , rdpParameterValue
    , rdpApplyMethod
    , rdpDataType
    , rdpIsModifiable
    , rdpAllowedValues
    , rdpParameterName
    , rdpDescription

    -- * RelationalDatabaseSnapshot
    , RelationalDatabaseSnapshot
    , relationalDatabaseSnapshot
    , rdsEngineVersion
    , rdsState
    , rdsFromRelationalDatabaseName
    , rdsResourceType
    , rdsFromRelationalDatabaseBlueprintId
    , rdsArn
    , rdsCreatedAt
    , rdsLocation
    , rdsEngine
    , rdsName
    , rdsSizeInGb
    , rdsSupportCode
    , rdsFromRelationalDatabaseARN
    , rdsFromRelationalDatabaseBundleId
    , rdsTags

    -- * ResourceLocation
    , ResourceLocation
    , resourceLocation
    , rlRegionName
    , rlAvailabilityZone

    -- * StaticIP
    , StaticIP
    , staticIP
    , siIpAddress
    , siResourceType
    , siArn
    , siCreatedAt
    , siLocation
    , siIsAttached
    , siName
    , siSupportCode
    , siAttachedTo

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.AlarmState
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import Network.AWS.Lightsail.Types.ComparisonOperator
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
import Network.AWS.Lightsail.Types.InstanceHealthReason
import Network.AWS.Lightsail.Types.InstanceHealthState
import Network.AWS.Lightsail.Types.InstanceMetricName
import Network.AWS.Lightsail.Types.InstancePlatform
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerMetricName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
import Network.AWS.Lightsail.Types.MetricName
import Network.AWS.Lightsail.Types.MetricStatistic
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.PortAccessType
import Network.AWS.Lightsail.Types.PortInfoSourceType
import Network.AWS.Lightsail.Types.PortState
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.RelationalDatabaseEngine
import Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
import Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.TreatMissingData
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.AddOnRequest
import Network.AWS.Lightsail.Types.Alarm
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.Blueprint
import Network.AWS.Lightsail.Types.Bundle
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.ContactMethod
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.DiskInfo
import Network.AWS.Lightsail.Types.DiskMap
import Network.AWS.Lightsail.Types.DiskSnapshot
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.Domain
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.Instance
import Network.AWS.Lightsail.Types.InstanceAccessDetails
import Network.AWS.Lightsail.Types.InstanceEntry
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.InstancePortState
import Network.AWS.Lightsail.Types.InstanceSnapshot
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.KeyPair
import Network.AWS.Lightsail.Types.LoadBalancer
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
import Network.AWS.Lightsail.Types.LogEvent
import Network.AWS.Lightsail.Types.MetricDatapoint
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Lightsail.Types.Operation
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.PortInfo
import Network.AWS.Lightsail.Types.RegionInfo
import Network.AWS.Lightsail.Types.RelationalDatabase
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.StaticIP
import Network.AWS.Lightsail.Types.Tag

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
lightsail :: Service
lightsail
  = Service{_svcAbbrev = "Lightsail", _svcSigner = v4,
            _svcPrefix = "lightsail", _svcVersion = "2016-11-28",
            _svcEndpoint = defaultEndpoint lightsail,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Lightsail",
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

-- | Lightsail throws this exception when an account is still in the setup in progress state.
--
--
_AccountSetupInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountSetupInProgressException
  = _MatchServiceError lightsail
      "AccountSetupInProgressException"

-- | Lightsail throws this exception when the user has not been authenticated.
--
--
_UnauthenticatedException :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthenticatedException
  = _MatchServiceError lightsail
      "UnauthenticatedException"

-- | A general service exception.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException
  = _MatchServiceError lightsail "ServiceException"

-- | Lightsail throws this exception when the user cannot be authenticated or uses invalid credentials to access a resource.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError lightsail
      "AccessDeniedException"

-- | Lightsail throws this exception when user input does not conform to the validation rules of an input field.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError lightsail
      "InvalidInputException"

-- | Lightsail throws this exception when an operation fails to execute.
--
--
_OperationFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationFailureException
  = _MatchServiceError lightsail
      "OperationFailureException"

-- | Lightsail throws this exception when it cannot find a resource.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException
  = _MatchServiceError lightsail "NotFoundException"
