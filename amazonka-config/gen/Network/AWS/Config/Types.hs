{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types
    (
    -- * Service Configuration
      config

    -- * Errors
    , _MaxNumberOfConfigurationRecordersExceededException
    , _InvalidLimitException
    , _OrganizationConformancePackTemplateValidationException
    , _NoSuchRemediationExceptionException
    , _OversizedConfigurationItemException
    , _MaxNumberOfConformancePacksExceededException
    , _InsufficientPermissionsException
    , _OrganizationAllFeaturesNotEnabledException
    , _RemediationInProgressException
    , _ResourceNotDiscoveredException
    , _OrganizationAccessDeniedException
    , _NoSuchConformancePackException
    , _InvalidSNSTopicARNException
    , _InvalidTimeRangeException
    , _NoSuchRemediationConfigurationException
    , _NoSuchDeliveryChannelException
    , _InvalidParameterValueException
    , _NoSuchConfigRuleInConformancePackException
    , _ValidationException
    , _InvalidS3KeyPrefixException
    , _TooManyTagsException
    , _MaxNumberOfOrganizationConformancePacksExceededException
    , _MaxNumberOfOrganizationConfigRulesExceededException
    , _ResourceNotFoundException
    , _InsufficientDeliveryPolicyException
    , _InvalidConfigurationRecorderNameException
    , _NoRunningConfigurationRecorderException
    , _NoAvailableDeliveryChannelException
    , _LastDeliveryChannelDeleteFailedException
    , _ConformancePackTemplateValidationException
    , _NoSuchBucketException
    , _MaxNumberOfConfigRulesExceededException
    , _MaxActiveResourcesExceededException
    , _MaxNumberOfRetentionConfigurationsExceededException
    , _NoAvailableConfigurationRecorderException
    , _NoSuchConfigurationAggregatorException
    , _InvalidNextTokenException
    , _InvalidRoleException
    , _NoSuchConfigRuleException
    , _NoSuchRetentionConfigurationException
    , _InvalidRecordingGroupException
    , _InvalidExpressionException
    , _NoAvailableOrganizationException
    , _NoSuchOrganizationConformancePackException
    , _NoSuchOrganizationConfigRuleException
    , _InvalidResultTokenException
    , _LimitExceededException
    , _ResourceInUseException
    , _NoSuchConfigurationRecorderException
    , _InvalidDeliveryChannelNameException
    , _MaxNumberOfDeliveryChannelsExceededException

    -- * AggregatedSourceStatusType
    , AggregatedSourceStatusType (..)

    -- * AggregatedSourceType
    , AggregatedSourceType (..)

    -- * ChronologicalOrder
    , ChronologicalOrder (..)

    -- * ComplianceType
    , ComplianceType (..)

    -- * ConfigRuleComplianceSummaryGroupKey
    , ConfigRuleComplianceSummaryGroupKey (..)

    -- * ConfigRuleState
    , ConfigRuleState (..)

    -- * ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- * ConformancePackComplianceType
    , ConformancePackComplianceType (..)

    -- * ConformancePackState
    , ConformancePackState (..)

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * EventSource
    , EventSource (..)

    -- * MaximumExecutionFrequency
    , MaximumExecutionFrequency (..)

    -- * MemberAccountRuleStatus
    , MemberAccountRuleStatus (..)

    -- * MessageType
    , MessageType (..)

    -- * OrganizationConfigRuleTriggerType
    , OrganizationConfigRuleTriggerType (..)

    -- * OrganizationResourceDetailedStatus
    , OrganizationResourceDetailedStatus (..)

    -- * OrganizationResourceStatus
    , OrganizationResourceStatus (..)

    -- * OrganizationRuleStatus
    , OrganizationRuleStatus (..)

    -- * Owner
    , Owner (..)

    -- * RecorderStatus
    , RecorderStatus (..)

    -- * RemediationExecutionState
    , RemediationExecutionState (..)

    -- * RemediationExecutionStepState
    , RemediationExecutionStepState (..)

    -- * RemediationTargetType
    , RemediationTargetType (..)

    -- * ResourceCountGroupKey
    , ResourceCountGroupKey (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ResourceValueType
    , ResourceValueType (..)

    -- * AccountAggregationSource
    , AccountAggregationSource
    , accountAggregationSource
    , aasAWSRegions
    , aasAllAWSRegions
    , aasAccountIds

    -- * AggregateComplianceByConfigRule
    , AggregateComplianceByConfigRule
    , aggregateComplianceByConfigRule
    , acbcrCompliance
    , acbcrConfigRuleName
    , acbcrAccountId
    , acbcrAWSRegion

    -- * AggregateComplianceCount
    , AggregateComplianceCount
    , aggregateComplianceCount
    , accGroupName
    , accComplianceSummary

    -- * AggregateEvaluationResult
    , AggregateEvaluationResult
    , aggregateEvaluationResult
    , aerEvaluationResultIdentifier
    , aerAnnotation
    , aerConfigRuleInvokedTime
    , aerResultRecordedTime
    , aerAccountId
    , aerComplianceType
    , aerAWSRegion

    -- * AggregateResourceIdentifier
    , AggregateResourceIdentifier
    , aggregateResourceIdentifier
    , ariResourceName
    , ariSourceAccountId
    , ariSourceRegion
    , ariResourceId
    , ariResourceType

    -- * AggregatedSourceStatus
    , AggregatedSourceStatus
    , aggregatedSourceStatus
    , assLastErrorCode
    , assLastUpdateStatus
    , assSourceType
    , assSourceId
    , assLastErrorMessage
    , assAWSRegion
    , assLastUpdateTime

    -- * AggregationAuthorization
    , AggregationAuthorization
    , aggregationAuthorization
    , aaCreationTime
    , aaAuthorizedAWSRegion
    , aaAggregationAuthorizationARN
    , aaAuthorizedAccountId

    -- * BaseConfigurationItem
    , BaseConfigurationItem
    , baseConfigurationItem
    , bciResourceId
    , bciResourceType
    , bciConfigurationStateId
    , bciArn
    , bciResourceName
    , bciResourceCreationTime
    , bciConfigurationItemStatus
    , bciConfigurationItemCaptureTime
    , bciAccountId
    , bciSupplementaryConfiguration
    , bciAvailabilityZone
    , bciVersion
    , bciAwsRegion
    , bciConfiguration

    -- * Compliance
    , Compliance
    , compliance
    , cComplianceContributorCount
    , cComplianceType

    -- * ComplianceByConfigRule
    , ComplianceByConfigRule
    , complianceByConfigRule
    , cbcrCompliance
    , cbcrConfigRuleName

    -- * ComplianceByResource
    , ComplianceByResource
    , complianceByResource
    , cbrResourceId
    , cbrResourceType
    , cbrCompliance

    -- * ComplianceContributorCount
    , ComplianceContributorCount
    , complianceContributorCount
    , cccCappedCount
    , cccCapExceeded

    -- * ComplianceSummary
    , ComplianceSummary
    , complianceSummary
    , csComplianceSummaryTimestamp
    , csCompliantResourceCount
    , csNonCompliantResourceCount

    -- * ComplianceSummaryByResourceType
    , ComplianceSummaryByResourceType
    , complianceSummaryByResourceType
    , csbrtResourceType
    , csbrtComplianceSummary

    -- * ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastErrorCode
    , cediLastAttemptTime
    , cediLastSuccessfulTime
    , cediLastStatus
    , cediLastErrorMessage
    , cediNextDeliveryTime

    -- * ConfigRule
    , ConfigRule
    , configRule
    , crInputParameters
    , crConfigRuleName
    , crCreatedBy
    , crMaximumExecutionFrequency
    , crConfigRuleId
    , crScope
    , crConfigRuleState
    , crDescription
    , crConfigRuleARN
    , crSource

    -- * ConfigRuleComplianceFilters
    , ConfigRuleComplianceFilters
    , configRuleComplianceFilters
    , crcfConfigRuleName
    , crcfAccountId
    , crcfComplianceType
    , crcfAWSRegion

    -- * ConfigRuleComplianceSummaryFilters
    , ConfigRuleComplianceSummaryFilters
    , configRuleComplianceSummaryFilters
    , crcsfAccountId
    , crcsfAWSRegion

    -- * ConfigRuleEvaluationStatus
    , ConfigRuleEvaluationStatus
    , configRuleEvaluationStatus
    , cresLastErrorCode
    , cresLastFailedEvaluationTime
    , cresFirstActivatedTime
    , cresLastSuccessfulEvaluationTime
    , cresLastDeactivatedTime
    , cresConfigRuleName
    , cresLastErrorMessage
    , cresConfigRuleId
    , cresLastFailedInvocationTime
    , cresFirstEvaluationStarted
    , cresLastSuccessfulInvocationTime
    , cresConfigRuleARN

    -- * ConfigSnapshotDeliveryProperties
    , ConfigSnapshotDeliveryProperties
    , configSnapshotDeliveryProperties
    , csdpDeliveryFrequency

    -- * ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastStatusChangeTime
    , csdiLastStatus
    , csdiLastErrorMessage

    -- * ConfigurationAggregator
    , ConfigurationAggregator
    , configurationAggregator
    , caConfigurationAggregatorARN
    , caCreationTime
    , caOrganizationAggregationSource
    , caLastUpdatedTime
    , caAccountAggregationSources
    , caConfigurationAggregatorName

    -- * ConfigurationItem
    , ConfigurationItem
    , configurationItem
    , ciResourceId
    , ciResourceType
    , ciConfigurationStateId
    , ciArn
    , ciResourceName
    , ciResourceCreationTime
    , ciConfigurationItemStatus
    , ciConfigurationItemCaptureTime
    , ciAccountId
    , ciSupplementaryConfiguration
    , ciAvailabilityZone
    , ciRelationships
    , ciVersion
    , ciAwsRegion
    , ciRelatedEvents
    , ciConfiguration
    , ciConfigurationItemMD5Hash
    , ciTags

    -- * ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- * ConfigurationRecorderStatus
    , ConfigurationRecorderStatus
    , configurationRecorderStatus
    , crsLastErrorCode
    , crsLastStopTime
    , crsLastStatusChangeTime
    , crsRecording
    , crsLastStatus
    , crsLastErrorMessage
    , crsName
    , crsLastStartTime

    -- * ConformancePackComplianceFilters
    , ConformancePackComplianceFilters
    , conformancePackComplianceFilters
    , cpcfConfigRuleNames
    , cpcfComplianceType

    -- * ConformancePackComplianceSummary
    , ConformancePackComplianceSummary
    , conformancePackComplianceSummary
    , cpcsConformancePackName
    , cpcsConformancePackComplianceStatus

    -- * ConformancePackDetail
    , ConformancePackDetail
    , conformancePackDetail
    , cpdDeliveryS3KeyPrefix
    , cpdCreatedBy
    , cpdLastUpdateRequestedTime
    , cpdConformancePackInputParameters
    , cpdConformancePackName
    , cpdConformancePackARN
    , cpdConformancePackId
    , cpdDeliveryS3Bucket

    -- * ConformancePackEvaluationFilters
    , ConformancePackEvaluationFilters
    , conformancePackEvaluationFilters
    , cpefResourceIds
    , cpefResourceType
    , cpefConfigRuleNames
    , cpefComplianceType

    -- * ConformancePackEvaluationResult
    , ConformancePackEvaluationResult
    , conformancePackEvaluationResult
    , cperAnnotation
    , cperComplianceType
    , cperEvaluationResultIdentifier
    , cperConfigRuleInvokedTime
    , cperResultRecordedTime

    -- * ConformancePackInputParameter
    , ConformancePackInputParameter
    , conformancePackInputParameter
    , cpipParameterName
    , cpipParameterValue

    -- * ConformancePackRuleCompliance
    , ConformancePackRuleCompliance
    , conformancePackRuleCompliance
    , cprcConfigRuleName
    , cprcComplianceType

    -- * ConformancePackStatusDetail
    , ConformancePackStatusDetail
    , conformancePackStatusDetail
    , cpsdConformancePackStatusReason
    , cpsdLastUpdateCompletedTime
    , cpsdConformancePackName
    , cpsdConformancePackId
    , cpsdConformancePackARN
    , cpsdConformancePackState
    , cpsdStackARN
    , cpsdLastUpdateRequestedTime

    -- * DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcS3KeyPrefix
    , dcSnsTopicARN
    , dcName
    , dcConfigSnapshotDeliveryProperties
    , dcS3BucketName

    -- * DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
    , dcsConfigHistoryDeliveryInfo
    , dcsName

    -- * Evaluation
    , Evaluation
    , evaluation
    , eAnnotation
    , eComplianceResourceType
    , eComplianceResourceId
    , eComplianceType
    , eOrderingTimestamp

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erEvaluationResultIdentifier
    , erAnnotation
    , erConfigRuleInvokedTime
    , erResultRecordedTime
    , erResultToken
    , erComplianceType

    -- * EvaluationResultIdentifier
    , EvaluationResultIdentifier
    , evaluationResultIdentifier
    , eriEvaluationResultQualifier
    , eriOrderingTimestamp

    -- * EvaluationResultQualifier
    , EvaluationResultQualifier
    , evaluationResultQualifier
    , erqResourceId
    , erqResourceType
    , erqConfigRuleName

    -- * ExecutionControls
    , ExecutionControls
    , executionControls
    , ecSsmControls

    -- * FailedDeleteRemediationExceptionsBatch
    , FailedDeleteRemediationExceptionsBatch
    , failedDeleteRemediationExceptionsBatch
    , fdrebFailureMessage
    , fdrebFailedItems

    -- * FailedRemediationBatch
    , FailedRemediationBatch
    , failedRemediationBatch
    , frbFailureMessage
    , frbFailedItems

    -- * FailedRemediationExceptionBatch
    , FailedRemediationExceptionBatch
    , failedRemediationExceptionBatch
    , frebFailureMessage
    , frebFailedItems

    -- * FieldInfo
    , FieldInfo
    , fieldInfo
    , fiName

    -- * GroupedResourceCount
    , GroupedResourceCount
    , groupedResourceCount
    , grcGroupName
    , grcResourceCount

    -- * MemberAccountStatus
    , MemberAccountStatus
    , memberAccountStatus
    , masErrorCode
    , masErrorMessage
    , masLastUpdateTime
    , masAccountId
    , masConfigRuleName
    , masMemberAccountRuleStatus

    -- * OrganizationAggregationSource
    , OrganizationAggregationSource
    , organizationAggregationSource
    , oasAWSRegions
    , oasAllAWSRegions
    , oasRoleARN

    -- * OrganizationConfigRule
    , OrganizationConfigRule
    , organizationConfigRule
    , ocrOrganizationManagedRuleMetadata
    , ocrExcludedAccounts
    , ocrOrganizationCustomRuleMetadata
    , ocrLastUpdateTime
    , ocrOrganizationConfigRuleName
    , ocrOrganizationConfigRuleARN

    -- * OrganizationConfigRuleStatus
    , OrganizationConfigRuleStatus
    , organizationConfigRuleStatus
    , ocrsErrorCode
    , ocrsErrorMessage
    , ocrsLastUpdateTime
    , ocrsOrganizationConfigRuleName
    , ocrsOrganizationRuleStatus

    -- * OrganizationConformancePack
    , OrganizationConformancePack
    , organizationConformancePack
    , ocpDeliveryS3KeyPrefix
    , ocpConformancePackInputParameters
    , ocpExcludedAccounts
    , ocpOrganizationConformancePackName
    , ocpOrganizationConformancePackARN
    , ocpDeliveryS3Bucket
    , ocpLastUpdateTime

    -- * OrganizationConformancePackDetailedStatus
    , OrganizationConformancePackDetailedStatus
    , organizationConformancePackDetailedStatus
    , ocpdsErrorCode
    , ocpdsErrorMessage
    , ocpdsLastUpdateTime
    , ocpdsAccountId
    , ocpdsConformancePackName
    , ocpdsStatus

    -- * OrganizationConformancePackStatus
    , OrganizationConformancePackStatus
    , organizationConformancePackStatus
    , ocpsErrorCode
    , ocpsErrorMessage
    , ocpsLastUpdateTime
    , ocpsOrganizationConformancePackName
    , ocpsStatus

    -- * OrganizationCustomRuleMetadata
    , OrganizationCustomRuleMetadata
    , organizationCustomRuleMetadata
    , ocrmInputParameters
    , ocrmResourceIdScope
    , ocrmTagValueScope
    , ocrmMaximumExecutionFrequency
    , ocrmTagKeyScope
    , ocrmResourceTypesScope
    , ocrmDescription
    , ocrmLambdaFunctionARN
    , ocrmOrganizationConfigRuleTriggerTypes

    -- * OrganizationManagedRuleMetadata
    , OrganizationManagedRuleMetadata
    , organizationManagedRuleMetadata
    , omrmInputParameters
    , omrmResourceIdScope
    , omrmTagValueScope
    , omrmMaximumExecutionFrequency
    , omrmTagKeyScope
    , omrmResourceTypesScope
    , omrmDescription
    , omrmRuleIdentifier

    -- * OrganizationResourceDetailedStatusFilters
    , OrganizationResourceDetailedStatusFilters
    , organizationResourceDetailedStatusFilters
    , ordsfStatus
    , ordsfAccountId

    -- * PendingAggregationRequest
    , PendingAggregationRequest
    , pendingAggregationRequest
    , parRequesterAccountId
    , parRequesterAWSRegion

    -- * QueryInfo
    , QueryInfo
    , queryInfo
    , qiSelectFields

    -- * RecordingGroup
    , RecordingGroup
    , recordingGroup
    , rgAllSupported
    , rgIncludeGlobalResourceTypes
    , rgResourceTypes

    -- * Relationship
    , Relationship
    , relationship
    , rResourceId
    , rResourceType
    , rResourceName
    , rRelationshipName

    -- * RemediationConfiguration
    , RemediationConfiguration
    , remediationConfiguration
    , rcResourceType
    , rcARN
    , rcAutomatic
    , rcCreatedByService
    , rcRetryAttemptSeconds
    , rcExecutionControls
    , rcParameters
    , rcMaximumAutomaticAttempts
    , rcTargetVersion
    , rcConfigRuleName
    , rcTargetType
    , rcTargetId

    -- * RemediationException
    , RemediationException
    , remediationException
    , reMessage
    , reExpirationTime
    , reConfigRuleName
    , reResourceType
    , reResourceId

    -- * RemediationExceptionResourceKey
    , RemediationExceptionResourceKey
    , remediationExceptionResourceKey
    , rerkResourceId
    , rerkResourceType

    -- * RemediationExecutionStatus
    , RemediationExecutionStatus
    , remediationExecutionStatus
    , rState
    , rLastUpdatedTime
    , rResourceKey
    , rStepDetails
    , rInvocationTime

    -- * RemediationExecutionStep
    , RemediationExecutionStep
    , remediationExecutionStep
    , resState
    , resStartTime
    , resName
    , resStopTime
    , resErrorMessage

    -- * RemediationParameterValue
    , RemediationParameterValue
    , remediationParameterValue
    , rpvStaticValue
    , rpvResourceValue

    -- * ResourceCount
    , ResourceCount
    , resourceCount
    , resResourceType
    , resCount

    -- * ResourceCountFilters
    , ResourceCountFilters
    , resourceCountFilters
    , rcfResourceType
    , rcfAccountId
    , rcfRegion

    -- * ResourceFilters
    , ResourceFilters
    , resourceFilters
    , rfResourceId
    , rfResourceName
    , rfAccountId
    , rfRegion

    -- * ResourceIdentifier
    , ResourceIdentifier
    , resourceIdentifier
    , riResourceId
    , riResourceType
    , riResourceName
    , riResourceDeletionTime

    -- * ResourceKey
    , ResourceKey
    , resourceKey
    , rkResourceType
    , rkResourceId

    -- * ResourceValue
    , ResourceValue
    , resourceValue
    , rvValue

    -- * RetentionConfiguration
    , RetentionConfiguration
    , retentionConfiguration
    , rcName
    , rcRetentionPeriodInDays

    -- * Scope
    , Scope
    , scope
    , sComplianceResourceTypes
    , sComplianceResourceId
    , sTagValue
    , sTagKey

    -- * Source
    , Source
    , source
    , sSourceDetails
    , sOwner
    , sSourceIdentifier

    -- * SourceDetail
    , SourceDetail
    , sourceDetail
    , sdMessageType
    , sdMaximumExecutionFrequency
    , sdEventSource

    -- * SsmControls
    , SsmControls
    , ssmControls
    , scConcurrentExecutionRatePercentage
    , scErrorPercentage

    -- * StaticValue
    , StaticValue
    , staticValue
    , svValues

    -- * StatusDetailFilters
    , StatusDetailFilters
    , statusDetailFilters
    , sdfMemberAccountRuleStatus
    , sdfAccountId

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Config.Types.AggregatedSourceStatusType
import Network.AWS.Config.Types.AggregatedSourceType
import Network.AWS.Config.Types.ChronologicalOrder
import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryGroupKey
import Network.AWS.Config.Types.ConfigRuleState
import Network.AWS.Config.Types.ConfigurationItemStatus
import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Config.Types.ConformancePackState
import Network.AWS.Config.Types.DeliveryStatus
import Network.AWS.Config.Types.EventSource
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.MemberAccountRuleStatus
import Network.AWS.Config.Types.MessageType
import Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import Network.AWS.Config.Types.OrganizationResourceStatus
import Network.AWS.Config.Types.OrganizationRuleStatus
import Network.AWS.Config.Types.Owner
import Network.AWS.Config.Types.RecorderStatus
import Network.AWS.Config.Types.RemediationExecutionState
import Network.AWS.Config.Types.RemediationExecutionStepState
import Network.AWS.Config.Types.RemediationTargetType
import Network.AWS.Config.Types.ResourceCountGroupKey
import Network.AWS.Config.Types.ResourceType
import Network.AWS.Config.Types.ResourceValueType
import Network.AWS.Config.Types.AccountAggregationSource
import Network.AWS.Config.Types.AggregateComplianceByConfigRule
import Network.AWS.Config.Types.AggregateComplianceCount
import Network.AWS.Config.Types.AggregateEvaluationResult
import Network.AWS.Config.Types.AggregateResourceIdentifier
import Network.AWS.Config.Types.AggregatedSourceStatus
import Network.AWS.Config.Types.AggregationAuthorization
import Network.AWS.Config.Types.BaseConfigurationItem
import Network.AWS.Config.Types.Compliance
import Network.AWS.Config.Types.ComplianceByConfigRule
import Network.AWS.Config.Types.ComplianceByResource
import Network.AWS.Config.Types.ComplianceContributorCount
import Network.AWS.Config.Types.ComplianceSummary
import Network.AWS.Config.Types.ComplianceSummaryByResourceType
import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigRule
import Network.AWS.Config.Types.ConfigRuleComplianceFilters
import Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
import Network.AWS.Config.Types.ConfigRuleEvaluationStatus
import Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import Network.AWS.Config.Types.ConfigurationAggregator
import Network.AWS.Config.Types.ConfigurationItem
import Network.AWS.Config.Types.ConfigurationRecorder
import Network.AWS.Config.Types.ConfigurationRecorderStatus
import Network.AWS.Config.Types.ConformancePackComplianceFilters
import Network.AWS.Config.Types.ConformancePackComplianceSummary
import Network.AWS.Config.Types.ConformancePackDetail
import Network.AWS.Config.Types.ConformancePackEvaluationFilters
import Network.AWS.Config.Types.ConformancePackEvaluationResult
import Network.AWS.Config.Types.ConformancePackInputParameter
import Network.AWS.Config.Types.ConformancePackRuleCompliance
import Network.AWS.Config.Types.ConformancePackStatusDetail
import Network.AWS.Config.Types.DeliveryChannel
import Network.AWS.Config.Types.DeliveryChannelStatus
import Network.AWS.Config.Types.Evaluation
import Network.AWS.Config.Types.EvaluationResult
import Network.AWS.Config.Types.EvaluationResultIdentifier
import Network.AWS.Config.Types.EvaluationResultQualifier
import Network.AWS.Config.Types.ExecutionControls
import Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
import Network.AWS.Config.Types.FailedRemediationBatch
import Network.AWS.Config.Types.FailedRemediationExceptionBatch
import Network.AWS.Config.Types.FieldInfo
import Network.AWS.Config.Types.GroupedResourceCount
import Network.AWS.Config.Types.MemberAccountStatus
import Network.AWS.Config.Types.OrganizationAggregationSource
import Network.AWS.Config.Types.OrganizationConfigRule
import Network.AWS.Config.Types.OrganizationConfigRuleStatus
import Network.AWS.Config.Types.OrganizationConformancePack
import Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
import Network.AWS.Config.Types.OrganizationConformancePackStatus
import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
import Network.AWS.Config.Types.PendingAggregationRequest
import Network.AWS.Config.Types.QueryInfo
import Network.AWS.Config.Types.RecordingGroup
import Network.AWS.Config.Types.Relationship
import Network.AWS.Config.Types.RemediationConfiguration
import Network.AWS.Config.Types.RemediationException
import Network.AWS.Config.Types.RemediationExceptionResourceKey
import Network.AWS.Config.Types.RemediationExecutionStatus
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.RemediationParameterValue
import Network.AWS.Config.Types.ResourceCount
import Network.AWS.Config.Types.ResourceCountFilters
import Network.AWS.Config.Types.ResourceFilters
import Network.AWS.Config.Types.ResourceIdentifier
import Network.AWS.Config.Types.ResourceKey
import Network.AWS.Config.Types.ResourceValue
import Network.AWS.Config.Types.RetentionConfiguration
import Network.AWS.Config.Types.Scope
import Network.AWS.Config.Types.Source
import Network.AWS.Config.Types.SourceDetail
import Network.AWS.Config.Types.SsmControls
import Network.AWS.Config.Types.StaticValue
import Network.AWS.Config.Types.StatusDetailFilters
import Network.AWS.Config.Types.Tag

-- | API version @2014-11-12@ of the Amazon Config SDK configuration.
config :: Service
config
  = Service{_svcAbbrev = "Config", _svcSigner = v4,
            _svcPrefix = "config", _svcVersion = "2014-11-12",
            _svcEndpoint = defaultEndpoint config,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Config",
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

-- | You have reached the limit of the number of recorders you can create.
--
--
_MaxNumberOfConfigurationRecordersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigurationRecordersExceededException
  = _MatchServiceError config
      "MaxNumberOfConfigurationRecordersExceededException"

-- | The specified limit is outside the allowable range.
--
--
_InvalidLimitException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLimitException
  = _MatchServiceError config "InvalidLimitException"

-- | You have specified a template that is not valid or supported.
--
--
_OrganizationConformancePackTemplateValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationConformancePackTemplateValidationException
  = _MatchServiceError config
      "OrganizationConformancePackTemplateValidationException"

-- | You tried to delete a remediation exception that does not exist.
--
--
_NoSuchRemediationExceptionException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchRemediationExceptionException
  = _MatchServiceError config
      "NoSuchRemediationExceptionException"

-- | The configuration item size is outside the allowable range.
--
--
_OversizedConfigurationItemException :: AsError a => Getting (First ServiceError) a ServiceError
_OversizedConfigurationItemException
  = _MatchServiceError config
      "OversizedConfigurationItemException"

-- | You have reached the limit (6) of the number of conformance packs in an account (6 conformance pack with 25 AWS Config rules per pack).
--
--
_MaxNumberOfConformancePacksExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConformancePacksExceededException
  = _MatchServiceError config
      "MaxNumberOfConformancePacksExceededException"

-- | Indicates one of the following errors:
--
--
--     * For PutConfigRule, the rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.
--
--     * For PutConfigRule, the AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.
--
--     * For PutOrganizationConfigRule, organization config rule cannot be created because you do not have permissions to call IAM @GetRole@ action or create a service linked role.
--
--     * For PutConformancePack and PutOrganizationConformancePack, a conformance pack cannot be created because you do not have permissions: 
--
--     * To call IAM @GetRole@ action or create a service linked role.
--
--     * To read Amazon S3 bucket.
--
--
--
--
--
_InsufficientPermissionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientPermissionsException
  = _MatchServiceError config
      "InsufficientPermissionsException"

-- | AWS Config resource cannot be created because your organization does not have all features enabled.
--
--
_OrganizationAllFeaturesNotEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationAllFeaturesNotEnabledException
  = _MatchServiceError config
      "OrganizationAllFeaturesNotEnabledException"

-- | Remediation action is in progress. You can either cancel execution in AWS Systems Manager or wait and try again later. 
--
--
_RemediationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_RemediationInProgressException
  = _MatchServiceError config
      "RemediationInProgressException"

-- | You have specified a resource that is either unknown or has not been discovered.
--
--
_ResourceNotDiscoveredException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotDiscoveredException
  = _MatchServiceError config
      "ResourceNotDiscoveredException"

-- | For PutConfigAggregator API, no permission to call EnableAWSServiceAccess API.
--
--
-- For all OrganizationConfigRule and OrganizationConformancePack APIs, AWS Config throws an exception if APIs are called from member accounts. All APIs must be called from organization master account.
--
_OrganizationAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationAccessDeniedException
  = _MatchServiceError config
      "OrganizationAccessDeniedException"

-- | You specified one or more conformance packs that do not exist.
--
--
_NoSuchConformancePackException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConformancePackException
  = _MatchServiceError config
      "NoSuchConformancePackException"

-- | The specified Amazon SNS topic does not exist.
--
--
_InvalidSNSTopicARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException
  = _MatchServiceError config
      "InvalidSNSTopicARNException"

-- | The specified time range is not valid. The earlier time is not chronologically before the later time.
--
--
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException
  = _MatchServiceError config
      "InvalidTimeRangeException"

-- | You specified an AWS Config rule without a remediation configuration.
--
--
_NoSuchRemediationConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchRemediationConfigurationException
  = _MatchServiceError config
      "NoSuchRemediationConfigurationException"

-- | You have specified a delivery channel that does not exist.
--
--
_NoSuchDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDeliveryChannelException
  = _MatchServiceError config
      "NoSuchDeliveryChannelException"

-- | One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException
  = _MatchServiceError config
      "InvalidParameterValueException"

-- | AWS Config rule that you passed in the filter does not exist.
--
--
_NoSuchConfigRuleInConformancePackException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigRuleInConformancePackException
  = _MatchServiceError config
      "NoSuchConfigRuleInConformancePackException"

-- | The requested action is not valid.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException
  = _MatchServiceError config "ValidationException"

-- | The specified Amazon S3 key prefix is not valid.
--
--
_InvalidS3KeyPrefixException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixException
  = _MatchServiceError config
      "InvalidS3KeyPrefixException"

-- | You have reached the limit of the number of tags you can use. You have more than 50 tags.
--
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException
  = _MatchServiceError config "TooManyTagsException"

-- | You have reached the limit (6) of the number of organization conformance packs in an account (6 conformance pack with 25 AWS Config rules per pack per account).
--
--
_MaxNumberOfOrganizationConformancePacksExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfOrganizationConformancePacksExceededException
  = _MatchServiceError config
      "MaxNumberOfOrganizationConformancePacksExceededException"

-- | You have reached the limit of the number of organization config rules you can create.
--
--
_MaxNumberOfOrganizationConfigRulesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfOrganizationConfigRulesExceededException
  = _MatchServiceError config
      "MaxNumberOfOrganizationConfigRulesExceededException"

-- | You have specified a resource that does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError config
      "ResourceNotFoundException"

-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
--
--
_InsufficientDeliveryPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDeliveryPolicyException
  = _MatchServiceError config
      "InsufficientDeliveryPolicyException"

-- | You have provided a configuration recorder name that is not valid.
--
--
_InvalidConfigurationRecorderNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRecorderNameException
  = _MatchServiceError config
      "InvalidConfigurationRecorderNameException"

-- | There is no configuration recorder running.
--
--
_NoRunningConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoRunningConfigurationRecorderException
  = _MatchServiceError config
      "NoRunningConfigurationRecorderException"

-- | There is no delivery channel available to record configurations.
--
--
_NoAvailableDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableDeliveryChannelException
  = _MatchServiceError config
      "NoAvailableDeliveryChannelException"

-- | You cannot delete the delivery channel you specified because the configuration recorder is running.
--
--
_LastDeliveryChannelDeleteFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_LastDeliveryChannelDeleteFailedException
  = _MatchServiceError config
      "LastDeliveryChannelDeleteFailedException"

-- | You have specified a template that is not valid or supported.
--
--
_ConformancePackTemplateValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConformancePackTemplateValidationException
  = _MatchServiceError config
      "ConformancePackTemplateValidationException"

-- | The specified Amazon S3 bucket does not exist.
--
--
_NoSuchBucketException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException
  = _MatchServiceError config "NoSuchBucketException"

-- | Failed to add the AWS Config rule because the account already contains the maximum number of 150 rules. Consider deleting any deactivated rules before you add new rules.
--
--
_MaxNumberOfConfigRulesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigRulesExceededException
  = _MatchServiceError config
      "MaxNumberOfConfigRulesExceededException"

-- | You have reached the limit (100,000) of active custom resource types in your account. Delete unused resources using @DeleteResourceConfig@ .
--
--
_MaxActiveResourcesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxActiveResourcesExceededException
  = _MatchServiceError config
      "MaxActiveResourcesExceededException"

-- | Failed to add the retention configuration because a retention configuration with that name already exists.
--
--
_MaxNumberOfRetentionConfigurationsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfRetentionConfigurationsExceededException
  = _MatchServiceError config
      "MaxNumberOfRetentionConfigurationsExceededException"

-- | There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.
--
--
_NoAvailableConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableConfigurationRecorderException
  = _MatchServiceError config
      "NoAvailableConfigurationRecorderException"

-- | You have specified a configuration aggregator that does not exist.
--
--
_NoSuchConfigurationAggregatorException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigurationAggregatorException
  = _MatchServiceError config
      "NoSuchConfigurationAggregatorException"

-- | The specified next token is invalid. Specify the @nextToken@ string that was returned in the previous response to get the next page of results.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException
  = _MatchServiceError config
      "InvalidNextTokenException"

-- | You have provided a null or empty role ARN.
--
--
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException
  = _MatchServiceError config "InvalidRoleException"

-- | One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.
--
--
_NoSuchConfigRuleException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigRuleException
  = _MatchServiceError config
      "NoSuchConfigRuleException"

-- | You have specified a retention configuration that does not exist.
--
--
_NoSuchRetentionConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchRetentionConfigurationException
  = _MatchServiceError config
      "NoSuchRetentionConfigurationException"

-- | AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values might also be incorrectly formatted.
--
--
_InvalidRecordingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRecordingGroupException
  = _MatchServiceError config
      "InvalidRecordingGroupException"

-- | The syntax of the query is incorrect.
--
--
_InvalidExpressionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidExpressionException
  = _MatchServiceError config
      "InvalidExpressionException"

-- | Organization is no longer available.
--
--
_NoAvailableOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableOrganizationException
  = _MatchServiceError config
      "NoAvailableOrganizationException"

-- | AWS Config organization conformance pack that you passed in the filter does not exist.
--
--
-- For DeleteOrganizationConformancePack, you tried to delete an organization conformance pack that does not exist.
--
_NoSuchOrganizationConformancePackException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchOrganizationConformancePackException
  = _MatchServiceError config
      "NoSuchOrganizationConformancePackException"

-- | You specified one or more organization config rules that do not exist.
--
--
_NoSuchOrganizationConfigRuleException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchOrganizationConfigRuleException
  = _MatchServiceError config
      "NoSuchOrganizationConfigRuleException"

-- | The specified @ResultToken@ is invalid.
--
--
_InvalidResultTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResultTokenException
  = _MatchServiceError config
      "InvalidResultTokenException"

-- | For @StartConfigRulesEvaluation@ API, this exception is thrown if an evaluation is in progress or if you call the 'StartConfigRulesEvaluation' API more than once per minute.
--
--
-- For @PutConfigurationAggregator@ API, this exception is thrown if the number of accounts and aggregators exceeds the limit.
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError config "LimitExceededException"

-- | You see this exception in the following cases: 
--
--
--     * For DeleteConfigRule, AWS Config is deleting this rule. Try your request again later.
--
--     * For DeleteConfigRule, the rule is deleting your evaluation results. Try your request again later.
--
--     * For DeleteConfigRule, a remediation action is associated with the rule and AWS Config cannot delete this rule. Delete the remediation action associated with the rule before deleting the rule and try your request again later.
--
--     * For PutConfigOrganizationRule, organization config rule deletion is in progress. Try your request again later.
--
--     * For DeleteOrganizationConfigRule, organization config rule creation is in progress. Try your request again later.
--
--     * For PutConformancePack and PutOrganizationConformancePack, a conformance pack creation, update, and deletion is in progress. Try your request again later.
--
--     * For DeleteConformancePack, a conformance pack creation, update, and deletion is in progress. Try your request again later.
--
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError config "ResourceInUseException"

-- | You have specified a configuration recorder that does not exist.
--
--
_NoSuchConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigurationRecorderException
  = _MatchServiceError config
      "NoSuchConfigurationRecorderException"

-- | The specified delivery channel name is not valid.
--
--
_InvalidDeliveryChannelNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeliveryChannelNameException
  = _MatchServiceError config
      "InvalidDeliveryChannelNameException"

-- | You have reached the limit of the number of delivery channels you can create.
--
--
_MaxNumberOfDeliveryChannelsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfDeliveryChannelsExceededException
  = _MatchServiceError config
      "MaxNumberOfDeliveryChannelsExceededException"
