{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Inspector__ 
--
-- Amazon Inspector enables you to analyze the behavior of your AWS resources and to identify potential security issues. For more information, see <https://docs.aws.amazon.com/inspector/latest/userguide/inspector_introduction.html Amazon Inspector User Guide> .
--
module Network.AWS.Inspector
    (
    -- * Service Configuration
      inspector

    -- * Errors
    -- $errors

    -- ** NoSuchEntityException
    , _NoSuchEntityException

    -- ** InvalidCrossAccountRoleException
    , _InvalidCrossAccountRoleException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** PreviewGenerationInProgressException
    , _PreviewGenerationInProgressException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** InternalException
    , _InternalException

    -- ** AssessmentRunInProgressException
    , _AssessmentRunInProgressException

    -- ** AgentsAlreadyRunningAssessmentException
    , _AgentsAlreadyRunningAssessmentException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ServiceTemporarilyUnavailableException
    , _ServiceTemporarilyUnavailableException

    -- ** UnsupportedFeatureException
    , _UnsupportedFeatureException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetTelemetryMetadata 
    , module Network.AWS.Inspector.GetTelemetryMetadata

    -- ** ListFindings (Paginated)
    , module Network.AWS.Inspector.ListFindings

    -- ** ListAssessmentTemplates (Paginated)
    , module Network.AWS.Inspector.ListAssessmentTemplates

    -- ** SubscribeToEvent 
    , module Network.AWS.Inspector.SubscribeToEvent

    -- ** ListAssessmentRunAgents (Paginated)
    , module Network.AWS.Inspector.ListAssessmentRunAgents

    -- ** StartAssessmentRun 
    , module Network.AWS.Inspector.StartAssessmentRun

    -- ** DeleteAssessmentTemplate 
    , module Network.AWS.Inspector.DeleteAssessmentTemplate

    -- ** CreateAssessmentTemplate 
    , module Network.AWS.Inspector.CreateAssessmentTemplate

    -- ** DescribeExclusions 
    , module Network.AWS.Inspector.DescribeExclusions

    -- ** ListTagsForResource 
    , module Network.AWS.Inspector.ListTagsForResource

    -- ** SetTagsForResource 
    , module Network.AWS.Inspector.SetTagsForResource

    -- ** DescribeCrossAccountAccessRole 
    , module Network.AWS.Inspector.DescribeCrossAccountAccessRole

    -- ** DescribeAssessmentTemplates 
    , module Network.AWS.Inspector.DescribeAssessmentTemplates

    -- ** DescribeResourceGroups 
    , module Network.AWS.Inspector.DescribeResourceGroups

    -- ** CreateAssessmentTarget 
    , module Network.AWS.Inspector.CreateAssessmentTarget

    -- ** GetExclusionsPreview 
    , module Network.AWS.Inspector.GetExclusionsPreview

    -- ** ListEventSubscriptions (Paginated)
    , module Network.AWS.Inspector.ListEventSubscriptions

    -- ** RegisterCrossAccountAccessRole 
    , module Network.AWS.Inspector.RegisterCrossAccountAccessRole

    -- ** ListAssessmentTargets (Paginated)
    , module Network.AWS.Inspector.ListAssessmentTargets

    -- ** CreateExclusionsPreview 
    , module Network.AWS.Inspector.CreateExclusionsPreview

    -- ** CreateResourceGroup 
    , module Network.AWS.Inspector.CreateResourceGroup

    -- ** DescribeRulesPackages 
    , module Network.AWS.Inspector.DescribeRulesPackages

    -- ** StopAssessmentRun 
    , module Network.AWS.Inspector.StopAssessmentRun

    -- ** ListExclusions (Paginated)
    , module Network.AWS.Inspector.ListExclusions

    -- ** PreviewAgents (Paginated)
    , module Network.AWS.Inspector.PreviewAgents

    -- ** DescribeFindings 
    , module Network.AWS.Inspector.DescribeFindings

    -- ** AddAttributesToFindings 
    , module Network.AWS.Inspector.AddAttributesToFindings

    -- ** UpdateAssessmentTarget 
    , module Network.AWS.Inspector.UpdateAssessmentTarget

    -- ** DeleteAssessmentTarget 
    , module Network.AWS.Inspector.DeleteAssessmentTarget

    -- ** DeleteAssessmentRun 
    , module Network.AWS.Inspector.DeleteAssessmentRun

    -- ** ListAssessmentRuns (Paginated)
    , module Network.AWS.Inspector.ListAssessmentRuns

    -- ** GetAssessmentReport 
    , module Network.AWS.Inspector.GetAssessmentReport

    -- ** ListRulesPackages (Paginated)
    , module Network.AWS.Inspector.ListRulesPackages

    -- ** DescribeAssessmentRuns 
    , module Network.AWS.Inspector.DescribeAssessmentRuns

    -- ** UnsubscribeFromEvent 
    , module Network.AWS.Inspector.UnsubscribeFromEvent

    -- ** RemoveAttributesFromFindings 
    , module Network.AWS.Inspector.RemoveAttributesFromFindings

    -- ** DescribeAssessmentTargets 
    , module Network.AWS.Inspector.DescribeAssessmentTargets

    -- * Types

    -- ** AgentHealth
    , AgentHealth (..)

    -- ** AgentHealthCode
    , AgentHealthCode (..)

    -- ** AssessmentRunNotificationSNSStatusCode
    , AssessmentRunNotificationSNSStatusCode (..)

    -- ** AssessmentRunState
    , AssessmentRunState (..)

    -- ** AssetType
    , AssetType (..)

    -- ** FailedItemErrorCode
    , FailedItemErrorCode (..)

    -- ** InspectorEvent
    , InspectorEvent (..)

    -- ** Locale
    , Locale (..)

    -- ** PreviewStatus
    , PreviewStatus (..)

    -- ** ReportFileFormat
    , ReportFileFormat (..)

    -- ** ReportStatus
    , ReportStatus (..)

    -- ** ReportType
    , ReportType (..)

    -- ** ScopeType
    , ScopeType (..)

    -- ** Severity
    , Severity (..)

    -- ** StopAction
    , StopAction (..)

    -- ** AgentFilter
    , AgentFilter
    , agentFilter
    , afAgentHealths
    , afAgentHealthCodes

    -- ** AgentPreview
    , AgentPreview
    , agentPreview
    , apHostname
    , apAutoScalingGroup
    , apOperatingSystem
    , apAgentVersion
    , apKernelVersion
    , apAgentHealth
    , apIpv4Address
    , apAgentId

    -- ** AssessmentRun
    , AssessmentRun
    , assessmentRun
    , arStartedAt
    , arCompletedAt
    , arArn
    , arName
    , arAssessmentTemplateARN
    , arState
    , arDurationInSeconds
    , arRulesPackageARNs
    , arUserAttributesForFindings
    , arCreatedAt
    , arStateChangedAt
    , arDataCollected
    , arStateChanges
    , arNotifications
    , arFindingCounts

    -- ** AssessmentRunAgent
    , AssessmentRunAgent
    , assessmentRunAgent
    , araAutoScalingGroup
    , araAgentHealthDetails
    , araAgentId
    , araAssessmentRunARN
    , araAgentHealth
    , araAgentHealthCode
    , araTelemetryMetadata

    -- ** AssessmentRunFilter
    , AssessmentRunFilter
    , assessmentRunFilter
    , arfStates
    , arfNamePattern
    , arfStartTimeRange
    , arfStateChangeTimeRange
    , arfRulesPackageARNs
    , arfCompletionTimeRange
    , arfDurationRange

    -- ** AssessmentRunNotification
    , AssessmentRunNotification
    , assessmentRunNotification
    , arnSnsTopicARN
    , arnSnsPublishStatusCode
    , arnMessage
    , arnDate
    , arnEvent
    , arnError

    -- ** AssessmentRunStateChange
    , AssessmentRunStateChange
    , assessmentRunStateChange
    , arscStateChangedAt
    , arscState

    -- ** AssessmentTarget
    , AssessmentTarget
    , assessmentTarget
    , aResourceGroupARN
    , aArn
    , aName
    , aCreatedAt
    , aUpdatedAt

    -- ** AssessmentTargetFilter
    , AssessmentTargetFilter
    , assessmentTargetFilter
    , atfAssessmentTargetNamePattern

    -- ** AssessmentTemplate
    , AssessmentTemplate
    , assessmentTemplate
    , atLastAssessmentRunARN
    , atArn
    , atName
    , atAssessmentTargetARN
    , atDurationInSeconds
    , atRulesPackageARNs
    , atUserAttributesForFindings
    , atAssessmentRunCount
    , atCreatedAt

    -- ** AssessmentTemplateFilter
    , AssessmentTemplateFilter
    , assessmentTemplateFilter
    , atfNamePattern
    , atfRulesPackageARNs
    , atfDurationRange

    -- ** AssetAttributes
    , AssetAttributes
    , assetAttributes
    , aaHostname
    , aaAutoScalingGroup
    , aaNetworkInterfaces
    , aaIpv4Addresses
    , aaAgentId
    , aaAmiId
    , aaTags
    , aaSchemaVersion

    -- ** Attribute
    , Attribute
    , attribute
    , aValue
    , aKey

    -- ** DurationRange
    , DurationRange
    , durationRange
    , drMinSeconds
    , drMaxSeconds

    -- ** EventSubscription
    , EventSubscription
    , eventSubscription
    , esEvent
    , esSubscribedAt

    -- ** Exclusion
    , Exclusion
    , exclusion
    , eAttributes
    , eArn
    , eTitle
    , eDescription
    , eRecommendation
    , eScopes

    -- ** ExclusionPreview
    , ExclusionPreview
    , exclusionPreview
    , epAttributes
    , epTitle
    , epDescription
    , epRecommendation
    , epScopes

    -- ** FailedItemDetails
    , FailedItemDetails
    , failedItemDetails
    , fidFailureCode
    , fidRetryable

    -- ** Finding
    , Finding
    , finding
    , fService
    , fSeverity
    , fSchemaVersion
    , fConfidence
    , fAssetAttributes
    , fServiceAttributes
    , fId
    , fNumericSeverity
    , fAssetType
    , fTitle
    , fIndicatorOfCompromise
    , fDescription
    , fRecommendation
    , fArn
    , fAttributes
    , fUserAttributes
    , fCreatedAt
    , fUpdatedAt

    -- ** FindingFilter
    , FindingFilter
    , findingFilter
    , ffAgentIds
    , ffRuleNames
    , ffUserAttributes
    , ffRulesPackageARNs
    , ffAttributes
    , ffSeverities
    , ffCreationTimeRange
    , ffAutoScalingGroups

    -- ** InspectorServiceAttributes
    , InspectorServiceAttributes
    , inspectorServiceAttributes
    , isaRulesPackageARN
    , isaAssessmentRunARN
    , isaSchemaVersion

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niPrivateIPAddresses
    , niPublicDNSName
    , niSecurityGroups
    , niVpcId
    , niSubnetId
    , niNetworkInterfaceId
    , niPrivateIPAddress
    , niPublicIP
    , niPrivateDNSName
    , niIpv6Addresses

    -- ** PrivateIP
    , PrivateIP
    , privateIP
    , piPrivateIPAddress
    , piPrivateDNSName

    -- ** ResourceGroup
    , ResourceGroup
    , resourceGroup
    , rgArn
    , rgTags
    , rgCreatedAt

    -- ** ResourceGroupTag
    , ResourceGroupTag
    , resourceGroupTag
    , rgtValue
    , rgtKey

    -- ** RulesPackage
    , RulesPackage
    , rulesPackage
    , rpDescription
    , rpArn
    , rpName
    , rpVersion
    , rpProvider

    -- ** Scope
    , Scope
    , scope
    , sValue
    , sKey

    -- ** SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgGroupId
    , sgGroupName

    -- ** Subscription
    , Subscription
    , subscription
    , sResourceARN
    , sTopicARN
    , sEventSubscriptions

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TelemetryMetadata
    , TelemetryMetadata
    , telemetryMetadata
    , tmDataSize
    , tmMessageType
    , tmCount

    -- ** TimestampRange
    , TimestampRange
    , timestampRange
    , trEndDate
    , trBeginDate
    ) where

import Network.AWS.Inspector.AddAttributesToFindings
import Network.AWS.Inspector.CreateAssessmentTarget
import Network.AWS.Inspector.CreateAssessmentTemplate
import Network.AWS.Inspector.CreateExclusionsPreview
import Network.AWS.Inspector.CreateResourceGroup
import Network.AWS.Inspector.DeleteAssessmentRun
import Network.AWS.Inspector.DeleteAssessmentTarget
import Network.AWS.Inspector.DeleteAssessmentTemplate
import Network.AWS.Inspector.DescribeAssessmentRuns
import Network.AWS.Inspector.DescribeAssessmentTargets
import Network.AWS.Inspector.DescribeAssessmentTemplates
import Network.AWS.Inspector.DescribeCrossAccountAccessRole
import Network.AWS.Inspector.DescribeExclusions
import Network.AWS.Inspector.DescribeFindings
import Network.AWS.Inspector.DescribeResourceGroups
import Network.AWS.Inspector.DescribeRulesPackages
import Network.AWS.Inspector.GetAssessmentReport
import Network.AWS.Inspector.GetExclusionsPreview
import Network.AWS.Inspector.GetTelemetryMetadata
import Network.AWS.Inspector.ListAssessmentRunAgents
import Network.AWS.Inspector.ListAssessmentRuns
import Network.AWS.Inspector.ListAssessmentTargets
import Network.AWS.Inspector.ListAssessmentTemplates
import Network.AWS.Inspector.ListEventSubscriptions
import Network.AWS.Inspector.ListExclusions
import Network.AWS.Inspector.ListFindings
import Network.AWS.Inspector.ListRulesPackages
import Network.AWS.Inspector.ListTagsForResource
import Network.AWS.Inspector.PreviewAgents
import Network.AWS.Inspector.RegisterCrossAccountAccessRole
import Network.AWS.Inspector.RemoveAttributesFromFindings
import Network.AWS.Inspector.SetTagsForResource
import Network.AWS.Inspector.StartAssessmentRun
import Network.AWS.Inspector.StopAssessmentRun
import Network.AWS.Inspector.SubscribeToEvent
import Network.AWS.Inspector.Types
import Network.AWS.Inspector.UnsubscribeFromEvent
import Network.AWS.Inspector.UpdateAssessmentTarget
import Network.AWS.Inspector.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Inspector'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
