{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Product (
    module Network.AWS.Config.Types.AccountAggregationSource,
    module Network.AWS.Config.Types.AggregateComplianceByConfigRule,
    module Network.AWS.Config.Types.AggregateComplianceCount,
    module Network.AWS.Config.Types.AggregateEvaluationResult,
    module Network.AWS.Config.Types.AggregateResourceIdentifier,
    module Network.AWS.Config.Types.AggregatedSourceStatus,
    module Network.AWS.Config.Types.AggregationAuthorization,
    module Network.AWS.Config.Types.BaseConfigurationItem,
    module Network.AWS.Config.Types.Compliance,
    module Network.AWS.Config.Types.ComplianceByConfigRule,
    module Network.AWS.Config.Types.ComplianceByResource,
    module Network.AWS.Config.Types.ComplianceContributorCount,
    module Network.AWS.Config.Types.ComplianceSummary,
    module Network.AWS.Config.Types.ComplianceSummaryByResourceType,
    module Network.AWS.Config.Types.ConfigExportDeliveryInfo,
    module Network.AWS.Config.Types.ConfigRule,
    module Network.AWS.Config.Types.ConfigRuleComplianceFilters,
    module Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters,
    module Network.AWS.Config.Types.ConfigRuleEvaluationStatus,
    module Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties,
    module Network.AWS.Config.Types.ConfigStreamDeliveryInfo,
    module Network.AWS.Config.Types.ConfigurationAggregator,
    module Network.AWS.Config.Types.ConfigurationItem,
    module Network.AWS.Config.Types.ConfigurationRecorder,
    module Network.AWS.Config.Types.ConfigurationRecorderStatus,
    module Network.AWS.Config.Types.ConformancePackComplianceFilters,
    module Network.AWS.Config.Types.ConformancePackComplianceSummary,
    module Network.AWS.Config.Types.ConformancePackDetail,
    module Network.AWS.Config.Types.ConformancePackEvaluationFilters,
    module Network.AWS.Config.Types.ConformancePackEvaluationResult,
    module Network.AWS.Config.Types.ConformancePackInputParameter,
    module Network.AWS.Config.Types.ConformancePackRuleCompliance,
    module Network.AWS.Config.Types.ConformancePackStatusDetail,
    module Network.AWS.Config.Types.DeliveryChannel,
    module Network.AWS.Config.Types.DeliveryChannelStatus,
    module Network.AWS.Config.Types.Evaluation,
    module Network.AWS.Config.Types.EvaluationResult,
    module Network.AWS.Config.Types.EvaluationResultIdentifier,
    module Network.AWS.Config.Types.EvaluationResultQualifier,
    module Network.AWS.Config.Types.ExecutionControls,
    module Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch,
    module Network.AWS.Config.Types.FailedRemediationBatch,
    module Network.AWS.Config.Types.FailedRemediationExceptionBatch,
    module Network.AWS.Config.Types.FieldInfo,
    module Network.AWS.Config.Types.GroupedResourceCount,
    module Network.AWS.Config.Types.MemberAccountStatus,
    module Network.AWS.Config.Types.OrganizationAggregationSource,
    module Network.AWS.Config.Types.OrganizationConfigRule,
    module Network.AWS.Config.Types.OrganizationConfigRuleStatus,
    module Network.AWS.Config.Types.OrganizationConformancePack,
    module Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus,
    module Network.AWS.Config.Types.OrganizationConformancePackStatus,
    module Network.AWS.Config.Types.OrganizationCustomRuleMetadata,
    module Network.AWS.Config.Types.OrganizationManagedRuleMetadata,
    module Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters,
    module Network.AWS.Config.Types.PendingAggregationRequest,
    module Network.AWS.Config.Types.QueryInfo,
    module Network.AWS.Config.Types.RecordingGroup,
    module Network.AWS.Config.Types.Relationship,
    module Network.AWS.Config.Types.RemediationConfiguration,
    module Network.AWS.Config.Types.RemediationException,
    module Network.AWS.Config.Types.RemediationExceptionResourceKey,
    module Network.AWS.Config.Types.RemediationExecutionStatus,
    module Network.AWS.Config.Types.RemediationExecutionStep,
    module Network.AWS.Config.Types.RemediationParameterValue,
    module Network.AWS.Config.Types.ResourceCount,
    module Network.AWS.Config.Types.ResourceCountFilters,
    module Network.AWS.Config.Types.ResourceFilters,
    module Network.AWS.Config.Types.ResourceIdentifier,
    module Network.AWS.Config.Types.ResourceKey,
    module Network.AWS.Config.Types.ResourceValue,
    module Network.AWS.Config.Types.RetentionConfiguration,
    module Network.AWS.Config.Types.Scope,
    module Network.AWS.Config.Types.Source,
    module Network.AWS.Config.Types.SourceDetail,
    module Network.AWS.Config.Types.SsmControls,
    module Network.AWS.Config.Types.StaticValue,
    module Network.AWS.Config.Types.StatusDetailFilters,
    module Network.AWS.Config.Types.Tag
  ) where

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
import Network.AWS.Lens
import Network.AWS.Prelude
