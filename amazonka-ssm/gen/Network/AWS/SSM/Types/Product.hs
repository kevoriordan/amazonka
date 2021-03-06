{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Product (
    module Network.AWS.SSM.Types.AccountSharingInfo,
    module Network.AWS.SSM.Types.Activation,
    module Network.AWS.SSM.Types.Association,
    module Network.AWS.SSM.Types.AssociationDescription,
    module Network.AWS.SSM.Types.AssociationExecution,
    module Network.AWS.SSM.Types.AssociationExecutionFilter,
    module Network.AWS.SSM.Types.AssociationExecutionTarget,
    module Network.AWS.SSM.Types.AssociationExecutionTargetsFilter,
    module Network.AWS.SSM.Types.AssociationFilter,
    module Network.AWS.SSM.Types.AssociationOverview,
    module Network.AWS.SSM.Types.AssociationStatus,
    module Network.AWS.SSM.Types.AssociationVersionInfo,
    module Network.AWS.SSM.Types.AttachmentContent,
    module Network.AWS.SSM.Types.AttachmentInformation,
    module Network.AWS.SSM.Types.AttachmentsSource,
    module Network.AWS.SSM.Types.AutomationExecution,
    module Network.AWS.SSM.Types.AutomationExecutionFilter,
    module Network.AWS.SSM.Types.AutomationExecutionMetadata,
    module Network.AWS.SSM.Types.CloudWatchOutputConfig,
    module Network.AWS.SSM.Types.Command,
    module Network.AWS.SSM.Types.CommandFilter,
    module Network.AWS.SSM.Types.CommandInvocation,
    module Network.AWS.SSM.Types.CommandPlugin,
    module Network.AWS.SSM.Types.ComplianceExecutionSummary,
    module Network.AWS.SSM.Types.ComplianceItem,
    module Network.AWS.SSM.Types.ComplianceItemEntry,
    module Network.AWS.SSM.Types.ComplianceStringFilter,
    module Network.AWS.SSM.Types.ComplianceSummaryItem,
    module Network.AWS.SSM.Types.CompliantSummary,
    module Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry,
    module Network.AWS.SSM.Types.DescribeActivationsFilter,
    module Network.AWS.SSM.Types.DocumentDefaultVersionDescription,
    module Network.AWS.SSM.Types.DocumentDescription,
    module Network.AWS.SSM.Types.DocumentFilter,
    module Network.AWS.SSM.Types.DocumentIdentifier,
    module Network.AWS.SSM.Types.DocumentKeyValuesFilter,
    module Network.AWS.SSM.Types.DocumentParameter,
    module Network.AWS.SSM.Types.DocumentRequires,
    module Network.AWS.SSM.Types.DocumentVersionInfo,
    module Network.AWS.SSM.Types.EffectivePatch,
    module Network.AWS.SSM.Types.FailedCreateAssociation,
    module Network.AWS.SSM.Types.FailureDetails,
    module Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview,
    module Network.AWS.SSM.Types.InstanceAssociation,
    module Network.AWS.SSM.Types.InstanceAssociationOutputLocation,
    module Network.AWS.SSM.Types.InstanceAssociationOutputURL,
    module Network.AWS.SSM.Types.InstanceAssociationStatusInfo,
    module Network.AWS.SSM.Types.InstanceInformation,
    module Network.AWS.SSM.Types.InstanceInformationFilter,
    module Network.AWS.SSM.Types.InstanceInformationStringFilter,
    module Network.AWS.SSM.Types.InstancePatchState,
    module Network.AWS.SSM.Types.InstancePatchStateFilter,
    module Network.AWS.SSM.Types.InventoryAggregator,
    module Network.AWS.SSM.Types.InventoryDeletionStatusItem,
    module Network.AWS.SSM.Types.InventoryDeletionSummary,
    module Network.AWS.SSM.Types.InventoryDeletionSummaryItem,
    module Network.AWS.SSM.Types.InventoryFilter,
    module Network.AWS.SSM.Types.InventoryGroup,
    module Network.AWS.SSM.Types.InventoryItem,
    module Network.AWS.SSM.Types.InventoryItemAttribute,
    module Network.AWS.SSM.Types.InventoryItemSchema,
    module Network.AWS.SSM.Types.InventoryResultEntity,
    module Network.AWS.SSM.Types.InventoryResultItem,
    module Network.AWS.SSM.Types.LoggingInfo,
    module Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters,
    module Network.AWS.SSM.Types.MaintenanceWindowExecution,
    module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity,
    module Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity,
    module Network.AWS.SSM.Types.MaintenanceWindowFilter,
    module Network.AWS.SSM.Types.MaintenanceWindowIdentity,
    module Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget,
    module Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters,
    module Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters,
    module Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters,
    module Network.AWS.SSM.Types.MaintenanceWindowTarget,
    module Network.AWS.SSM.Types.MaintenanceWindowTask,
    module Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters,
    module Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression,
    module Network.AWS.SSM.Types.NonCompliantSummary,
    module Network.AWS.SSM.Types.NotificationConfig,
    module Network.AWS.SSM.Types.OpsAggregator,
    module Network.AWS.SSM.Types.OpsEntity,
    module Network.AWS.SSM.Types.OpsEntityItem,
    module Network.AWS.SSM.Types.OpsFilter,
    module Network.AWS.SSM.Types.OpsItem,
    module Network.AWS.SSM.Types.OpsItemDataValue,
    module Network.AWS.SSM.Types.OpsItemFilter,
    module Network.AWS.SSM.Types.OpsItemNotification,
    module Network.AWS.SSM.Types.OpsItemSummary,
    module Network.AWS.SSM.Types.OpsResultAttribute,
    module Network.AWS.SSM.Types.OutputSource,
    module Network.AWS.SSM.Types.Parameter,
    module Network.AWS.SSM.Types.ParameterHistory,
    module Network.AWS.SSM.Types.ParameterInlinePolicy,
    module Network.AWS.SSM.Types.ParameterMetadata,
    module Network.AWS.SSM.Types.ParameterStringFilter,
    module Network.AWS.SSM.Types.ParametersFilter,
    module Network.AWS.SSM.Types.Patch,
    module Network.AWS.SSM.Types.PatchBaselineIdentity,
    module Network.AWS.SSM.Types.PatchComplianceData,
    module Network.AWS.SSM.Types.PatchFilter,
    module Network.AWS.SSM.Types.PatchFilterGroup,
    module Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping,
    module Network.AWS.SSM.Types.PatchOrchestratorFilter,
    module Network.AWS.SSM.Types.PatchRule,
    module Network.AWS.SSM.Types.PatchRuleGroup,
    module Network.AWS.SSM.Types.PatchSource,
    module Network.AWS.SSM.Types.PatchStatus,
    module Network.AWS.SSM.Types.ProgressCounters,
    module Network.AWS.SSM.Types.RelatedOpsItem,
    module Network.AWS.SSM.Types.ResolvedTargets,
    module Network.AWS.SSM.Types.ResourceComplianceSummaryItem,
    module Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource,
    module Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing,
    module Network.AWS.SSM.Types.ResourceDataSyncItem,
    module Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit,
    module Network.AWS.SSM.Types.ResourceDataSyncS3Destination,
    module Network.AWS.SSM.Types.ResourceDataSyncSource,
    module Network.AWS.SSM.Types.ResourceDataSyncSourceWithState,
    module Network.AWS.SSM.Types.ResultAttribute,
    module Network.AWS.SSM.Types.S3OutputLocation,
    module Network.AWS.SSM.Types.S3OutputURL,
    module Network.AWS.SSM.Types.ScheduledWindowExecution,
    module Network.AWS.SSM.Types.ServiceSetting,
    module Network.AWS.SSM.Types.Session,
    module Network.AWS.SSM.Types.SessionFilter,
    module Network.AWS.SSM.Types.SessionManagerOutputURL,
    module Network.AWS.SSM.Types.SeveritySummary,
    module Network.AWS.SSM.Types.StepExecution,
    module Network.AWS.SSM.Types.StepExecutionFilter,
    module Network.AWS.SSM.Types.Tag,
    module Network.AWS.SSM.Types.Target,
    module Network.AWS.SSM.Types.TargetLocation
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AccountSharingInfo
import Network.AWS.SSM.Types.Activation
import Network.AWS.SSM.Types.Association
import Network.AWS.SSM.Types.AssociationDescription
import Network.AWS.SSM.Types.AssociationExecution
import Network.AWS.SSM.Types.AssociationExecutionFilter
import Network.AWS.SSM.Types.AssociationExecutionTarget
import Network.AWS.SSM.Types.AssociationExecutionTargetsFilter
import Network.AWS.SSM.Types.AssociationFilter
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.AssociationStatus
import Network.AWS.SSM.Types.AssociationVersionInfo
import Network.AWS.SSM.Types.AttachmentContent
import Network.AWS.SSM.Types.AttachmentInformation
import Network.AWS.SSM.Types.AttachmentsSource
import Network.AWS.SSM.Types.AutomationExecution
import Network.AWS.SSM.Types.AutomationExecutionFilter
import Network.AWS.SSM.Types.AutomationExecutionMetadata
import Network.AWS.SSM.Types.CloudWatchOutputConfig
import Network.AWS.SSM.Types.Command
import Network.AWS.SSM.Types.CommandFilter
import Network.AWS.SSM.Types.CommandInvocation
import Network.AWS.SSM.Types.CommandPlugin
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceItem
import Network.AWS.SSM.Types.ComplianceItemEntry
import Network.AWS.SSM.Types.ComplianceStringFilter
import Network.AWS.SSM.Types.ComplianceSummaryItem
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.DescribeActivationsFilter
import Network.AWS.SSM.Types.DocumentDefaultVersionDescription
import Network.AWS.SSM.Types.DocumentDescription
import Network.AWS.SSM.Types.DocumentFilter
import Network.AWS.SSM.Types.DocumentIdentifier
import Network.AWS.SSM.Types.DocumentKeyValuesFilter
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentRequires
import Network.AWS.SSM.Types.DocumentVersionInfo
import Network.AWS.SSM.Types.EffectivePatch
import Network.AWS.SSM.Types.FailedCreateAssociation
import Network.AWS.SSM.Types.FailureDetails
import Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
import Network.AWS.SSM.Types.InstanceAssociation
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.InstanceAssociationOutputURL
import Network.AWS.SSM.Types.InstanceAssociationStatusInfo
import Network.AWS.SSM.Types.InstanceInformation
import Network.AWS.SSM.Types.InstanceInformationFilter
import Network.AWS.SSM.Types.InstanceInformationStringFilter
import Network.AWS.SSM.Types.InstancePatchState
import Network.AWS.SSM.Types.InstancePatchStateFilter
import Network.AWS.SSM.Types.InventoryAggregator
import Network.AWS.SSM.Types.InventoryDeletionStatusItem
import Network.AWS.SSM.Types.InventoryDeletionSummary
import Network.AWS.SSM.Types.InventoryDeletionSummaryItem
import Network.AWS.SSM.Types.InventoryFilter
import Network.AWS.SSM.Types.InventoryGroup
import Network.AWS.SSM.Types.InventoryItem
import Network.AWS.SSM.Types.InventoryItemAttribute
import Network.AWS.SSM.Types.InventoryItemSchema
import Network.AWS.SSM.Types.InventoryResultEntity
import Network.AWS.SSM.Types.InventoryResultItem
import Network.AWS.SSM.Types.LoggingInfo
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowExecution
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskIdentity
import Network.AWS.SSM.Types.MaintenanceWindowExecutionTaskInvocationIdentity
import Network.AWS.SSM.Types.MaintenanceWindowFilter
import Network.AWS.SSM.Types.MaintenanceWindowIdentity
import Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
import Network.AWS.SSM.Types.MaintenanceWindowTarget
import Network.AWS.SSM.Types.MaintenanceWindowTask
import Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
import Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
import Network.AWS.SSM.Types.NonCompliantSummary
import Network.AWS.SSM.Types.NotificationConfig
import Network.AWS.SSM.Types.OpsAggregator
import Network.AWS.SSM.Types.OpsEntity
import Network.AWS.SSM.Types.OpsEntityItem
import Network.AWS.SSM.Types.OpsFilter
import Network.AWS.SSM.Types.OpsItem
import Network.AWS.SSM.Types.OpsItemDataValue
import Network.AWS.SSM.Types.OpsItemFilter
import Network.AWS.SSM.Types.OpsItemNotification
import Network.AWS.SSM.Types.OpsItemSummary
import Network.AWS.SSM.Types.OpsResultAttribute
import Network.AWS.SSM.Types.OutputSource
import Network.AWS.SSM.Types.Parameter
import Network.AWS.SSM.Types.ParameterHistory
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterMetadata
import Network.AWS.SSM.Types.ParameterStringFilter
import Network.AWS.SSM.Types.ParametersFilter
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchBaselineIdentity
import Network.AWS.SSM.Types.PatchComplianceData
import Network.AWS.SSM.Types.PatchFilter
import Network.AWS.SSM.Types.PatchFilterGroup
import Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
import Network.AWS.SSM.Types.PatchOrchestratorFilter
import Network.AWS.SSM.Types.PatchRule
import Network.AWS.SSM.Types.PatchRuleGroup
import Network.AWS.SSM.Types.PatchSource
import Network.AWS.SSM.Types.PatchStatus
import Network.AWS.SSM.Types.ProgressCounters
import Network.AWS.SSM.Types.RelatedOpsItem
import Network.AWS.SSM.Types.ResolvedTargets
import Network.AWS.SSM.Types.ResourceComplianceSummaryItem
import Network.AWS.SSM.Types.ResourceDataSyncAWSOrganizationsSource
import Network.AWS.SSM.Types.ResourceDataSyncDestinationDataSharing
import Network.AWS.SSM.Types.ResourceDataSyncItem
import Network.AWS.SSM.Types.ResourceDataSyncOrganizationalUnit
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncSource
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState
import Network.AWS.SSM.Types.ResultAttribute
import Network.AWS.SSM.Types.S3OutputLocation
import Network.AWS.SSM.Types.S3OutputURL
import Network.AWS.SSM.Types.ScheduledWindowExecution
import Network.AWS.SSM.Types.ServiceSetting
import Network.AWS.SSM.Types.Session
import Network.AWS.SSM.Types.SessionFilter
import Network.AWS.SSM.Types.SessionManagerOutputURL
import Network.AWS.SSM.Types.SeveritySummary
import Network.AWS.SSM.Types.StepExecution
import Network.AWS.SSM.Types.StepExecutionFilter
import Network.AWS.SSM.Types.Tag
import Network.AWS.SSM.Types.Target
import Network.AWS.SSM.Types.TargetLocation
