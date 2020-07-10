{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.Product (
    module Network.AWS.CodePipeline.Types.AWSSessionCredentials,
    module Network.AWS.CodePipeline.Types.ActionConfiguration,
    module Network.AWS.CodePipeline.Types.ActionConfigurationProperty,
    module Network.AWS.CodePipeline.Types.ActionContext,
    module Network.AWS.CodePipeline.Types.ActionDeclaration,
    module Network.AWS.CodePipeline.Types.ActionExecution,
    module Network.AWS.CodePipeline.Types.ActionExecutionDetail,
    module Network.AWS.CodePipeline.Types.ActionExecutionFilter,
    module Network.AWS.CodePipeline.Types.ActionExecutionInput,
    module Network.AWS.CodePipeline.Types.ActionExecutionOutput,
    module Network.AWS.CodePipeline.Types.ActionExecutionResult,
    module Network.AWS.CodePipeline.Types.ActionRevision,
    module Network.AWS.CodePipeline.Types.ActionState,
    module Network.AWS.CodePipeline.Types.ActionType,
    module Network.AWS.CodePipeline.Types.ActionTypeId,
    module Network.AWS.CodePipeline.Types.ActionTypeSettings,
    module Network.AWS.CodePipeline.Types.ApprovalResult,
    module Network.AWS.CodePipeline.Types.Artifact,
    module Network.AWS.CodePipeline.Types.ArtifactDetail,
    module Network.AWS.CodePipeline.Types.ArtifactDetails,
    module Network.AWS.CodePipeline.Types.ArtifactLocation,
    module Network.AWS.CodePipeline.Types.ArtifactRevision,
    module Network.AWS.CodePipeline.Types.ArtifactStore,
    module Network.AWS.CodePipeline.Types.BlockerDeclaration,
    module Network.AWS.CodePipeline.Types.CurrentRevision,
    module Network.AWS.CodePipeline.Types.EncryptionKey,
    module Network.AWS.CodePipeline.Types.ErrorDetails,
    module Network.AWS.CodePipeline.Types.ExecutionDetails,
    module Network.AWS.CodePipeline.Types.ExecutionTrigger,
    module Network.AWS.CodePipeline.Types.FailureDetails,
    module Network.AWS.CodePipeline.Types.InputArtifact,
    module Network.AWS.CodePipeline.Types.Job,
    module Network.AWS.CodePipeline.Types.JobData,
    module Network.AWS.CodePipeline.Types.JobDetails,
    module Network.AWS.CodePipeline.Types.ListWebhookItem,
    module Network.AWS.CodePipeline.Types.OutputArtifact,
    module Network.AWS.CodePipeline.Types.PipelineContext,
    module Network.AWS.CodePipeline.Types.PipelineDeclaration,
    module Network.AWS.CodePipeline.Types.PipelineExecution,
    module Network.AWS.CodePipeline.Types.PipelineExecutionSummary,
    module Network.AWS.CodePipeline.Types.PipelineMetadata,
    module Network.AWS.CodePipeline.Types.PipelineSummary,
    module Network.AWS.CodePipeline.Types.S3ArtifactLocation,
    module Network.AWS.CodePipeline.Types.S3Location,
    module Network.AWS.CodePipeline.Types.SourceRevision,
    module Network.AWS.CodePipeline.Types.StageContext,
    module Network.AWS.CodePipeline.Types.StageDeclaration,
    module Network.AWS.CodePipeline.Types.StageExecution,
    module Network.AWS.CodePipeline.Types.StageState,
    module Network.AWS.CodePipeline.Types.StopExecutionTrigger,
    module Network.AWS.CodePipeline.Types.Tag,
    module Network.AWS.CodePipeline.Types.ThirdPartyJob,
    module Network.AWS.CodePipeline.Types.ThirdPartyJobData,
    module Network.AWS.CodePipeline.Types.ThirdPartyJobDetails,
    module Network.AWS.CodePipeline.Types.TransitionState,
    module Network.AWS.CodePipeline.Types.WebhookAuthConfiguration,
    module Network.AWS.CodePipeline.Types.WebhookDefinition,
    module Network.AWS.CodePipeline.Types.WebhookFilterRule
  ) where

import Network.AWS.CodePipeline.Types.AWSSessionCredentials
import Network.AWS.CodePipeline.Types.ActionConfiguration
import Network.AWS.CodePipeline.Types.ActionConfigurationProperty
import Network.AWS.CodePipeline.Types.ActionContext
import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.ActionExecution
import Network.AWS.CodePipeline.Types.ActionExecutionDetail
import Network.AWS.CodePipeline.Types.ActionExecutionFilter
import Network.AWS.CodePipeline.Types.ActionExecutionInput
import Network.AWS.CodePipeline.Types.ActionExecutionOutput
import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ActionRevision
import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.ActionType
import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ActionTypeSettings
import Network.AWS.CodePipeline.Types.ApprovalResult
import Network.AWS.CodePipeline.Types.Artifact
import Network.AWS.CodePipeline.Types.ArtifactDetail
import Network.AWS.CodePipeline.Types.ArtifactDetails
import Network.AWS.CodePipeline.Types.ArtifactLocation
import Network.AWS.CodePipeline.Types.ArtifactRevision
import Network.AWS.CodePipeline.Types.ArtifactStore
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import Network.AWS.CodePipeline.Types.CurrentRevision
import Network.AWS.CodePipeline.Types.EncryptionKey
import Network.AWS.CodePipeline.Types.ErrorDetails
import Network.AWS.CodePipeline.Types.ExecutionDetails
import Network.AWS.CodePipeline.Types.ExecutionTrigger
import Network.AWS.CodePipeline.Types.FailureDetails
import Network.AWS.CodePipeline.Types.InputArtifact
import Network.AWS.CodePipeline.Types.Job
import Network.AWS.CodePipeline.Types.JobData
import Network.AWS.CodePipeline.Types.JobDetails
import Network.AWS.CodePipeline.Types.ListWebhookItem
import Network.AWS.CodePipeline.Types.OutputArtifact
import Network.AWS.CodePipeline.Types.PipelineContext
import Network.AWS.CodePipeline.Types.PipelineDeclaration
import Network.AWS.CodePipeline.Types.PipelineExecution
import Network.AWS.CodePipeline.Types.PipelineExecutionSummary
import Network.AWS.CodePipeline.Types.PipelineMetadata
import Network.AWS.CodePipeline.Types.PipelineSummary
import Network.AWS.CodePipeline.Types.S3ArtifactLocation
import Network.AWS.CodePipeline.Types.S3Location
import Network.AWS.CodePipeline.Types.SourceRevision
import Network.AWS.CodePipeline.Types.StageContext
import Network.AWS.CodePipeline.Types.StageDeclaration
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.StageState
import Network.AWS.CodePipeline.Types.StopExecutionTrigger
import Network.AWS.CodePipeline.Types.Tag
import Network.AWS.CodePipeline.Types.ThirdPartyJob
import Network.AWS.CodePipeline.Types.ThirdPartyJobData
import Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
import Network.AWS.CodePipeline.Types.TransitionState
import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookDefinition
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import Network.AWS.Lens
import Network.AWS.Prelude
