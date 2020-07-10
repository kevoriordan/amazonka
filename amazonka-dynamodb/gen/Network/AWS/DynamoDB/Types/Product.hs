{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Product (
    module Network.AWS.DynamoDB.Types.ArchivalSummary,
    module Network.AWS.DynamoDB.Types.AttributeDefinition,
    module Network.AWS.DynamoDB.Types.AttributeValue,
    module Network.AWS.DynamoDB.Types.AttributeValueUpdate,
    module Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription,
    module Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate,
    module Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription,
    module Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate,
    module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription,
    module Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate,
    module Network.AWS.DynamoDB.Types.BackupDescription,
    module Network.AWS.DynamoDB.Types.BackupDetails,
    module Network.AWS.DynamoDB.Types.BackupSummary,
    module Network.AWS.DynamoDB.Types.BillingModeSummary,
    module Network.AWS.DynamoDB.Types.Capacity,
    module Network.AWS.DynamoDB.Types.Condition,
    module Network.AWS.DynamoDB.Types.ConditionCheck,
    module Network.AWS.DynamoDB.Types.ConsumedCapacity,
    module Network.AWS.DynamoDB.Types.ContinuousBackupsDescription,
    module Network.AWS.DynamoDB.Types.ContributorInsightsSummary,
    module Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction,
    module Network.AWS.DynamoDB.Types.CreateReplicaAction,
    module Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction,
    module Network.AWS.DynamoDB.Types.Delete,
    module Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction,
    module Network.AWS.DynamoDB.Types.DeleteReplicaAction,
    module Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction,
    module Network.AWS.DynamoDB.Types.DeleteRequest,
    module Network.AWS.DynamoDB.Types.Endpoint,
    module Network.AWS.DynamoDB.Types.ExpectedAttributeValue,
    module Network.AWS.DynamoDB.Types.FailureException,
    module Network.AWS.DynamoDB.Types.Get,
    module Network.AWS.DynamoDB.Types.GlobalSecondaryIndex,
    module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate,
    module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription,
    module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo,
    module Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate,
    module Network.AWS.DynamoDB.Types.GlobalTable,
    module Network.AWS.DynamoDB.Types.GlobalTableDescription,
    module Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate,
    module Network.AWS.DynamoDB.Types.ItemCollectionMetrics,
    module Network.AWS.DynamoDB.Types.ItemResponse,
    module Network.AWS.DynamoDB.Types.KeySchemaElement,
    module Network.AWS.DynamoDB.Types.KeysAndAttributes,
    module Network.AWS.DynamoDB.Types.LocalSecondaryIndex,
    module Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription,
    module Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo,
    module Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription,
    module Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification,
    module Network.AWS.DynamoDB.Types.Projection,
    module Network.AWS.DynamoDB.Types.ProvisionedThroughput,
    module Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription,
    module Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride,
    module Network.AWS.DynamoDB.Types.Put,
    module Network.AWS.DynamoDB.Types.PutRequest,
    module Network.AWS.DynamoDB.Types.Replica,
    module Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription,
    module Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate,
    module Network.AWS.DynamoDB.Types.ReplicaDescription,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription,
    module Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate,
    module Network.AWS.DynamoDB.Types.ReplicaSettingsDescription,
    module Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate,
    module Network.AWS.DynamoDB.Types.ReplicaUpdate,
    module Network.AWS.DynamoDB.Types.ReplicationGroupUpdate,
    module Network.AWS.DynamoDB.Types.RestoreSummary,
    module Network.AWS.DynamoDB.Types.SSEDescription,
    module Network.AWS.DynamoDB.Types.SSESpecification,
    module Network.AWS.DynamoDB.Types.SourceTableDetails,
    module Network.AWS.DynamoDB.Types.SourceTableFeatureDetails,
    module Network.AWS.DynamoDB.Types.StreamSpecification,
    module Network.AWS.DynamoDB.Types.TableAutoScalingDescription,
    module Network.AWS.DynamoDB.Types.TableDescription,
    module Network.AWS.DynamoDB.Types.Tag,
    module Network.AWS.DynamoDB.Types.TimeToLiveDescription,
    module Network.AWS.DynamoDB.Types.TimeToLiveSpecification,
    module Network.AWS.DynamoDB.Types.TransactGetItem,
    module Network.AWS.DynamoDB.Types.TransactWriteItem,
    module Network.AWS.DynamoDB.Types.Update,
    module Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction,
    module Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction,
    module Network.AWS.DynamoDB.Types.WriteRequest
  ) where

import Network.AWS.DynamoDB.Types.ArchivalSummary
import Network.AWS.DynamoDB.Types.AttributeDefinition
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.AttributeValueUpdate
import Network.AWS.DynamoDB.Types.AutoScalingPolicyDescription
import Network.AWS.DynamoDB.Types.AutoScalingPolicyUpdate
import Network.AWS.DynamoDB.Types.AutoScalingSettingsDescription
import Network.AWS.DynamoDB.Types.AutoScalingSettingsUpdate
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationDescription
import Network.AWS.DynamoDB.Types.AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
import Network.AWS.DynamoDB.Types.BackupDescription
import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.BackupSummary
import Network.AWS.DynamoDB.Types.BillingModeSummary
import Network.AWS.DynamoDB.Types.Capacity
import Network.AWS.DynamoDB.Types.Condition
import Network.AWS.DynamoDB.Types.ConditionCheck
import Network.AWS.DynamoDB.Types.ConsumedCapacity
import Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
import Network.AWS.DynamoDB.Types.ContributorInsightsSummary
import Network.AWS.DynamoDB.Types.CreateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.CreateReplicaAction
import Network.AWS.DynamoDB.Types.CreateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.Delete
import Network.AWS.DynamoDB.Types.DeleteGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.DeleteReplicaAction
import Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.DeleteRequest
import Network.AWS.DynamoDB.Types.Endpoint
import Network.AWS.DynamoDB.Types.ExpectedAttributeValue
import Network.AWS.DynamoDB.Types.FailureException
import Network.AWS.DynamoDB.Types.Get
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndex
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.GlobalSecondaryIndexUpdate
import Network.AWS.DynamoDB.Types.GlobalTable
import Network.AWS.DynamoDB.Types.GlobalTableDescription
import Network.AWS.DynamoDB.Types.GlobalTableGlobalSecondaryIndexSettingsUpdate
import Network.AWS.DynamoDB.Types.ItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ItemResponse
import Network.AWS.DynamoDB.Types.KeySchemaElement
import Network.AWS.DynamoDB.Types.KeysAndAttributes
import Network.AWS.DynamoDB.Types.LocalSecondaryIndex
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.LocalSecondaryIndexInfo
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
import Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
import Network.AWS.DynamoDB.Types.Projection
import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import Network.AWS.DynamoDB.Types.ProvisionedThroughputDescription
import Network.AWS.DynamoDB.Types.ProvisionedThroughputOverride
import Network.AWS.DynamoDB.Types.Put
import Network.AWS.DynamoDB.Types.PutRequest
import Network.AWS.DynamoDB.Types.Replica
import Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaAutoScalingUpdate
import Network.AWS.DynamoDB.Types.ReplicaDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndex
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaGlobalSecondaryIndexSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaSettingsDescription
import Network.AWS.DynamoDB.Types.ReplicaSettingsUpdate
import Network.AWS.DynamoDB.Types.ReplicaUpdate
import Network.AWS.DynamoDB.Types.ReplicationGroupUpdate
import Network.AWS.DynamoDB.Types.RestoreSummary
import Network.AWS.DynamoDB.Types.SSEDescription
import Network.AWS.DynamoDB.Types.SSESpecification
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import Network.AWS.DynamoDB.Types.StreamSpecification
import Network.AWS.DynamoDB.Types.TableAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableDescription
import Network.AWS.DynamoDB.Types.Tag
import Network.AWS.DynamoDB.Types.TimeToLiveDescription
import Network.AWS.DynamoDB.Types.TimeToLiveSpecification
import Network.AWS.DynamoDB.Types.TransactGetItem
import Network.AWS.DynamoDB.Types.TransactWriteItem
import Network.AWS.DynamoDB.Types.Update
import Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
import Network.AWS.DynamoDB.Types.UpdateReplicationGroupMemberAction
import Network.AWS.DynamoDB.Types.WriteRequest
import Network.AWS.Lens
import Network.AWS.Prelude
