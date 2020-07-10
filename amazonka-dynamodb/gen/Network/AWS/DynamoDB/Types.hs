{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types
    (
    -- * Service Configuration
      dynamoDB

    -- * Errors
    , _IdempotentParameterMismatchException
    , _TransactionConflictException
    , _ContinuousBackupsUnavailableException
    , _BackupInUseException
    , _TableInUseException
    , _IndexNotFoundException
    , _TableNotFoundException
    , _BackupNotFoundException
    , _ItemCollectionSizeLimitExceededException
    , _RequestLimitExceeded
    , _TransactionCanceledException
    , _ReplicaNotFoundException
    , _ReplicaAlreadyExistsException
    , _GlobalTableNotFoundException
    , _TransactionInProgressException
    , _ProvisionedThroughputExceededException
    , _ResourceNotFoundException
    , _InvalidRestoreTimeException
    , _PointInTimeRecoveryUnavailableException
    , _InternalServerError
    , _TableAlreadyExistsException
    , _GlobalTableAlreadyExistsException
    , _ConditionalCheckFailedException
    , _LimitExceededException
    , _ResourceInUseException

    -- * AttributeAction
    , AttributeAction (..)

    -- * BackupStatus
    , BackupStatus (..)

    -- * BackupType
    , BackupType (..)

    -- * BackupTypeFilter
    , BackupTypeFilter (..)

    -- * BillingMode
    , BillingMode (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * ConditionalOperator
    , ConditionalOperator (..)

    -- * ContinuousBackupsStatus
    , ContinuousBackupsStatus (..)

    -- * ContributorInsightsAction
    , ContributorInsightsAction (..)

    -- * ContributorInsightsStatus
    , ContributorInsightsStatus (..)

    -- * GlobalTableStatus
    , GlobalTableStatus (..)

    -- * IndexStatus
    , IndexStatus (..)

    -- * KeyType
    , KeyType (..)

    -- * PointInTimeRecoveryStatus
    , PointInTimeRecoveryStatus (..)

    -- * ProjectionType
    , ProjectionType (..)

    -- * ReplicaStatus
    , ReplicaStatus (..)

    -- * ReturnConsumedCapacity
    , ReturnConsumedCapacity (..)

    -- * ReturnItemCollectionMetrics
    , ReturnItemCollectionMetrics (..)

    -- * ReturnValue
    , ReturnValue (..)

    -- * ReturnValuesOnConditionCheckFailure
    , ReturnValuesOnConditionCheckFailure (..)

    -- * SSEStatus
    , SSEStatus (..)

    -- * SSEType
    , SSEType (..)

    -- * ScalarAttributeType
    , ScalarAttributeType (..)

    -- * Select
    , Select (..)

    -- * StreamViewType
    , StreamViewType (..)

    -- * TableStatus
    , TableStatus (..)

    -- * TimeToLiveStatus
    , TimeToLiveStatus (..)

    -- * ArchivalSummary
    , ArchivalSummary
    , archivalSummary
    , asArchivalReason
    , asArchivalDateTime
    , asArchivalBackupARN

    -- * AttributeDefinition
    , AttributeDefinition
    , attributeDefinition
    , adAttributeName
    , adAttributeType

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avNS
    , avM
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * AttributeValueUpdate
    , AttributeValueUpdate
    , attributeValueUpdate
    , avuValue
    , avuAction

    -- * AutoScalingPolicyDescription
    , AutoScalingPolicyDescription
    , autoScalingPolicyDescription
    , aspdPolicyName
    , aspdTargetTrackingScalingPolicyConfiguration

    -- * AutoScalingPolicyUpdate
    , AutoScalingPolicyUpdate
    , autoScalingPolicyUpdate
    , aspuPolicyName
    , aspuTargetTrackingScalingPolicyConfiguration

    -- * AutoScalingSettingsDescription
    , AutoScalingSettingsDescription
    , autoScalingSettingsDescription
    , assdAutoScalingDisabled
    , assdMinimumUnits
    , assdMaximumUnits
    , assdScalingPolicies
    , assdAutoScalingRoleARN

    -- * AutoScalingSettingsUpdate
    , AutoScalingSettingsUpdate
    , autoScalingSettingsUpdate
    , assuAutoScalingDisabled
    , assuMinimumUnits
    , assuScalingPolicyUpdate
    , assuMaximumUnits
    , assuAutoScalingRoleARN

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , AutoScalingTargetTrackingScalingPolicyConfigurationDescription
    , autoScalingTargetTrackingScalingPolicyConfigurationDescription
    , asttspcdScaleInCooldown
    , asttspcdDisableScaleIn
    , asttspcdScaleOutCooldown
    , asttspcdTargetValue

    -- * AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , AutoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , autoScalingTargetTrackingScalingPolicyConfigurationUpdate
    , asttspcuScaleInCooldown
    , asttspcuDisableScaleIn
    , asttspcuScaleOutCooldown
    , asttspcuTargetValue

    -- * BackupDescription
    , BackupDescription
    , backupDescription
    , bdBackupDetails
    , bdSourceTableDetails
    , bdSourceTableFeatureDetails

    -- * BackupDetails
    , BackupDetails
    , backupDetails
    , bdBackupExpiryDateTime
    , bdBackupSizeBytes
    , bdBackupARN
    , bdBackupName
    , bdBackupStatus
    , bdBackupType
    , bdBackupCreationDateTime

    -- * BackupSummary
    , BackupSummary
    , backupSummary
    , bsBackupExpiryDateTime
    , bsTableARN
    , bsBackupName
    , bsBackupStatus
    , bsBackupSizeBytes
    , bsBackupARN
    , bsTableId
    , bsBackupCreationDateTime
    , bsBackupType
    , bsTableName

    -- * BillingModeSummary
    , BillingModeSummary
    , billingModeSummary
    , bmsLastUpdateToPayPerRequestDateTime
    , bmsBillingMode

    -- * Capacity
    , Capacity
    , capacity
    , capReadCapacityUnits
    , capCapacityUnits
    , capWriteCapacityUnits

    -- * Condition
    , Condition
    , condition
    , cAttributeValueList
    , cComparisonOperator

    -- * ConditionCheck
    , ConditionCheck
    , conditionCheck
    , ccExpressionAttributeNames
    , ccExpressionAttributeValues
    , ccReturnValuesOnConditionCheckFailure
    , ccKey
    , ccTableName
    , ccConditionExpression

    -- * ConsumedCapacity
    , ConsumedCapacity
    , consumedCapacity
    , cReadCapacityUnits
    , cGlobalSecondaryIndexes
    , cCapacityUnits
    , cWriteCapacityUnits
    , cLocalSecondaryIndexes
    , cTable
    , cTableName

    -- * ContinuousBackupsDescription
    , ContinuousBackupsDescription
    , continuousBackupsDescription
    , cbdPointInTimeRecoveryDescription
    , cbdContinuousBackupsStatus

    -- * ContributorInsightsSummary
    , ContributorInsightsSummary
    , contributorInsightsSummary
    , cisContributorInsightsStatus
    , cisTableName
    , cisIndexName

    -- * CreateGlobalSecondaryIndexAction
    , CreateGlobalSecondaryIndexAction
    , createGlobalSecondaryIndexAction
    , cgsiaProvisionedThroughput
    , cgsiaIndexName
    , cgsiaKeySchema
    , cgsiaProjection

    -- * CreateReplicaAction
    , CreateReplicaAction
    , createReplicaAction
    , craRegionName

    -- * CreateReplicationGroupMemberAction
    , CreateReplicationGroupMemberAction
    , createReplicationGroupMemberAction
    , crgmaKMSMasterKeyId
    , crgmaProvisionedThroughputOverride
    , crgmaGlobalSecondaryIndexes
    , crgmaRegionName

    -- * Delete
    , Delete
    , delete'
    , dExpressionAttributeNames
    , dExpressionAttributeValues
    , dReturnValuesOnConditionCheckFailure
    , dConditionExpression
    , dKey
    , dTableName

    -- * DeleteGlobalSecondaryIndexAction
    , DeleteGlobalSecondaryIndexAction
    , deleteGlobalSecondaryIndexAction
    , dgsiaIndexName

    -- * DeleteReplicaAction
    , DeleteReplicaAction
    , deleteReplicaAction
    , draRegionName

    -- * DeleteReplicationGroupMemberAction
    , DeleteReplicationGroupMemberAction
    , deleteReplicationGroupMemberAction
    , drgmaRegionName

    -- * DeleteRequest
    , DeleteRequest
    , deleteRequest
    , drKey

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , eCachePeriodInMinutes

    -- * ExpectedAttributeValue
    , ExpectedAttributeValue
    , expectedAttributeValue
    , eavAttributeValueList
    , eavExists
    , eavValue
    , eavComparisonOperator

    -- * FailureException
    , FailureException
    , failureException
    , feExceptionName
    , feExceptionDescription

    -- * Get
    , Get
    , get'
    , getProjectionExpression
    , getExpressionAttributeNames
    , getKey
    , getTableName

    -- * GlobalSecondaryIndex
    , GlobalSecondaryIndex
    , globalSecondaryIndex
    , gsiProvisionedThroughput
    , gsiIndexName
    , gsiKeySchema
    , gsiProjection

    -- * GlobalSecondaryIndexAutoScalingUpdate
    , GlobalSecondaryIndexAutoScalingUpdate
    , globalSecondaryIndexAutoScalingUpdate
    , gsiasuProvisionedWriteCapacityAutoScalingUpdate
    , gsiasuIndexName

    -- * GlobalSecondaryIndexDescription
    , GlobalSecondaryIndexDescription
    , globalSecondaryIndexDescription
    , gsidBackfilling
    , gsidIndexSizeBytes
    , gsidIndexStatus
    , gsidProvisionedThroughput
    , gsidIndexARN
    , gsidKeySchema
    , gsidProjection
    , gsidItemCount
    , gsidIndexName

    -- * GlobalSecondaryIndexInfo
    , GlobalSecondaryIndexInfo
    , globalSecondaryIndexInfo
    , gsiiProvisionedThroughput
    , gsiiKeySchema
    , gsiiProjection
    , gsiiIndexName

    -- * GlobalSecondaryIndexUpdate
    , GlobalSecondaryIndexUpdate
    , globalSecondaryIndexUpdate
    , gsiuCreate
    , gsiuDelete
    , gsiuUpdate

    -- * GlobalTable
    , GlobalTable
    , globalTable
    , gtGlobalTableName
    , gtReplicationGroup

    -- * GlobalTableDescription
    , GlobalTableDescription
    , globalTableDescription
    , gtdGlobalTableStatus
    , gtdGlobalTableName
    , gtdGlobalTableARN
    , gtdCreationDateTime
    , gtdReplicationGroup

    -- * GlobalTableGlobalSecondaryIndexSettingsUpdate
    , GlobalTableGlobalSecondaryIndexSettingsUpdate
    , globalTableGlobalSecondaryIndexSettingsUpdate
    , gtgsisuProvisionedWriteCapacityUnits
    , gtgsisuProvisionedWriteCapacityAutoScalingSettingsUpdate
    , gtgsisuIndexName

    -- * ItemCollectionMetrics
    , ItemCollectionMetrics
    , itemCollectionMetrics
    , icmItemCollectionKey
    , icmSizeEstimateRangeGB

    -- * ItemResponse
    , ItemResponse
    , itemResponse
    , iItem

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * KeysAndAttributes
    , KeysAndAttributes
    , keysAndAttributes
    , kaaProjectionExpression
    , kaaAttributesToGet
    , kaaExpressionAttributeNames
    , kaaConsistentRead
    , kaaKeys

    -- * LocalSecondaryIndex
    , LocalSecondaryIndex
    , localSecondaryIndex
    , lsiIndexName
    , lsiKeySchema
    , lsiProjection

    -- * LocalSecondaryIndexDescription
    , LocalSecondaryIndexDescription
    , localSecondaryIndexDescription
    , lsidIndexSizeBytes
    , lsidIndexARN
    , lsidKeySchema
    , lsidProjection
    , lsidItemCount
    , lsidIndexName

    -- * LocalSecondaryIndexInfo
    , LocalSecondaryIndexInfo
    , localSecondaryIndexInfo
    , lsiiKeySchema
    , lsiiProjection
    , lsiiIndexName

    -- * PointInTimeRecoveryDescription
    , PointInTimeRecoveryDescription
    , pointInTimeRecoveryDescription
    , pitrdPointInTimeRecoveryStatus
    , pitrdEarliestRestorableDateTime
    , pitrdLatestRestorableDateTime

    -- * PointInTimeRecoverySpecification
    , PointInTimeRecoverySpecification
    , pointInTimeRecoverySpecification
    , pitrsPointInTimeRecoveryEnabled

    -- * Projection
    , Projection
    , projection
    , pProjectionType
    , pNonKeyAttributes

    -- * ProvisionedThroughput
    , ProvisionedThroughput
    , provisionedThroughput
    , ptReadCapacityUnits
    , ptWriteCapacityUnits

    -- * ProvisionedThroughputDescription
    , ProvisionedThroughputDescription
    , provisionedThroughputDescription
    , ptdReadCapacityUnits
    , ptdLastDecreaseDateTime
    , ptdWriteCapacityUnits
    , ptdNumberOfDecreasesToday
    , ptdLastIncreaseDateTime

    -- * ProvisionedThroughputOverride
    , ProvisionedThroughputOverride
    , provisionedThroughputOverride
    , ptoReadCapacityUnits

    -- * Put
    , Put
    , put
    , pExpressionAttributeNames
    , pExpressionAttributeValues
    , pReturnValuesOnConditionCheckFailure
    , pConditionExpression
    , pItem
    , pTableName

    -- * PutRequest
    , PutRequest
    , putRequest
    , prItem

    -- * Replica
    , Replica
    , replica
    , rRegionName

    -- * ReplicaAutoScalingDescription
    , ReplicaAutoScalingDescription
    , replicaAutoScalingDescription
    , rasdReplicaStatus
    , rasdRegionName
    , rasdGlobalSecondaryIndexes
    , rasdReplicaProvisionedWriteCapacityAutoScalingSettings
    , rasdReplicaProvisionedReadCapacityAutoScalingSettings

    -- * ReplicaAutoScalingUpdate
    , ReplicaAutoScalingUpdate
    , replicaAutoScalingUpdate
    , rasuReplicaProvisionedReadCapacityAutoScalingUpdate
    , rasuReplicaGlobalSecondaryIndexUpdates
    , rasuRegionName

    -- * ReplicaDescription
    , ReplicaDescription
    , replicaDescription
    , rdReplicaStatus
    , rdRegionName
    , rdReplicaStatusPercentProgress
    , rdReplicaStatusDescription
    , rdKMSMasterKeyId
    , rdProvisionedThroughputOverride
    , rdGlobalSecondaryIndexes

    -- * ReplicaGlobalSecondaryIndex
    , ReplicaGlobalSecondaryIndex
    , replicaGlobalSecondaryIndex
    , rgsiProvisionedThroughputOverride
    , rgsiIndexName

    -- * ReplicaGlobalSecondaryIndexAutoScalingDescription
    , ReplicaGlobalSecondaryIndexAutoScalingDescription
    , replicaGlobalSecondaryIndexAutoScalingDescription
    , rgsiasdIndexStatus
    , rgsiasdProvisionedWriteCapacityAutoScalingSettings
    , rgsiasdProvisionedReadCapacityAutoScalingSettings
    , rgsiasdIndexName

    -- * ReplicaGlobalSecondaryIndexAutoScalingUpdate
    , ReplicaGlobalSecondaryIndexAutoScalingUpdate
    , replicaGlobalSecondaryIndexAutoScalingUpdate
    , rgsiasuProvisionedReadCapacityAutoScalingUpdate
    , rgsiasuIndexName

    -- * ReplicaGlobalSecondaryIndexDescription
    , ReplicaGlobalSecondaryIndexDescription
    , replicaGlobalSecondaryIndexDescription
    , rgsidProvisionedThroughputOverride
    , rgsidIndexName

    -- * ReplicaGlobalSecondaryIndexSettingsDescription
    , ReplicaGlobalSecondaryIndexSettingsDescription
    , replicaGlobalSecondaryIndexSettingsDescription
    , rgsisdIndexStatus
    , rgsisdProvisionedReadCapacityUnits
    , rgsisdProvisionedWriteCapacityUnits
    , rgsisdProvisionedWriteCapacityAutoScalingSettings
    , rgsisdProvisionedReadCapacityAutoScalingSettings
    , rgsisdIndexName

    -- * ReplicaGlobalSecondaryIndexSettingsUpdate
    , ReplicaGlobalSecondaryIndexSettingsUpdate
    , replicaGlobalSecondaryIndexSettingsUpdate
    , rgsisuProvisionedReadCapacityAutoScalingSettingsUpdate
    , rgsisuProvisionedReadCapacityUnits
    , rgsisuIndexName

    -- * ReplicaSettingsDescription
    , ReplicaSettingsDescription
    , replicaSettingsDescription
    , rsdReplicaStatus
    , rsdReplicaProvisionedReadCapacityUnits
    , rsdReplicaProvisionedWriteCapacityUnits
    , rsdReplicaBillingModeSummary
    , rsdReplicaGlobalSecondaryIndexSettings
    , rsdReplicaProvisionedWriteCapacityAutoScalingSettings
    , rsdReplicaProvisionedReadCapacityAutoScalingSettings
    , rsdRegionName

    -- * ReplicaSettingsUpdate
    , ReplicaSettingsUpdate
    , replicaSettingsUpdate
    , rsuReplicaProvisionedReadCapacityAutoScalingSettingsUpdate
    , rsuReplicaProvisionedReadCapacityUnits
    , rsuReplicaGlobalSecondaryIndexSettingsUpdate
    , rsuRegionName

    -- * ReplicaUpdate
    , ReplicaUpdate
    , replicaUpdate
    , ruCreate
    , ruDelete

    -- * ReplicationGroupUpdate
    , ReplicationGroupUpdate
    , replicationGroupUpdate
    , rguCreate
    , rguDelete
    , rguUpdate

    -- * RestoreSummary
    , RestoreSummary
    , restoreSummary
    , rsSourceTableARN
    , rsSourceBackupARN
    , rsRestoreDateTime
    , rsRestoreInProgress

    -- * SSEDescription
    , SSEDescription
    , sSEDescription
    , ssedStatus
    , ssedInaccessibleEncryptionDateTime
    , ssedSSEType
    , ssedKMSMasterKeyARN

    -- * SSESpecification
    , SSESpecification
    , sSESpecification
    , ssesEnabled
    , ssesKMSMasterKeyId
    , ssesSSEType

    -- * SourceTableDetails
    , SourceTableDetails
    , sourceTableDetails
    , stdTableSizeBytes
    , stdTableARN
    , stdBillingMode
    , stdItemCount
    , stdTableName
    , stdTableId
    , stdKeySchema
    , stdTableCreationDateTime
    , stdProvisionedThroughput

    -- * SourceTableFeatureDetails
    , SourceTableFeatureDetails
    , sourceTableFeatureDetails
    , stfdStreamDescription
    , stfdGlobalSecondaryIndexes
    , stfdLocalSecondaryIndexes
    , stfdSSEDescription
    , stfdTimeToLiveDescription

    -- * StreamSpecification
    , StreamSpecification
    , streamSpecification
    , ssStreamViewType
    , ssStreamEnabled

    -- * TableAutoScalingDescription
    , TableAutoScalingDescription
    , tableAutoScalingDescription
    , tasdTableStatus
    , tasdReplicas
    , tasdTableName

    -- * TableDescription
    , TableDescription
    , tableDescription
    , tdRestoreSummary
    , tdGlobalTableVersion
    , tdTableSizeBytes
    , tdAttributeDefinitions
    , tdLatestStreamARN
    , tdProvisionedThroughput
    , tdTableStatus
    , tdTableARN
    , tdKeySchema
    , tdGlobalSecondaryIndexes
    , tdLatestStreamLabel
    , tdBillingModeSummary
    , tdLocalSecondaryIndexes
    , tdCreationDateTime
    , tdSSEDescription
    , tdTableId
    , tdReplicas
    , tdItemCount
    , tdArchivalSummary
    , tdTableName
    , tdStreamSpecification

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TimeToLiveDescription
    , TimeToLiveDescription
    , timeToLiveDescription
    , ttldTimeToLiveStatus
    , ttldAttributeName

    -- * TimeToLiveSpecification
    , TimeToLiveSpecification
    , timeToLiveSpecification
    , ttlsEnabled
    , ttlsAttributeName

    -- * TransactGetItem
    , TransactGetItem
    , transactGetItem
    , tgiGet

    -- * TransactWriteItem
    , TransactWriteItem
    , transactWriteItem
    , twiConditionCheck
    , twiPut
    , twiDelete
    , twiUpdate

    -- * Update
    , Update
    , update
    , uExpressionAttributeNames
    , uExpressionAttributeValues
    , uReturnValuesOnConditionCheckFailure
    , uConditionExpression
    , uKey
    , uUpdateExpression
    , uTableName

    -- * UpdateGlobalSecondaryIndexAction
    , UpdateGlobalSecondaryIndexAction
    , updateGlobalSecondaryIndexAction
    , ugsiaIndexName
    , ugsiaProvisionedThroughput

    -- * UpdateReplicationGroupMemberAction
    , UpdateReplicationGroupMemberAction
    , updateReplicationGroupMemberAction
    , urgmaKMSMasterKeyId
    , urgmaProvisionedThroughputOverride
    , urgmaGlobalSecondaryIndexes
    , urgmaRegionName

    -- * WriteRequest
    , WriteRequest
    , writeRequest
    , wrDeleteRequest
    , wrPutRequest
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.DynamoDB.Types.AttributeAction
import Network.AWS.DynamoDB.Types.BackupStatus
import Network.AWS.DynamoDB.Types.BackupType
import Network.AWS.DynamoDB.Types.BackupTypeFilter
import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.DynamoDB.Types.ComparisonOperator
import Network.AWS.DynamoDB.Types.ConditionalOperator
import Network.AWS.DynamoDB.Types.ContinuousBackupsStatus
import Network.AWS.DynamoDB.Types.ContributorInsightsAction
import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import Network.AWS.DynamoDB.Types.GlobalTableStatus
import Network.AWS.DynamoDB.Types.IndexStatus
import Network.AWS.DynamoDB.Types.KeyType
import Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
import Network.AWS.DynamoDB.Types.ProjectionType
import Network.AWS.DynamoDB.Types.ReplicaStatus
import Network.AWS.DynamoDB.Types.ReturnConsumedCapacity
import Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics
import Network.AWS.DynamoDB.Types.ReturnValue
import Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
import Network.AWS.DynamoDB.Types.SSEStatus
import Network.AWS.DynamoDB.Types.SSEType
import Network.AWS.DynamoDB.Types.ScalarAttributeType
import Network.AWS.DynamoDB.Types.Select
import Network.AWS.DynamoDB.Types.StreamViewType
import Network.AWS.DynamoDB.Types.TableStatus
import Network.AWS.DynamoDB.Types.TimeToLiveStatus
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

-- | API version @2012-08-10@ of the Amazon DynamoDB SDK configuration.
dynamoDB :: Service
dynamoDB
  = Service{_svcAbbrev = "DynamoDB", _svcSigner = v4,
            _svcPrefix = "dynamodb", _svcVersion = "2012-08-10",
            _svcEndpoint = defaultEndpoint dynamoDB,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "DynamoDB",
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
              (hasCode "TransactionInProgressException" .
                 hasStatus 400)
              e
            = Just "still_processing"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | DynamoDB rejected the request because you retried a request with a different payload but with an idempotent token that was already used.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException
  = _MatchServiceError dynamoDB
      "IdempotentParameterMismatchException"

-- | Operation was rejected because there is an ongoing transaction for the item.
--
--
_TransactionConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionConflictException
  = _MatchServiceError dynamoDB
      "TransactionConflictException"

-- | Backups have not yet been enabled for this table.
--
--
_ContinuousBackupsUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ContinuousBackupsUnavailableException
  = _MatchServiceError dynamoDB
      "ContinuousBackupsUnavailableException"

-- | There is another ongoing conflicting backup control plane operation on the table. The backup is either being created, deleted or restored to a table.
--
--
_BackupInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_BackupInUseException
  = _MatchServiceError dynamoDB "BackupInUseException"

-- | A target table with the specified name is either being created or deleted. 
--
--
_TableInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_TableInUseException
  = _MatchServiceError dynamoDB "TableInUseException"

-- | The operation tried to access a nonexistent index.
--
--
_IndexNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_IndexNotFoundException
  = _MatchServiceError dynamoDB
      "IndexNotFoundException"

-- | A source table with the name @TableName@ does not currently exist within the subscriber's account.
--
--
_TableNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TableNotFoundException
  = _MatchServiceError dynamoDB
      "TableNotFoundException"

-- | Backup not found for the given BackupARN. 
--
--
_BackupNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_BackupNotFoundException
  = _MatchServiceError dynamoDB
      "BackupNotFoundException"

-- | An item collection is too large. This exception is only returned for tables that have one or more local secondary indexes.
--
--
_ItemCollectionSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ItemCollectionSizeLimitExceededException
  = _MatchServiceError dynamoDB
      "ItemCollectionSizeLimitExceededException"

-- | Throughput exceeds the current throughput limit for your account. Please contact AWS Support at <https://aws.amazon.com/support AWS Support> to request a limit increase.
--
--
_RequestLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_RequestLimitExceeded
  = _MatchServiceError dynamoDB "RequestLimitExceeded"

-- | The entire transaction request was canceled.
--
--
-- DynamoDB cancels a @TransactWriteItems@ request under the following circumstances:
--
--     * A condition in one of the condition expressions is not met.
--
--     * A table in the @TransactWriteItems@ request is in a different account or region.
--
--     * More than one action in the @TransactWriteItems@ operation targets the same item.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * An item size becomes too large (larger than 400 KB), or a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--     * There is a user error, such as an invalid data format.
--
--
--
-- DynamoDB cancels a @TransactGetItems@ request under the following circumstances:
--
--     * There is an ongoing @TransactGetItems@ operation that conflicts with a concurrent @PutItem@ , @UpdateItem@ , @DeleteItem@ or @TransactWriteItems@ request. In this case the @TransactGetItems@ operation fails with a @TransactionCanceledException@ .
--
--     * A table in the @TransactGetItems@ request is in a different account or region.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * There is a user error, such as an invalid data format.
--
--
--
-- Cancellation reason codes and possible error messages:
--
--     * No Errors:
--
--     * Code: @NONE@ 
--
--     * Message: @null@ 
--
--
--
--     * Conditional Check Failed:
--
--     * Code: @ConditionalCheckFailed@ 
--
--     * Message: The conditional request failed. 
--
--
--
--     * Item Collection Size Limit Exceeded:
--
--     * Code: @ItemCollectionSizeLimitExceeded@ 
--
--     * Message: Collection size exceeded.
--
--
--
--     * Transaction Conflict:
--
--     * Code: @TransactionConflict@ 
--
--     * Message: Transaction is ongoing for the item.
--
--
--
--     * Provisioned Throughput Exceeded:
--
--     * Code: @ProvisionedThroughputExceeded@ 
--
--     * Messages: 
--
--     * The level of configured provisioned throughput for the table was exceeded. Consider increasing your provisioning level with the UpdateTable API.
--
--     * The level of configured provisioned throughput for one or more global secondary indexes of the table was exceeded. Consider increasing your provisioning level for the under-provisioned global secondary indexes with the UpdateTable API.
--
--
--
--
--
--     * Throttling Error:
--
--     * Code: @ThrottlingError@ 
--
--     * Messages: 
--
--     * Throughput exceeds the current capacity of your table or index. DynamoDB is automatically scaling your table or index so please try again shortly. If exceptions persist, check if you have a hot key: https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/bp-partition-key-design.html.
--
--     * Throughput exceeds the current capacity for one or more global secondary indexes. DynamoDB is automatically scaling your index so please try again shortly.
--
--
--
--
--
--     * Validation Error:
--
--     * Code: @ValidationError@ 
--
--     * Messages: 
--
--     * One or more parameter values were invalid.
--
--     * The update expression attempted to update the secondary index key beyond allowed size limits.
--
--     * The update expression attempted to update the secondary index key to unsupported type.
--
--     * An operand in the update expression has an incorrect data type.
--
--     * Item size to update has exceeded the maximum allowed size.
--
--     * Number overflow. Attempting to store a number with magnitude larger than supported range.
--
--     * Type mismatch for attribute to update.
--
--     * Nesting Levels have exceeded supported limits.
--
--     * The document path provided in the update expression is invalid for update.
--
--     * The provided expression refers to an attribute that does not exist in the item.
--
--
--
--
--
--
--
_TransactionCanceledException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionCanceledException
  = _MatchServiceError dynamoDB
      "TransactionCanceledException"

-- | The specified replica is no longer part of the global table.
--
--
_ReplicaNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicaNotFoundException
  = _MatchServiceError dynamoDB
      "ReplicaNotFoundException"

-- | The specified replica is already part of the global table.
--
--
_ReplicaAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicaAlreadyExistsException
  = _MatchServiceError dynamoDB
      "ReplicaAlreadyExistsException"

-- | The specified global table does not exist.
--
--
_GlobalTableNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalTableNotFoundException
  = _MatchServiceError dynamoDB
      "GlobalTableNotFoundException"

-- | The transaction with the given request token is already in progress.
--
--
_TransactionInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_TransactionInProgressException
  = _MatchServiceError dynamoDB
      "TransactionInProgressException"

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Programming.Errors.html#Programming.Errors.RetryAndBackoff Error Retries and Exponential Backoff> in the /Amazon DynamoDB Developer Guide/ .
--
--
_ProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedThroughputExceededException
  = _MatchServiceError dynamoDB
      "ProvisionedThroughputExceededException"

-- | The operation tried to access a nonexistent table or index. The resource might not be specified correctly, or its status might not be @ACTIVE@ .
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError dynamoDB
      "ResourceNotFoundException"

-- | An invalid restore time was specified. RestoreDateTime must be between EarliestRestorableDateTime and LatestRestorableDateTime.
--
--
_InvalidRestoreTimeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRestoreTimeException
  = _MatchServiceError dynamoDB
      "InvalidRestoreTimeException"

-- | Point in time recovery has not yet been enabled for this source table.
--
--
_PointInTimeRecoveryUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_PointInTimeRecoveryUnavailableException
  = _MatchServiceError dynamoDB
      "PointInTimeRecoveryUnavailableException"

-- | An error occurred on the server side.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError
  = _MatchServiceError dynamoDB "InternalServerError"

-- | A target table with the specified name already exists. 
--
--
_TableAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_TableAlreadyExistsException
  = _MatchServiceError dynamoDB
      "TableAlreadyExistsException"

-- | The specified global table already exists.
--
--
_GlobalTableAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_GlobalTableAlreadyExistsException
  = _MatchServiceError dynamoDB
      "GlobalTableAlreadyExistsException"

-- | A condition specified in the operation could not be evaluated.
--
--
_ConditionalCheckFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_ConditionalCheckFailedException
  = _MatchServiceError dynamoDB
      "ConditionalCheckFailedException"

-- | There is no limit to the number of daily on-demand backups that can be taken. 
--
--
-- Up to 50 simultaneous table operations are allowed per account. These operations include @CreateTable@ , @UpdateTable@ , @DeleteTable@ ,@UpdateTimeToLive@ , @RestoreTableFromBackup@ , and @RestoreTableToPointInTime@ . 
--
-- The only exception is when you are creating a table with one or more secondary indexes. You can have up to 25 such requests running at a time; however, if the table or index specifications are complex, DynamoDB might temporarily reduce the number of concurrent operations.
--
-- There is a soft account limit of 256 tables.
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError dynamoDB
      "LimitExceededException"

-- | The operation conflicts with the resource's availability. For example, you attempted to recreate an existing table, or tried to delete a table currently in the @CREATING@ state.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError dynamoDB
      "ResourceInUseException"
