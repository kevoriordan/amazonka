{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types
    (
    -- * Service Configuration
      glue

    -- * Errors
    , _NoScheduleException
    , _IdempotentParameterMismatchException
    , _CrawlerStoppingException
    , _ResourceNumberLimitExceededException
    , _ConcurrentRunsExceededException
    , _InternalServiceException
    , _SchedulerNotRunningException
    , _SchedulerTransitioningException
    , _SchedulerRunningException
    , _AccessDeniedException
    , _CrawlerRunningException
    , _ValidationException
    , _MLTransformNotReadyException
    , _CrawlerNotRunningException
    , _VersionMismatchException
    , _GlueEncryptionException
    , _InvalidInputException
    , _ConditionCheckFailureException
    , _ConcurrentModificationException
    , _EntityNotFoundException
    , _OperationTimeoutException
    , _AlreadyExistsException

    -- * CSVHeaderOption
    , CSVHeaderOption (..)

    -- * CatalogEncryptionMode
    , CatalogEncryptionMode (..)

    -- * CloudWatchEncryptionMode
    , CloudWatchEncryptionMode (..)

    -- * Comparator
    , Comparator (..)

    -- * ConnectionPropertyKey
    , ConnectionPropertyKey (..)

    -- * ConnectionType
    , ConnectionType (..)

    -- * CrawlState
    , CrawlState (..)

    -- * CrawlerState
    , CrawlerState (..)

    -- * DeleteBehavior
    , DeleteBehavior (..)

    -- * ExistCondition
    , ExistCondition (..)

    -- * JobBookmarksEncryptionMode
    , JobBookmarksEncryptionMode (..)

    -- * JobRunState
    , JobRunState (..)

    -- * Language
    , Language (..)

    -- * LastCrawlStatus
    , LastCrawlStatus (..)

    -- * Logical
    , Logical (..)

    -- * LogicalOperator
    , LogicalOperator (..)

    -- * NodeType
    , NodeType (..)

    -- * Permission
    , Permission (..)

    -- * PrincipalType
    , PrincipalType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * S3EncryptionMode
    , S3EncryptionMode (..)

    -- * ScheduleState
    , ScheduleState (..)

    -- * Sort
    , Sort (..)

    -- * SortDirectionType
    , SortDirectionType (..)

    -- * TaskRunSortColumnType
    , TaskRunSortColumnType (..)

    -- * TaskStatusType
    , TaskStatusType (..)

    -- * TaskType
    , TaskType (..)

    -- * TransformSortColumnType
    , TransformSortColumnType (..)

    -- * TransformStatusType
    , TransformStatusType (..)

    -- * TransformType
    , TransformType (..)

    -- * TriggerState
    , TriggerState (..)

    -- * TriggerType
    , TriggerType (..)

    -- * UpdateBehavior
    , UpdateBehavior (..)

    -- * WorkerType
    , WorkerType (..)

    -- * WorkflowRunStatus
    , WorkflowRunStatus (..)

    -- * Action
    , Action
    , action
    , aNotificationProperty
    , aArguments
    , aJobName
    , aSecurityConfiguration
    , aTimeout
    , aCrawlerName

    -- * BatchStopJobRunError
    , BatchStopJobRunError
    , batchStopJobRunError
    , bsjreJobName
    , bsjreJobRunId
    , bsjreErrorDetail

    -- * BatchStopJobRunSuccessfulSubmission
    , BatchStopJobRunSuccessfulSubmission
    , batchStopJobRunSuccessfulSubmission
    , bsjrssJobName
    , bsjrssJobRunId

    -- * CSVClassifier
    , CSVClassifier
    , csvClassifier
    , ccCreationTime
    , ccQuoteSymbol
    , ccContainsHeader
    , ccLastUpdated
    , ccDisableValueTrimming
    , ccHeader
    , ccVersion
    , ccAllowSingleColumn
    , ccDelimiter
    , ccName

    -- * CatalogEntry
    , CatalogEntry
    , catalogEntry
    , ceDatabaseName
    , ceTableName

    -- * CatalogImportStatus
    , CatalogImportStatus
    , catalogImportStatus
    , cisImportedBy
    , cisImportTime
    , cisImportCompleted

    -- * CatalogTarget
    , CatalogTarget
    , catalogTarget
    , ctDatabaseName
    , ctTables

    -- * Classifier
    , Classifier
    , classifier
    , cGrokClassifier
    , cXMLClassifier
    , cCSVClassifier
    , cJSONClassifier

    -- * CloudWatchEncryption
    , CloudWatchEncryption
    , cloudWatchEncryption
    , cweCloudWatchEncryptionMode
    , cweKMSKeyARN

    -- * CodeGenEdge
    , CodeGenEdge
    , codeGenEdge
    , cgeTargetParameter
    , cgeSource
    , cgeTarget

    -- * CodeGenNode
    , CodeGenNode
    , codeGenNode
    , cgnLineNumber
    , cgnId
    , cgnNodeType
    , cgnArgs

    -- * CodeGenNodeArg
    , CodeGenNodeArg
    , codeGenNodeArg
    , cgnaParam
    , cgnaName
    , cgnaValue

    -- * Column
    , Column
    , column
    , cParameters
    , cType
    , cComment
    , cName

    -- * Condition
    , Condition
    , condition
    , cCrawlState
    , cState
    , cJobName
    , cLogicalOperator
    , cCrawlerName

    -- * ConfusionMatrix
    , ConfusionMatrix
    , confusionMatrix
    , cmNumTrueNegatives
    , cmNumFalseNegatives
    , cmNumTruePositives
    , cmNumFalsePositives

    -- * Connection
    , Connection
    , connection
    , conCreationTime
    , conLastUpdatedBy
    , conConnectionProperties
    , conLastUpdatedTime
    , conMatchCriteria
    , conPhysicalConnectionRequirements
    , conName
    , conDescription
    , conConnectionType

    -- * ConnectionInput
    , ConnectionInput
    , connectionInput
    , ciMatchCriteria
    , ciPhysicalConnectionRequirements
    , ciDescription
    , ciName
    , ciConnectionType
    , ciConnectionProperties

    -- * ConnectionPasswordEncryption
    , ConnectionPasswordEncryption
    , connectionPasswordEncryption
    , cpeAWSKMSKeyId
    , cpeReturnConnectionPasswordEncrypted

    -- * ConnectionsList
    , ConnectionsList
    , connectionsList
    , clConnections

    -- * Crawl
    , Crawl
    , crawl
    , craCompletedOn
    , craState
    , craStartedOn
    , craLogStream
    , craLogGroup
    , craErrorMessage

    -- * Crawler
    , Crawler
    , crawler
    , crwlrCreationTime
    , crwlrState
    , crwlrSchemaChangePolicy
    , crwlrLastUpdated
    , crwlrSchedule
    , crwlrLastCrawl
    , crwlrCrawlElapsedTime
    , crwlrClassifiers
    , crwlrRole
    , crwlrName
    , crwlrTargets
    , crwlrVersion
    , crwlrDatabaseName
    , crwlrCrawlerSecurityConfiguration
    , crwlrConfiguration
    , crwlrTablePrefix
    , crwlrDescription

    -- * CrawlerMetrics
    , CrawlerMetrics
    , crawlerMetrics
    , cmLastRuntimeSeconds
    , cmTablesCreated
    , cmStillEstimating
    , cmMedianRuntimeSeconds
    , cmTimeLeftSeconds
    , cmTablesDeleted
    , cmTablesUpdated
    , cmCrawlerName

    -- * CrawlerNodeDetails
    , CrawlerNodeDetails
    , crawlerNodeDetails
    , cndCrawls

    -- * CrawlerTargets
    , CrawlerTargets
    , crawlerTargets
    , ctDynamoDBTargets
    , ctS3Targets
    , ctCatalogTargets
    , ctJdbcTargets

    -- * CreateCSVClassifierRequest
    , CreateCSVClassifierRequest
    , createCSVClassifierRequest
    , cccrQuoteSymbol
    , cccrContainsHeader
    , cccrDisableValueTrimming
    , cccrHeader
    , cccrAllowSingleColumn
    , cccrDelimiter
    , cccrName

    -- * CreateGrokClassifierRequest
    , CreateGrokClassifierRequest
    , createGrokClassifierRequest
    , cgcrCustomPatterns
    , cgcrClassification
    , cgcrName
    , cgcrGrokPattern

    -- * CreateJSONClassifierRequest
    , CreateJSONClassifierRequest
    , createJSONClassifierRequest
    , cjcrName
    , cjcrJSONPath

    -- * CreateXMLClassifierRequest
    , CreateXMLClassifierRequest
    , createXMLClassifierRequest
    , cxcrRowTag
    , cxcrClassification
    , cxcrName

    -- * DataCatalogEncryptionSettings
    , DataCatalogEncryptionSettings
    , dataCatalogEncryptionSettings
    , dcesEncryptionAtRest
    , dcesConnectionPasswordEncryption

    -- * DataLakePrincipal
    , DataLakePrincipal
    , dataLakePrincipal
    , dlpDataLakePrincipalIdentifier

    -- * Database
    , Database
    , database
    , dLocationURI
    , dParameters
    , dDescription
    , dCreateTime
    , dCreateTableDefaultPermissions
    , dName

    -- * DatabaseInput
    , DatabaseInput
    , databaseInput
    , diLocationURI
    , diParameters
    , diDescription
    , diCreateTableDefaultPermissions
    , diName

    -- * DevEndpoint
    , DevEndpoint
    , devEndpoint
    , deStatus
    , deFailureReason
    , deEndpointName
    , deNumberOfWorkers
    , deExtraPythonLibsS3Path
    , deLastUpdateStatus
    , deSecurityGroupIds
    , deLastModifiedTimestamp
    , dePublicKeys
    , deVPCId
    , deArguments
    , dePrivateAddress
    , deWorkerType
    , deSecurityConfiguration
    , dePublicKey
    , deSubnetId
    , deGlueVersion
    , deNumberOfNodes
    , dePublicAddress
    , deAvailabilityZone
    , deZeppelinRemoteSparkInterpreterPort
    , deExtraJARsS3Path
    , deCreatedTimestamp
    , deYarnEndpointAddress
    , deRoleARN

    -- * DevEndpointCustomLibraries
    , DevEndpointCustomLibraries
    , devEndpointCustomLibraries
    , declExtraPythonLibsS3Path
    , declExtraJARsS3Path

    -- * DynamoDBTarget
    , DynamoDBTarget
    , dynamoDBTarget
    , ddtPath

    -- * Edge
    , Edge
    , edge
    , eSourceId
    , eDestinationId

    -- * EncryptionAtRest
    , EncryptionAtRest
    , encryptionAtRest
    , earSseAWSKMSKeyId
    , earCatalogEncryptionMode

    -- * EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecS3Encryption
    , ecJobBookmarksEncryption
    , ecCloudWatchEncryption

    -- * ErrorDetail
    , ErrorDetail
    , errorDetail
    , edErrorCode
    , edErrorMessage

    -- * EvaluationMetrics
    , EvaluationMetrics
    , evaluationMetrics
    , emFindMatchesMetrics
    , emTransformType

    -- * ExecutionProperty
    , ExecutionProperty
    , executionProperty
    , epMaxConcurrentRuns

    -- * ExportLabelsTaskRunProperties
    , ExportLabelsTaskRunProperties
    , exportLabelsTaskRunProperties
    , eltrpOutputS3Path

    -- * FindMatchesMetrics
    , FindMatchesMetrics
    , findMatchesMetrics
    , fmmF1
    , fmmAreaUnderPRCurve
    , fmmRecall
    , fmmPrecision
    , fmmConfusionMatrix

    -- * FindMatchesParameters
    , FindMatchesParameters
    , findMatchesParameters
    , fmpEnforceProvidedLabels
    , fmpAccuracyCostTradeoff
    , fmpPrecisionRecallTradeoff
    , fmpPrimaryKeyColumnName

    -- * FindMatchesTaskRunProperties
    , FindMatchesTaskRunProperties
    , findMatchesTaskRunProperties
    , fmtrpJobId
    , fmtrpJobName
    , fmtrpJobRunId

    -- * GetConnectionsFilter
    , GetConnectionsFilter
    , getConnectionsFilter
    , gcfMatchCriteria
    , gcfConnectionType

    -- * GlueTable
    , GlueTable
    , glueTable
    , gtCatalogId
    , gtConnectionName
    , gtDatabaseName
    , gtTableName

    -- * GrokClassifier
    , GrokClassifier
    , grokClassifier
    , gcCreationTime
    , gcLastUpdated
    , gcVersion
    , gcCustomPatterns
    , gcName
    , gcClassification
    , gcGrokPattern

    -- * ImportLabelsTaskRunProperties
    , ImportLabelsTaskRunProperties
    , importLabelsTaskRunProperties
    , iltrpReplace
    , iltrpInputS3Path

    -- * JSONClassifier
    , JSONClassifier
    , jsonClassifier
    , jcCreationTime
    , jcLastUpdated
    , jcVersion
    , jcName
    , jcJSONPath

    -- * JdbcTarget
    , JdbcTarget
    , jdbcTarget
    , jtPath
    , jtConnectionName
    , jtExclusions

    -- * Job
    , Job
    , job
    , jNumberOfWorkers
    , jCommand
    , jNotificationProperty
    , jLastModifiedOn
    , jConnections
    , jWorkerType
    , jSecurityConfiguration
    , jGlueVersion
    , jNonOverridableArguments
    , jRole
    , jName
    , jLogURI
    , jMaxRetries
    , jExecutionProperty
    , jAllocatedCapacity
    , jMaxCapacity
    , jTimeout
    , jDefaultArguments
    , jDescription
    , jCreatedOn

    -- * JobBookmarkEntry
    , JobBookmarkEntry
    , jobBookmarkEntry
    , jbeJobName
    , jbeRun
    , jbeRunId
    , jbeVersion
    , jbePreviousRunId
    , jbeAttempt
    , jbeJobBookmark

    -- * JobBookmarksEncryption
    , JobBookmarksEncryption
    , jobBookmarksEncryption
    , jbeJobBookmarksEncryptionMode
    , jbeKMSKeyARN

    -- * JobCommand
    , JobCommand
    , jobCommand
    , jobScriptLocation
    , jobPythonVersion
    , jobName

    -- * JobNodeDetails
    , JobNodeDetails
    , jobNodeDetails
    , jndJobRuns

    -- * JobRun
    , JobRun
    , jobRun
    , jrCompletedOn
    , jrNumberOfWorkers
    , jrTriggerName
    , jrNotificationProperty
    , jrLastModifiedOn
    , jrArguments
    , jrJobName
    , jrStartedOn
    , jrWorkerType
    , jrSecurityConfiguration
    , jrGlueVersion
    , jrJobRunState
    , jrLogGroupName
    , jrExecutionTime
    , jrPredecessorRuns
    , jrPreviousRunId
    , jrId
    , jrAttempt
    , jrAllocatedCapacity
    , jrMaxCapacity
    , jrTimeout
    , jrErrorMessage

    -- * JobUpdate
    , JobUpdate
    , jobUpdate
    , juNumberOfWorkers
    , juCommand
    , juNotificationProperty
    , juConnections
    , juWorkerType
    , juSecurityConfiguration
    , juGlueVersion
    , juNonOverridableArguments
    , juRole
    , juLogURI
    , juMaxRetries
    , juExecutionProperty
    , juAllocatedCapacity
    , juMaxCapacity
    , juTimeout
    , juDefaultArguments
    , juDescription

    -- * LabelingSetGenerationTaskRunProperties
    , LabelingSetGenerationTaskRunProperties
    , labelingSetGenerationTaskRunProperties
    , lsgtrpOutputS3Path

    -- * LastCrawlInfo
    , LastCrawlInfo
    , lastCrawlInfo
    , lciStatus
    , lciStartTime
    , lciLogStream
    , lciLogGroup
    , lciMessagePrefix
    , lciErrorMessage

    -- * Location
    , Location
    , location
    , lDynamoDB
    , lJdbc
    , lS3

    -- * MLTransform
    , MLTransform
    , mLTransform
    , mltStatus
    , mltNumberOfWorkers
    , mltLastModifiedOn
    , mltLabelCount
    , mltWorkerType
    , mltInputRecordTables
    , mltGlueVersion
    , mltEvaluationMetrics
    , mltSchema
    , mltRole
    , mltName
    , mltParameters
    , mltMaxRetries
    , mltMaxCapacity
    , mltTimeout
    , mltDescription
    , mltCreatedOn
    , mltTransformId

    -- * MappingEntry
    , MappingEntry
    , mappingEntry
    , meTargetTable
    , meSourceType
    , meSourceTable
    , meTargetType
    , meTargetPath
    , meSourcePath

    -- * Node
    , Node
    , node
    , nTriggerDetails
    , nUniqueId
    , nCrawlerDetails
    , nName
    , nJobDetails
    , nType

    -- * NotificationProperty
    , NotificationProperty
    , notificationProperty
    , npNotifyDelayAfter

    -- * Order
    , Order
    , order
    , oColumn
    , oSortOrder

    -- * Partition
    , Partition
    , partition
    , pCreationTime
    , pValues
    , pLastAnalyzedTime
    , pStorageDescriptor
    , pDatabaseName
    , pParameters
    , pLastAccessTime
    , pTableName

    -- * PartitionError
    , PartitionError
    , partitionError
    , pePartitionValues
    , peErrorDetail

    -- * PartitionInput
    , PartitionInput
    , partitionInput
    , piValues
    , piLastAnalyzedTime
    , piStorageDescriptor
    , piParameters
    , piLastAccessTime

    -- * PartitionValueList
    , PartitionValueList
    , partitionValueList
    , pvlValues

    -- * PhysicalConnectionRequirements
    , PhysicalConnectionRequirements
    , physicalConnectionRequirements
    , pcrSecurityGroupIdList
    , pcrSubnetId
    , pcrAvailabilityZone

    -- * Predecessor
    , Predecessor
    , predecessor
    , pJobName
    , pRunId

    -- * Predicate
    , Predicate
    , predicate
    , pLogical
    , pConditions

    -- * PrincipalPermissions
    , PrincipalPermissions
    , principalPermissions
    , ppPrincipal
    , ppPermissions

    -- * PropertyPredicate
    , PropertyPredicate
    , propertyPredicate
    , ppValue
    , ppKey
    , ppComparator

    -- * ResourceURI
    , ResourceURI
    , resourceURI
    , ruResourceType
    , ruURI

    -- * S3Encryption
    , S3Encryption
    , s3Encryption
    , seS3EncryptionMode
    , seKMSKeyARN

    -- * S3Target
    , S3Target
    , s3Target
    , stPath
    , stExclusions

    -- * Schedule
    , Schedule
    , schedule
    , sState
    , sScheduleExpression

    -- * SchemaChangePolicy
    , SchemaChangePolicy
    , schemaChangePolicy
    , scpDeleteBehavior
    , scpUpdateBehavior

    -- * SchemaColumn
    , SchemaColumn
    , schemaColumn
    , sName
    , sDataType

    -- * SecurityConfiguration
    , SecurityConfiguration
    , securityConfiguration
    , secName
    , secEncryptionConfiguration
    , secCreatedTimeStamp

    -- * Segment
    , Segment
    , segment
    , sSegmentNumber
    , sTotalSegments

    -- * SerDeInfo
    , SerDeInfo
    , serDeInfo
    , sdiSerializationLibrary
    , sdiName
    , sdiParameters

    -- * SkewedInfo
    , SkewedInfo
    , skewedInfo
    , siSkewedColumnValueLocationMaps
    , siSkewedColumnValues
    , siSkewedColumnNames

    -- * SortCriterion
    , SortCriterion
    , sortCriterion
    , scSort
    , scFieldName

    -- * StorageDescriptor
    , StorageDescriptor
    , storageDescriptor
    , sdSortColumns
    , sdCompressed
    , sdLocation
    , sdBucketColumns
    , sdSerdeInfo
    , sdOutputFormat
    , sdNumberOfBuckets
    , sdStoredAsSubDirectories
    , sdParameters
    , sdInputFormat
    , sdSkewedInfo
    , sdColumns

    -- * Table
    , Table
    , table
    , tRetention
    , tIsRegisteredWithLakeFormation
    , tCreatedBy
    , tTableType
    , tOwner
    , tViewOriginalText
    , tUpdateTime
    , tViewExpandedText
    , tLastAnalyzedTime
    , tStorageDescriptor
    , tDatabaseName
    , tParameters
    , tLastAccessTime
    , tDescription
    , tPartitionKeys
    , tCreateTime
    , tName

    -- * TableError
    , TableError
    , tableError
    , teTableName
    , teErrorDetail

    -- * TableInput
    , TableInput
    , tableInput
    , tiRetention
    , tiTableType
    , tiOwner
    , tiViewOriginalText
    , tiViewExpandedText
    , tiLastAnalyzedTime
    , tiStorageDescriptor
    , tiParameters
    , tiLastAccessTime
    , tiDescription
    , tiPartitionKeys
    , tiName

    -- * TableVersion
    , TableVersion
    , tableVersion
    , tvVersionId
    , tvTable

    -- * TableVersionError
    , TableVersionError
    , tableVersionError
    , tveVersionId
    , tveTableName
    , tveErrorDetail

    -- * TaskRun
    , TaskRun
    , taskRun
    , trCompletedOn
    , trStatus
    , trLastModifiedOn
    , trErrorString
    , trStartedOn
    , trLogGroupName
    , trExecutionTime
    , trProperties
    , trTransformId
    , trTaskRunId

    -- * TaskRunFilterCriteria
    , TaskRunFilterCriteria
    , taskRunFilterCriteria
    , trfcStatus
    , trfcStartedAfter
    , trfcStartedBefore
    , trfcTaskRunType

    -- * TaskRunProperties
    , TaskRunProperties
    , taskRunProperties
    , trpTaskType
    , trpExportLabelsTaskRunProperties
    , trpLabelingSetGenerationTaskRunProperties
    , trpFindMatchesTaskRunProperties
    , trpImportLabelsTaskRunProperties

    -- * TaskRunSortCriteria
    , TaskRunSortCriteria
    , taskRunSortCriteria
    , trscColumn
    , trscSortDirection

    -- * TransformFilterCriteria
    , TransformFilterCriteria
    , transformFilterCriteria
    , tfcCreatedAfter
    , tfcStatus
    , tfcLastModifiedAfter
    , tfcLastModifiedBefore
    , tfcGlueVersion
    , tfcSchema
    , tfcTransformType
    , tfcName
    , tfcCreatedBefore

    -- * TransformParameters
    , TransformParameters
    , transformParameters
    , tpFindMatchesParameters
    , tpTransformType

    -- * TransformSortCriteria
    , TransformSortCriteria
    , transformSortCriteria
    , tscColumn
    , tscSortDirection

    -- * Trigger
    , Trigger
    , trigger
    , triWorkflowName
    , triState
    , triActions
    , triSchedule
    , triPredicate
    , triName
    , triId
    , triType
    , triDescription

    -- * TriggerNodeDetails
    , TriggerNodeDetails
    , triggerNodeDetails
    , tndTrigger

    -- * TriggerUpdate
    , TriggerUpdate
    , triggerUpdate
    , tuActions
    , tuSchedule
    , tuPredicate
    , tuName
    , tuDescription

    -- * UpdateCSVClassifierRequest
    , UpdateCSVClassifierRequest
    , updateCSVClassifierRequest
    , uccrQuoteSymbol
    , uccrContainsHeader
    , uccrDisableValueTrimming
    , uccrHeader
    , uccrAllowSingleColumn
    , uccrDelimiter
    , uccrName

    -- * UpdateGrokClassifierRequest
    , UpdateGrokClassifierRequest
    , updateGrokClassifierRequest
    , ugcrClassification
    , ugcrCustomPatterns
    , ugcrGrokPattern
    , ugcrName

    -- * UpdateJSONClassifierRequest
    , UpdateJSONClassifierRequest
    , updateJSONClassifierRequest
    , ujcrJSONPath
    , ujcrName

    -- * UpdateXMLClassifierRequest
    , UpdateXMLClassifierRequest
    , updateXMLClassifierRequest
    , uxcrClassification
    , uxcrRowTag
    , uxcrName

    -- * UserDefinedFunction
    , UserDefinedFunction
    , userDefinedFunction
    , udfOwnerName
    , udfResourceURIs
    , udfFunctionName
    , udfOwnerType
    , udfCreateTime
    , udfClassName

    -- * UserDefinedFunctionInput
    , UserDefinedFunctionInput
    , userDefinedFunctionInput
    , udfiOwnerName
    , udfiResourceURIs
    , udfiFunctionName
    , udfiOwnerType
    , udfiClassName

    -- * Workflow
    , Workflow
    , workflow
    , wGraph
    , wLastModifiedOn
    , wDefaultRunProperties
    , wName
    , wLastRun
    , wDescription
    , wCreatedOn

    -- * WorkflowGraph
    , WorkflowGraph
    , workflowGraph
    , wgEdges
    , wgNodes

    -- * WorkflowRun
    , WorkflowRun
    , workflowRun
    , wrCompletedOn
    , wrStatus
    , wrGraph
    , wrStartedOn
    , wrWorkflowRunId
    , wrName
    , wrStatistics
    , wrWorkflowRunProperties

    -- * WorkflowRunStatistics
    , WorkflowRunStatistics
    , workflowRunStatistics
    , wrsRunningActions
    , wrsStoppedActions
    , wrsTotalActions
    , wrsFailedActions
    , wrsTimeoutActions
    , wrsSucceededActions

    -- * XMLClassifier
    , XMLClassifier
    , xmlClassifier
    , xcCreationTime
    , xcLastUpdated
    , xcVersion
    , xcRowTag
    , xcName
    , xcClassification
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Glue.Types.CSVHeaderOption
import Network.AWS.Glue.Types.CatalogEncryptionMode
import Network.AWS.Glue.Types.CloudWatchEncryptionMode
import Network.AWS.Glue.Types.Comparator
import Network.AWS.Glue.Types.ConnectionPropertyKey
import Network.AWS.Glue.Types.ConnectionType
import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.CrawlerState
import Network.AWS.Glue.Types.DeleteBehavior
import Network.AWS.Glue.Types.ExistCondition
import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.Language
import Network.AWS.Glue.Types.LastCrawlStatus
import Network.AWS.Glue.Types.Logical
import Network.AWS.Glue.Types.LogicalOperator
import Network.AWS.Glue.Types.NodeType
import Network.AWS.Glue.Types.Permission
import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceType
import Network.AWS.Glue.Types.S3EncryptionMode
import Network.AWS.Glue.Types.ScheduleState
import Network.AWS.Glue.Types.Sort
import Network.AWS.Glue.Types.SortDirectionType
import Network.AWS.Glue.Types.TaskRunSortColumnType
import Network.AWS.Glue.Types.TaskStatusType
import Network.AWS.Glue.Types.TaskType
import Network.AWS.Glue.Types.TransformSortColumnType
import Network.AWS.Glue.Types.TransformStatusType
import Network.AWS.Glue.Types.TransformType
import Network.AWS.Glue.Types.TriggerState
import Network.AWS.Glue.Types.TriggerType
import Network.AWS.Glue.Types.UpdateBehavior
import Network.AWS.Glue.Types.WorkerType
import Network.AWS.Glue.Types.WorkflowRunStatus
import Network.AWS.Glue.Types.Action
import Network.AWS.Glue.Types.BatchStopJobRunError
import Network.AWS.Glue.Types.BatchStopJobRunSuccessfulSubmission
import Network.AWS.Glue.Types.CSVClassifier
import Network.AWS.Glue.Types.CatalogEntry
import Network.AWS.Glue.Types.CatalogImportStatus
import Network.AWS.Glue.Types.CatalogTarget
import Network.AWS.Glue.Types.Classifier
import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.CodeGenEdge
import Network.AWS.Glue.Types.CodeGenNode
import Network.AWS.Glue.Types.CodeGenNodeArg
import Network.AWS.Glue.Types.Column
import Network.AWS.Glue.Types.Condition
import Network.AWS.Glue.Types.ConfusionMatrix
import Network.AWS.Glue.Types.Connection
import Network.AWS.Glue.Types.ConnectionInput
import Network.AWS.Glue.Types.ConnectionPasswordEncryption
import Network.AWS.Glue.Types.ConnectionsList
import Network.AWS.Glue.Types.Crawl
import Network.AWS.Glue.Types.Crawler
import Network.AWS.Glue.Types.CrawlerMetrics
import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.CrawlerTargets
import Network.AWS.Glue.Types.CreateCSVClassifierRequest
import Network.AWS.Glue.Types.CreateGrokClassifierRequest
import Network.AWS.Glue.Types.CreateJSONClassifierRequest
import Network.AWS.Glue.Types.CreateXMLClassifierRequest
import Network.AWS.Glue.Types.DataCatalogEncryptionSettings
import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Database
import Network.AWS.Glue.Types.DatabaseInput
import Network.AWS.Glue.Types.DevEndpoint
import Network.AWS.Glue.Types.DevEndpointCustomLibraries
import Network.AWS.Glue.Types.DynamoDBTarget
import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.EncryptionAtRest
import Network.AWS.Glue.Types.EncryptionConfiguration
import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Glue.Types.EvaluationMetrics
import Network.AWS.Glue.Types.ExecutionProperty
import Network.AWS.Glue.Types.ExportLabelsTaskRunProperties
import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.FindMatchesParameters
import Network.AWS.Glue.Types.FindMatchesTaskRunProperties
import Network.AWS.Glue.Types.GetConnectionsFilter
import Network.AWS.Glue.Types.GlueTable
import Network.AWS.Glue.Types.GrokClassifier
import Network.AWS.Glue.Types.ImportLabelsTaskRunProperties
import Network.AWS.Glue.Types.JSONClassifier
import Network.AWS.Glue.Types.JdbcTarget
import Network.AWS.Glue.Types.Job
import Network.AWS.Glue.Types.JobBookmarkEntry
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.JobCommand
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.JobRun
import Network.AWS.Glue.Types.JobUpdate
import Network.AWS.Glue.Types.LabelingSetGenerationTaskRunProperties
import Network.AWS.Glue.Types.LastCrawlInfo
import Network.AWS.Glue.Types.Location
import Network.AWS.Glue.Types.MLTransform
import Network.AWS.Glue.Types.MappingEntry
import Network.AWS.Glue.Types.Node
import Network.AWS.Glue.Types.NotificationProperty
import Network.AWS.Glue.Types.Order
import Network.AWS.Glue.Types.Partition
import Network.AWS.Glue.Types.PartitionError
import Network.AWS.Glue.Types.PartitionInput
import Network.AWS.Glue.Types.PartitionValueList
import Network.AWS.Glue.Types.PhysicalConnectionRequirements
import Network.AWS.Glue.Types.Predecessor
import Network.AWS.Glue.Types.Predicate
import Network.AWS.Glue.Types.PrincipalPermissions
import Network.AWS.Glue.Types.PropertyPredicate
import Network.AWS.Glue.Types.ResourceURI
import Network.AWS.Glue.Types.S3Encryption
import Network.AWS.Glue.Types.S3Target
import Network.AWS.Glue.Types.Schedule
import Network.AWS.Glue.Types.SchemaChangePolicy
import Network.AWS.Glue.Types.SchemaColumn
import Network.AWS.Glue.Types.SecurityConfiguration
import Network.AWS.Glue.Types.Segment
import Network.AWS.Glue.Types.SerDeInfo
import Network.AWS.Glue.Types.SkewedInfo
import Network.AWS.Glue.Types.SortCriterion
import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Glue.Types.Table
import Network.AWS.Glue.Types.TableError
import Network.AWS.Glue.Types.TableInput
import Network.AWS.Glue.Types.TableVersion
import Network.AWS.Glue.Types.TableVersionError
import Network.AWS.Glue.Types.TaskRun
import Network.AWS.Glue.Types.TaskRunFilterCriteria
import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskRunSortCriteria
import Network.AWS.Glue.Types.TransformFilterCriteria
import Network.AWS.Glue.Types.TransformParameters
import Network.AWS.Glue.Types.TransformSortCriteria
import Network.AWS.Glue.Types.Trigger
import Network.AWS.Glue.Types.TriggerNodeDetails
import Network.AWS.Glue.Types.TriggerUpdate
import Network.AWS.Glue.Types.UpdateCSVClassifierRequest
import Network.AWS.Glue.Types.UpdateGrokClassifierRequest
import Network.AWS.Glue.Types.UpdateJSONClassifierRequest
import Network.AWS.Glue.Types.UpdateXMLClassifierRequest
import Network.AWS.Glue.Types.UserDefinedFunction
import Network.AWS.Glue.Types.UserDefinedFunctionInput
import Network.AWS.Glue.Types.Workflow
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.XMLClassifier

-- | API version @2017-03-31@ of the Amazon Glue SDK configuration.
glue :: Service
glue
  = Service{_svcAbbrev = "Glue", _svcSigner = v4,
            _svcPrefix = "glue", _svcVersion = "2017-03-31",
            _svcEndpoint = defaultEndpoint glue,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Glue", _svcRetry = retry}
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

-- | There is no applicable schedule.
--
--
_NoScheduleException :: AsError a => Getting (First ServiceError) a ServiceError
_NoScheduleException
  = _MatchServiceError glue "NoScheduleException"

-- | The same unique identifier was associated with two different records.
--
--
_IdempotentParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotentParameterMismatchException
  = _MatchServiceError glue
      "IdempotentParameterMismatchException"

-- | The specified crawler is stopping.
--
--
_CrawlerStoppingException :: AsError a => Getting (First ServiceError) a ServiceError
_CrawlerStoppingException
  = _MatchServiceError glue "CrawlerStoppingException"

-- | A resource numerical limit was exceeded.
--
--
_ResourceNumberLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNumberLimitExceededException
  = _MatchServiceError glue
      "ResourceNumberLimitExceededException"

-- | Too many jobs are being run concurrently.
--
--
_ConcurrentRunsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentRunsExceededException
  = _MatchServiceError glue
      "ConcurrentRunsExceededException"

-- | An internal service error occurred.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException
  = _MatchServiceError glue "InternalServiceException"

-- | The specified scheduler is not running.
--
--
_SchedulerNotRunningException :: AsError a => Getting (First ServiceError) a ServiceError
_SchedulerNotRunningException
  = _MatchServiceError glue
      "SchedulerNotRunningException"

-- | The specified scheduler is transitioning.
--
--
_SchedulerTransitioningException :: AsError a => Getting (First ServiceError) a ServiceError
_SchedulerTransitioningException
  = _MatchServiceError glue
      "SchedulerTransitioningException"

-- | The specified scheduler is already running.
--
--
_SchedulerRunningException :: AsError a => Getting (First ServiceError) a ServiceError
_SchedulerRunningException
  = _MatchServiceError glue "SchedulerRunningException"

-- | Access to a resource was denied.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError glue "AccessDeniedException"

-- | The operation cannot be performed because the crawler is already running.
--
--
_CrawlerRunningException :: AsError a => Getting (First ServiceError) a ServiceError
_CrawlerRunningException
  = _MatchServiceError glue "CrawlerRunningException"

-- | A value could not be validated.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException
  = _MatchServiceError glue "ValidationException"

-- | The machine learning transform is not ready to run.
--
--
_MLTransformNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_MLTransformNotReadyException
  = _MatchServiceError glue
      "MLTransformNotReadyException"

-- | The specified crawler is not running.
--
--
_CrawlerNotRunningException :: AsError a => Getting (First ServiceError) a ServiceError
_CrawlerNotRunningException
  = _MatchServiceError glue
      "CrawlerNotRunningException"

-- | There was a version conflict.
--
--
_VersionMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_VersionMismatchException
  = _MatchServiceError glue "VersionMismatchException"

-- | An encryption operation failed.
--
--
_GlueEncryptionException :: AsError a => Getting (First ServiceError) a ServiceError
_GlueEncryptionException
  = _MatchServiceError glue "GlueEncryptionException"

-- | The input provided was not valid.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError glue "InvalidInputException"

-- | A specified condition was not satisfied.
--
--
_ConditionCheckFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ConditionCheckFailureException
  = _MatchServiceError glue
      "ConditionCheckFailureException"

-- | Two processes are trying to modify a resource simultaneously.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException
  = _MatchServiceError glue
      "ConcurrentModificationException"

-- | A specified entity does not exist
--
--
_EntityNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EntityNotFoundException
  = _MatchServiceError glue "EntityNotFoundException"

-- | The operation timed out.
--
--
_OperationTimeoutException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationTimeoutException
  = _MatchServiceError glue "OperationTimeoutException"

-- | A resource to be created or added already exists.
--
--
_AlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyExistsException
  = _MatchServiceError glue "AlreadyExistsException"
