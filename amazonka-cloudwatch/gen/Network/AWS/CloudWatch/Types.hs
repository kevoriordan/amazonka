{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types
    (
    -- * Service Configuration
      cloudWatch

    -- * Errors
    , _InvalidFormatFault
    , _InvalidParameterValueException
    , _ResourceNotFoundException
    , _MissingRequiredParameterException
    , _DashboardInvalidInputError
    , _InternalServiceFault
    , _InvalidNextToken
    , _ConcurrentModificationException
    , _DashboardNotFoundError
    , _LimitExceededFault
    , _LimitExceededException
    , _ResourceNotFound
    , _InvalidParameterCombinationException

    -- * AlarmType
    , AlarmType (..)

    -- * AnomalyDetectorStateValue
    , AnomalyDetectorStateValue (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * HistoryItemType
    , HistoryItemType (..)

    -- * ScanBy
    , ScanBy (..)

    -- * StandardUnit
    , StandardUnit (..)

    -- * StateValue
    , StateValue (..)

    -- * Statistic
    , Statistic (..)

    -- * StatusCode
    , StatusCode (..)

    -- * AlarmHistoryItem
    , AlarmHistoryItem
    , alarmHistoryItem
    , ahiAlarmName
    , ahiHistoryItemType
    , ahiHistoryData
    , ahiAlarmType
    , ahiHistorySummary
    , ahiTimestamp

    -- * AnomalyDetector
    , AnomalyDetector
    , anomalyDetector
    , adMetricName
    , adNamespace
    , adStateValue
    , adStat
    , adConfiguration
    , adDimensions

    -- * AnomalyDetectorConfiguration
    , AnomalyDetectorConfiguration
    , anomalyDetectorConfiguration
    , adcMetricTimezone
    , adcExcludedTimeRanges

    -- * CompositeAlarm
    , CompositeAlarm
    , compositeAlarm
    , caAlarmName
    , caStateUpdatedTimestamp
    , caAlarmDescription
    , caAlarmRule
    , caOKActions
    , caStateValue
    , caAlarmConfigurationUpdatedTimestamp
    , caActionsEnabled
    , caInsufficientDataActions
    , caStateReason
    , caStateReasonData
    , caAlarmARN
    , caAlarmActions

    -- * DashboardEntry
    , DashboardEntry
    , dashboardEntry
    , deSize
    , deDashboardName
    , deLastModified
    , deDashboardARN

    -- * DashboardValidationMessage
    , DashboardValidationMessage
    , dashboardValidationMessage
    , dvmDataPath
    , dvmMessage

    -- * Datapoint
    , Datapoint
    , datapoint
    , dSampleCount
    , dMaximum
    , dAverage
    , dMinimum
    , dExtendedStatistics
    , dSum
    , dUnit
    , dTimestamp

    -- * Dimension
    , Dimension
    , dimension
    , dName
    , dValue

    -- * DimensionFilter
    , DimensionFilter
    , dimensionFilter
    , dfValue
    , dfName

    -- * InsightRule
    , InsightRule
    , insightRule
    , irName
    , irState
    , irSchema
    , irDefinition

    -- * InsightRuleContributor
    , InsightRuleContributor
    , insightRuleContributor
    , ircKeys
    , ircApproximateAggregateValue
    , ircDatapoints

    -- * InsightRuleContributorDatapoint
    , InsightRuleContributorDatapoint
    , insightRuleContributorDatapoint
    , ircdTimestamp
    , ircdApproximateValue

    -- * InsightRuleMetricDatapoint
    , InsightRuleMetricDatapoint
    , insightRuleMetricDatapoint
    , irmdMaxContributorValue
    , irmdSampleCount
    , irmdMaximum
    , irmdAverage
    , irmdMinimum
    , irmdUniqueContributors
    , irmdSum
    , irmdTimestamp

    -- * MessageData
    , MessageData
    , messageData
    , mValue
    , mCode

    -- * Metric
    , Metric
    , metric
    , mMetricName
    , mNamespace
    , mDimensions

    -- * MetricAlarm
    , MetricAlarm
    , metricAlarm
    , maAlarmName
    , maStateUpdatedTimestamp
    , maMetrics
    , maTreatMissingData
    , maPeriod
    , maAlarmDescription
    , maEvaluationPeriods
    , maMetricName
    , maNamespace
    , maThresholdMetricId
    , maComparisonOperator
    , maOKActions
    , maEvaluateLowSampleCountPercentile
    , maStateValue
    , maDatapointsToAlarm
    , maThreshold
    , maAlarmConfigurationUpdatedTimestamp
    , maActionsEnabled
    , maInsufficientDataActions
    , maStateReason
    , maStateReasonData
    , maDimensions
    , maAlarmARN
    , maAlarmActions
    , maUnit
    , maStatistic
    , maExtendedStatistic

    -- * MetricDataQuery
    , MetricDataQuery
    , metricDataQuery
    , mdqReturnData
    , mdqPeriod
    , mdqExpression
    , mdqLabel
    , mdqMetricStat
    , mdqId

    -- * MetricDataResult
    , MetricDataResult
    , metricDataResult
    , mdrValues
    , mdrId
    , mdrTimestamps
    , mdrMessages
    , mdrLabel
    , mdrStatusCode

    -- * MetricDatum
    , MetricDatum
    , metricDatum
    , mdValues
    , mdCounts
    , mdValue
    , mdStorageResolution
    , mdDimensions
    , mdUnit
    , mdTimestamp
    , mdStatisticValues
    , mdMetricName

    -- * MetricStat
    , MetricStat
    , metricStat
    , msUnit
    , msMetric
    , msPeriod
    , msStat

    -- * PartialFailure
    , PartialFailure
    , partialFailure
    , pfFailureResource
    , pfFailureCode
    , pfFailureDescription
    , pfExceptionType

    -- * Range
    , Range
    , range
    , rStartTime
    , rEndTime

    -- * StatisticSet
    , StatisticSet
    , statisticSet
    , ssSampleCount
    , ssSum
    , ssMinimum
    , ssMaximum

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.CloudWatch.Types.AlarmType
import Network.AWS.CloudWatch.Types.AnomalyDetectorStateValue
import Network.AWS.CloudWatch.Types.ComparisonOperator
import Network.AWS.CloudWatch.Types.HistoryItemType
import Network.AWS.CloudWatch.Types.ScanBy
import Network.AWS.CloudWatch.Types.StandardUnit
import Network.AWS.CloudWatch.Types.StateValue
import Network.AWS.CloudWatch.Types.Statistic
import Network.AWS.CloudWatch.Types.StatusCode
import Network.AWS.CloudWatch.Types.AlarmHistoryItem
import Network.AWS.CloudWatch.Types.AnomalyDetector
import Network.AWS.CloudWatch.Types.AnomalyDetectorConfiguration
import Network.AWS.CloudWatch.Types.CompositeAlarm
import Network.AWS.CloudWatch.Types.DashboardEntry
import Network.AWS.CloudWatch.Types.DashboardValidationMessage
import Network.AWS.CloudWatch.Types.Datapoint
import Network.AWS.CloudWatch.Types.Dimension
import Network.AWS.CloudWatch.Types.DimensionFilter
import Network.AWS.CloudWatch.Types.InsightRule
import Network.AWS.CloudWatch.Types.InsightRuleContributor
import Network.AWS.CloudWatch.Types.InsightRuleContributorDatapoint
import Network.AWS.CloudWatch.Types.InsightRuleMetricDatapoint
import Network.AWS.CloudWatch.Types.MessageData
import Network.AWS.CloudWatch.Types.Metric
import Network.AWS.CloudWatch.Types.MetricAlarm
import Network.AWS.CloudWatch.Types.MetricDataQuery
import Network.AWS.CloudWatch.Types.MetricDataResult
import Network.AWS.CloudWatch.Types.MetricDatum
import Network.AWS.CloudWatch.Types.MetricStat
import Network.AWS.CloudWatch.Types.PartialFailure
import Network.AWS.CloudWatch.Types.Range
import Network.AWS.CloudWatch.Types.StatisticSet
import Network.AWS.CloudWatch.Types.Tag

-- | API version @2010-08-01@ of the Amazon CloudWatch SDK configuration.
cloudWatch :: Service
cloudWatch
  = Service{_svcAbbrev = "CloudWatch", _svcSigner = v4,
            _svcPrefix = "monitoring",
            _svcVersion = "2010-08-01",
            _svcEndpoint = defaultEndpoint cloudWatch,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseXMLError "CloudWatch",
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

-- | Data was not syntactically valid JSON.
--
--
_InvalidFormatFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFormatFault
  = _MatchServiceError cloudWatch "InvalidFormat" .
      hasStatus 400

-- | The value of an input parameter is bad or out-of-range.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException
  = _MatchServiceError cloudWatch
      "InvalidParameterValue"
      . hasStatus 400

-- | The named resource does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError cloudWatch
      "ResourceNotFoundException"
      . hasStatus 404

-- | An input parameter that is required is missing.
--
--
_MissingRequiredParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingRequiredParameterException
  = _MatchServiceError cloudWatch "MissingParameter" .
      hasStatus 400

-- | Some part of the dashboard data is invalid.
--
--
_DashboardInvalidInputError :: AsError a => Getting (First ServiceError) a ServiceError
_DashboardInvalidInputError
  = _MatchServiceError cloudWatch
      "InvalidParameterInput"
      . hasStatus 400

-- | Request processing has failed due to some unknown error, exception, or failure.
--
--
_InternalServiceFault :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceFault
  = _MatchServiceError cloudWatch
      "InternalServiceError"
      . hasStatus 500

-- | The next token specified is invalid.
--
--
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken
  = _MatchServiceError cloudWatch "InvalidNextToken" .
      hasStatus 400

-- | More than one process tried to modify a resource at the same time.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException
  = _MatchServiceError cloudWatch
      "ConcurrentModificationException"
      . hasStatus 429

-- | The specified dashboard does not exist.
--
--
_DashboardNotFoundError :: AsError a => Getting (First ServiceError) a ServiceError
_DashboardNotFoundError
  = _MatchServiceError cloudWatch "ResourceNotFound" .
      hasStatus 404

-- | The quota for alarms for this customer has already been reached.
--
--
_LimitExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededFault
  = _MatchServiceError cloudWatch "LimitExceeded" .
      hasStatus 400

-- | The operation exceeded one or more limits.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError cloudWatch
      "LimitExceededException"
      . hasStatus 400

-- | The named resource does not exist.
--
--
_ResourceNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFound
  = _MatchServiceError cloudWatch "ResourceNotFound" .
      hasStatus 404

-- | Parameters were used together that cannot be used together.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException
  = _MatchServiceError cloudWatch
      "InvalidParameterCombination"
      . hasStatus 400
