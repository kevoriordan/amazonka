{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types
    (
    -- * Service Configuration
      connect

    -- * Errors
    , _UserNotFoundException
    , _InternalServiceException
    , _InvalidParameterException
    , _InvalidRequestException
    , _ContactNotFoundException
    , _DestinationNotAllowedException
    , _ResourceNotFoundException
    , _DuplicateResourceException
    , _ThrottlingException
    , _OutboundContactNotPermittedException
    , _LimitExceededException

    -- * Channel
    , Channel (..)

    -- * Comparison
    , Comparison (..)

    -- * ContactFlowType
    , ContactFlowType (..)

    -- * CurrentMetricName
    , CurrentMetricName (..)

    -- * Grouping
    , Grouping (..)

    -- * HistoricalMetricName
    , HistoricalMetricName (..)

    -- * PhoneNumberCountryCode
    , PhoneNumberCountryCode (..)

    -- * PhoneNumberType
    , PhoneNumberType (..)

    -- * PhoneType
    , PhoneType (..)

    -- * QueueType
    , QueueType (..)

    -- * Statistic
    , Statistic (..)

    -- * Unit
    , Unit (..)

    -- * ChatMessage
    , ChatMessage
    , chatMessage
    , cmContentType
    , cmContent

    -- * ContactFlowSummary
    , ContactFlowSummary
    , contactFlowSummary
    , cfsARN
    , cfsName
    , cfsContactFlowType
    , cfsId

    -- * Credentials
    , Credentials
    , credentials
    , cAccessTokenExpiration
    , cAccessToken
    , cRefreshToken
    , cRefreshTokenExpiration

    -- * CurrentMetric
    , CurrentMetric
    , currentMetric
    , cmName
    , cmUnit

    -- * CurrentMetricData
    , CurrentMetricData
    , currentMetricData
    , cmdValue
    , cmdMetric

    -- * CurrentMetricResult
    , CurrentMetricResult
    , currentMetricResult
    , cmrCollections
    , cmrDimensions

    -- * Dimensions
    , Dimensions
    , dimensions
    , dChannel
    , dQueue

    -- * Filters
    , Filters
    , filters
    , fQueues
    , fChannels

    -- * HierarchyGroup
    , HierarchyGroup
    , hierarchyGroup
    , hgARN
    , hgName
    , hgHierarchyPath
    , hgId
    , hgLevelId

    -- * HierarchyGroupSummary
    , HierarchyGroupSummary
    , hierarchyGroupSummary
    , hgsARN
    , hgsName
    , hgsId

    -- * HierarchyLevel
    , HierarchyLevel
    , hierarchyLevel
    , hlARN
    , hlName
    , hlId

    -- * HierarchyPath
    , HierarchyPath
    , hierarchyPath
    , hpLevelFive
    , hpLevelThree
    , hpLevelFour
    , hpLevelTwo
    , hpLevelOne

    -- * HierarchyStructure
    , HierarchyStructure
    , hierarchyStructure
    , hsLevelFive
    , hsLevelThree
    , hsLevelFour
    , hsLevelTwo
    , hsLevelOne

    -- * HistoricalMetric
    , HistoricalMetric
    , historicalMetric
    , hmName
    , hmThreshold
    , hmUnit
    , hmStatistic

    -- * HistoricalMetricData
    , HistoricalMetricData
    , historicalMetricData
    , hmdValue
    , hmdMetric

    -- * HistoricalMetricResult
    , HistoricalMetricResult
    , historicalMetricResult
    , hmrCollections
    , hmrDimensions

    -- * HoursOfOperationSummary
    , HoursOfOperationSummary
    , hoursOfOperationSummary
    , hoosARN
    , hoosName
    , hoosId

    -- * ParticipantDetails
    , ParticipantDetails
    , participantDetails
    , pdDisplayName

    -- * PhoneNumberSummary
    , PhoneNumberSummary
    , phoneNumberSummary
    , pnsPhoneNumberType
    , pnsARN
    , pnsPhoneNumber
    , pnsPhoneNumberCountryCode
    , pnsId

    -- * QueueReference
    , QueueReference
    , queueReference
    , qrARN
    , qrId

    -- * QueueSummary
    , QueueSummary
    , queueSummary
    , qsARN
    , qsName
    , qsId
    , qsQueueType

    -- * RoutingProfileSummary
    , RoutingProfileSummary
    , routingProfileSummary
    , rpsARN
    , rpsName
    , rpsId

    -- * SecurityProfileSummary
    , SecurityProfileSummary
    , securityProfileSummary
    , spsARN
    , spsName
    , spsId

    -- * Threshold
    , Threshold
    , threshold
    , tThresholdValue
    , tComparison

    -- * User
    , User
    , user
    , uRoutingProfileId
    , uDirectoryUserId
    , uARN
    , uIdentityInfo
    , uSecurityProfileIds
    , uUsername
    , uId
    , uHierarchyGroupId
    , uPhoneConfig
    , uTags

    -- * UserIdentityInfo
    , UserIdentityInfo
    , userIdentityInfo
    , uiiEmail
    , uiiLastName
    , uiiFirstName

    -- * UserPhoneConfig
    , UserPhoneConfig
    , userPhoneConfig
    , upcAutoAccept
    , upcAfterContactWorkTimeLimit
    , upcDeskPhoneNumber
    , upcPhoneType

    -- * UserSummary
    , UserSummary
    , userSummary
    , usARN
    , usUsername
    , usId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Connect.Types.Channel
import Network.AWS.Connect.Types.Comparison
import Network.AWS.Connect.Types.ContactFlowType
import Network.AWS.Connect.Types.CurrentMetricName
import Network.AWS.Connect.Types.Grouping
import Network.AWS.Connect.Types.HistoricalMetricName
import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberType
import Network.AWS.Connect.Types.PhoneType
import Network.AWS.Connect.Types.QueueType
import Network.AWS.Connect.Types.Statistic
import Network.AWS.Connect.Types.Unit
import Network.AWS.Connect.Types.ChatMessage
import Network.AWS.Connect.Types.ContactFlowSummary
import Network.AWS.Connect.Types.Credentials
import Network.AWS.Connect.Types.CurrentMetric
import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.CurrentMetricResult
import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.Filters
import Network.AWS.Connect.Types.HierarchyGroup
import Network.AWS.Connect.Types.HierarchyGroupSummary
import Network.AWS.Connect.Types.HierarchyLevel
import Network.AWS.Connect.Types.HierarchyPath
import Network.AWS.Connect.Types.HierarchyStructure
import Network.AWS.Connect.Types.HistoricalMetric
import Network.AWS.Connect.Types.HistoricalMetricData
import Network.AWS.Connect.Types.HistoricalMetricResult
import Network.AWS.Connect.Types.HoursOfOperationSummary
import Network.AWS.Connect.Types.ParticipantDetails
import Network.AWS.Connect.Types.PhoneNumberSummary
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Connect.Types.QueueSummary
import Network.AWS.Connect.Types.RoutingProfileSummary
import Network.AWS.Connect.Types.SecurityProfileSummary
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.User
import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Connect.Types.UserSummary

-- | API version @2017-08-08@ of the Amazon Connect Service SDK configuration.
connect :: Service
connect
  = Service{_svcAbbrev = "Connect", _svcSigner = v4,
            _svcPrefix = "connect", _svcVersion = "2017-08-08",
            _svcEndpoint = defaultEndpoint connect,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Connect",
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

-- | No user with the specified credentials was found in the Amazon Connect instance.
--
--
_UserNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_UserNotFoundException
  = _MatchServiceError connect "UserNotFoundException"
      . hasStatus 404

-- | Request processing failed due to an error or failure with the service.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException
  = _MatchServiceError connect
      "InternalServiceException"
      . hasStatus 500

-- | One or more of the specified parameters are not valid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError connect
      "InvalidParameterException"
      . hasStatus 400

-- | The request is not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException
  = _MatchServiceError connect
      "InvalidRequestException"
      . hasStatus 400

-- | The contact with the specified ID is not active or does not exist.
--
--
_ContactNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ContactNotFoundException
  = _MatchServiceError connect
      "ContactNotFoundException"
      . hasStatus 410

-- | Outbound calls to the destination number are not allowed.
--
--
_DestinationNotAllowedException :: AsError a => Getting (First ServiceError) a ServiceError
_DestinationNotAllowedException
  = _MatchServiceError connect
      "DestinationNotAllowedException"
      . hasStatus 403

-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError connect
      "ResourceNotFoundException"
      . hasStatus 404

-- | A resource with the specified name already exists.
--
--
_DuplicateResourceException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateResourceException
  = _MatchServiceError connect
      "DuplicateResourceException"
      . hasStatus 409

-- | The throttling limit has been exceeded.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException
  = _MatchServiceError connect "ThrottlingException" .
      hasStatus 429

-- | The contact is not permitted.
--
--
_OutboundContactNotPermittedException :: AsError a => Getting (First ServiceError) a ServiceError
_OutboundContactNotPermittedException
  = _MatchServiceError connect
      "OutboundContactNotPermittedException"
      . hasStatus 403

-- | The allowed limit for the resource has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError connect "LimitExceededException"
      . hasStatus 429
