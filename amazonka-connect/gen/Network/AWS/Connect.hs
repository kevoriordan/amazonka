{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Connect is a cloud-based contact center solution that makes it easy to set up and manage a customer contact center and provide reliable customer engagement at any scale.
--
--
-- Amazon Connect provides rich metrics and real-time reporting that allow you to optimize contact routing. You can also resolve customer issues more efficiently by putting customers in touch with the right agents.
--
-- There are limits to the number of Amazon Connect resources that you can create and limits to the number of requests that you can make per second. For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Limits> in the /Amazon Connect Administrator Guide/ .
--
module Network.AWS.Connect
    (
    -- * Service Configuration
      connect

    -- * Errors
    -- $errors

    -- ** UserNotFoundException
    , _UserNotFoundException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ContactNotFoundException
    , _ContactNotFoundException

    -- ** DestinationNotAllowedException
    , _DestinationNotAllowedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** DuplicateResourceException
    , _DuplicateResourceException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** OutboundContactNotPermittedException
    , _OutboundContactNotPermittedException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListSecurityProfiles (Paginated)
    , module Network.AWS.Connect.ListSecurityProfiles

    -- ** UpdateUserHierarchy 
    , module Network.AWS.Connect.UpdateUserHierarchy

    -- ** UpdateUserRoutingProfile 
    , module Network.AWS.Connect.UpdateUserRoutingProfile

    -- ** StartOutboundVoiceContact 
    , module Network.AWS.Connect.StartOutboundVoiceContact

    -- ** ListTagsForResource 
    , module Network.AWS.Connect.ListTagsForResource

    -- ** GetMetricData (Paginated)
    , module Network.AWS.Connect.GetMetricData

    -- ** ListUsers (Paginated)
    , module Network.AWS.Connect.ListUsers

    -- ** ListUserHierarchyGroups (Paginated)
    , module Network.AWS.Connect.ListUserHierarchyGroups

    -- ** ListQueues (Paginated)
    , module Network.AWS.Connect.ListQueues

    -- ** GetCurrentMetricData 
    , module Network.AWS.Connect.GetCurrentMetricData

    -- ** ListRoutingProfiles (Paginated)
    , module Network.AWS.Connect.ListRoutingProfiles

    -- ** UpdateUserPhoneConfig 
    , module Network.AWS.Connect.UpdateUserPhoneConfig

    -- ** DescribeUserHierarchyStructure 
    , module Network.AWS.Connect.DescribeUserHierarchyStructure

    -- ** ListPhoneNumbers (Paginated)
    , module Network.AWS.Connect.ListPhoneNumbers

    -- ** UpdateContactAttributes 
    , module Network.AWS.Connect.UpdateContactAttributes

    -- ** StartChatContact 
    , module Network.AWS.Connect.StartChatContact

    -- ** UpdateUserSecurityProfiles 
    , module Network.AWS.Connect.UpdateUserSecurityProfiles

    -- ** GetContactAttributes 
    , module Network.AWS.Connect.GetContactAttributes

    -- ** DescribeUserHierarchyGroup 
    , module Network.AWS.Connect.DescribeUserHierarchyGroup

    -- ** DescribeUser 
    , module Network.AWS.Connect.DescribeUser

    -- ** CreateUser 
    , module Network.AWS.Connect.CreateUser

    -- ** GetFederationToken 
    , module Network.AWS.Connect.GetFederationToken

    -- ** StopContact 
    , module Network.AWS.Connect.StopContact

    -- ** DeleteUser 
    , module Network.AWS.Connect.DeleteUser

    -- ** TagResource 
    , module Network.AWS.Connect.TagResource

    -- ** UpdateUserIdentityInfo 
    , module Network.AWS.Connect.UpdateUserIdentityInfo

    -- ** ListContactFlows (Paginated)
    , module Network.AWS.Connect.ListContactFlows

    -- ** UntagResource 
    , module Network.AWS.Connect.UntagResource

    -- ** ListHoursOfOperations (Paginated)
    , module Network.AWS.Connect.ListHoursOfOperations

    -- * Types

    -- ** Channel
    , Channel (..)

    -- ** Comparison
    , Comparison (..)

    -- ** ContactFlowType
    , ContactFlowType (..)

    -- ** CurrentMetricName
    , CurrentMetricName (..)

    -- ** Grouping
    , Grouping (..)

    -- ** HistoricalMetricName
    , HistoricalMetricName (..)

    -- ** PhoneNumberCountryCode
    , PhoneNumberCountryCode (..)

    -- ** PhoneNumberType
    , PhoneNumberType (..)

    -- ** PhoneType
    , PhoneType (..)

    -- ** QueueType
    , QueueType (..)

    -- ** Statistic
    , Statistic (..)

    -- ** Unit
    , Unit (..)

    -- ** ChatMessage
    , ChatMessage
    , chatMessage
    , cmContentType
    , cmContent

    -- ** ContactFlowSummary
    , ContactFlowSummary
    , contactFlowSummary
    , cfsARN
    , cfsName
    , cfsContactFlowType
    , cfsId

    -- ** Credentials
    , Credentials
    , credentials
    , cAccessTokenExpiration
    , cAccessToken
    , cRefreshToken
    , cRefreshTokenExpiration

    -- ** CurrentMetric
    , CurrentMetric
    , currentMetric
    , cmName
    , cmUnit

    -- ** CurrentMetricData
    , CurrentMetricData
    , currentMetricData
    , cmdValue
    , cmdMetric

    -- ** CurrentMetricResult
    , CurrentMetricResult
    , currentMetricResult
    , cmrCollections
    , cmrDimensions

    -- ** Dimensions
    , Dimensions
    , dimensions
    , dChannel
    , dQueue

    -- ** Filters
    , Filters
    , filters
    , fQueues
    , fChannels

    -- ** HierarchyGroup
    , HierarchyGroup
    , hierarchyGroup
    , hgARN
    , hgName
    , hgHierarchyPath
    , hgId
    , hgLevelId

    -- ** HierarchyGroupSummary
    , HierarchyGroupSummary
    , hierarchyGroupSummary
    , hgsARN
    , hgsName
    , hgsId

    -- ** HierarchyLevel
    , HierarchyLevel
    , hierarchyLevel
    , hlARN
    , hlName
    , hlId

    -- ** HierarchyPath
    , HierarchyPath
    , hierarchyPath
    , hpLevelFive
    , hpLevelThree
    , hpLevelFour
    , hpLevelTwo
    , hpLevelOne

    -- ** HierarchyStructure
    , HierarchyStructure
    , hierarchyStructure
    , hsLevelFive
    , hsLevelThree
    , hsLevelFour
    , hsLevelTwo
    , hsLevelOne

    -- ** HistoricalMetric
    , HistoricalMetric
    , historicalMetric
    , hmName
    , hmThreshold
    , hmUnit
    , hmStatistic

    -- ** HistoricalMetricData
    , HistoricalMetricData
    , historicalMetricData
    , hmdValue
    , hmdMetric

    -- ** HistoricalMetricResult
    , HistoricalMetricResult
    , historicalMetricResult
    , hmrCollections
    , hmrDimensions

    -- ** HoursOfOperationSummary
    , HoursOfOperationSummary
    , hoursOfOperationSummary
    , hoosARN
    , hoosName
    , hoosId

    -- ** ParticipantDetails
    , ParticipantDetails
    , participantDetails
    , pdDisplayName

    -- ** PhoneNumberSummary
    , PhoneNumberSummary
    , phoneNumberSummary
    , pnsPhoneNumberType
    , pnsARN
    , pnsPhoneNumber
    , pnsPhoneNumberCountryCode
    , pnsId

    -- ** QueueReference
    , QueueReference
    , queueReference
    , qrARN
    , qrId

    -- ** QueueSummary
    , QueueSummary
    , queueSummary
    , qsARN
    , qsName
    , qsId
    , qsQueueType

    -- ** RoutingProfileSummary
    , RoutingProfileSummary
    , routingProfileSummary
    , rpsARN
    , rpsName
    , rpsId

    -- ** SecurityProfileSummary
    , SecurityProfileSummary
    , securityProfileSummary
    , spsARN
    , spsName
    , spsId

    -- ** Threshold
    , Threshold
    , threshold
    , tThresholdValue
    , tComparison

    -- ** User
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

    -- ** UserIdentityInfo
    , UserIdentityInfo
    , userIdentityInfo
    , uiiEmail
    , uiiLastName
    , uiiFirstName

    -- ** UserPhoneConfig
    , UserPhoneConfig
    , userPhoneConfig
    , upcAutoAccept
    , upcAfterContactWorkTimeLimit
    , upcDeskPhoneNumber
    , upcPhoneType

    -- ** UserSummary
    , UserSummary
    , userSummary
    , usARN
    , usUsername
    , usId
    ) where

import Network.AWS.Connect.CreateUser
import Network.AWS.Connect.DeleteUser
import Network.AWS.Connect.DescribeUser
import Network.AWS.Connect.DescribeUserHierarchyGroup
import Network.AWS.Connect.DescribeUserHierarchyStructure
import Network.AWS.Connect.GetContactAttributes
import Network.AWS.Connect.GetCurrentMetricData
import Network.AWS.Connect.GetFederationToken
import Network.AWS.Connect.GetMetricData
import Network.AWS.Connect.ListContactFlows
import Network.AWS.Connect.ListHoursOfOperations
import Network.AWS.Connect.ListPhoneNumbers
import Network.AWS.Connect.ListQueues
import Network.AWS.Connect.ListRoutingProfiles
import Network.AWS.Connect.ListSecurityProfiles
import Network.AWS.Connect.ListTagsForResource
import Network.AWS.Connect.ListUserHierarchyGroups
import Network.AWS.Connect.ListUsers
import Network.AWS.Connect.StartChatContact
import Network.AWS.Connect.StartOutboundVoiceContact
import Network.AWS.Connect.StopContact
import Network.AWS.Connect.TagResource
import Network.AWS.Connect.Types
import Network.AWS.Connect.UntagResource
import Network.AWS.Connect.UpdateContactAttributes
import Network.AWS.Connect.UpdateUserHierarchy
import Network.AWS.Connect.UpdateUserIdentityInfo
import Network.AWS.Connect.UpdateUserPhoneConfig
import Network.AWS.Connect.UpdateUserRoutingProfile
import Network.AWS.Connect.UpdateUserSecurityProfiles
import Network.AWS.Connect.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Connect'.
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
