{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Connect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Connect where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Connect
import Test.AWS.Connect.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListSecurityProfiles $
--             listSecurityProfiles
--
--         , requestUpdateUserHierarchy $
--             updateUserHierarchy
--
--         , requestUpdateUserRoutingProfile $
--             updateUserRoutingProfile
--
--         , requestStartOutboundVoiceContact $
--             startOutboundVoiceContact
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetMetricData $
--             getMetricData
--
--         , requestListUsers $
--             listUsers
--
--         , requestListUserHierarchyGroups $
--             listUserHierarchyGroups
--
--         , requestListQueues $
--             listQueues
--
--         , requestGetCurrentMetricData $
--             getCurrentMetricData
--
--         , requestListRoutingProfiles $
--             listRoutingProfiles
--
--         , requestUpdateUserPhoneConfig $
--             updateUserPhoneConfig
--
--         , requestDescribeUserHierarchyStructure $
--             describeUserHierarchyStructure
--
--         , requestListPhoneNumbers $
--             listPhoneNumbers
--
--         , requestUpdateContactAttributes $
--             updateContactAttributes
--
--         , requestStartChatContact $
--             startChatContact
--
--         , requestUpdateUserSecurityProfiles $
--             updateUserSecurityProfiles
--
--         , requestGetContactAttributes $
--             getContactAttributes
--
--         , requestDescribeUserHierarchyGroup $
--             describeUserHierarchyGroup
--
--         , requestDescribeUser $
--             describeUser
--
--         , requestCreateUser $
--             createUser
--
--         , requestGetFederationToken $
--             getFederationToken
--
--         , requestStopContact $
--             stopContact
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestTagResource $
--             tagResource
--
--         , requestUpdateUserIdentityInfo $
--             updateUserIdentityInfo
--
--         , requestListContactFlows $
--             listContactFlows
--
--         , requestUntagResource $
--             untagResource
--
--         , requestListHoursOfOperations $
--             listHoursOfOperations
--
--           ]

--     , testGroup "response"
--         [ responseListSecurityProfiles $
--             listSecurityProfilesResponse
--
--         , responseUpdateUserHierarchy $
--             updateUserHierarchyResponse
--
--         , responseUpdateUserRoutingProfile $
--             updateUserRoutingProfileResponse
--
--         , responseStartOutboundVoiceContact $
--             startOutboundVoiceContactResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetMetricData $
--             getMetricDataResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseListUserHierarchyGroups $
--             listUserHierarchyGroupsResponse
--
--         , responseListQueues $
--             listQueuesResponse
--
--         , responseGetCurrentMetricData $
--             getCurrentMetricDataResponse
--
--         , responseListRoutingProfiles $
--             listRoutingProfilesResponse
--
--         , responseUpdateUserPhoneConfig $
--             updateUserPhoneConfigResponse
--
--         , responseDescribeUserHierarchyStructure $
--             describeUserHierarchyStructureResponse
--
--         , responseListPhoneNumbers $
--             listPhoneNumbersResponse
--
--         , responseUpdateContactAttributes $
--             updateContactAttributesResponse
--
--         , responseStartChatContact $
--             startChatContactResponse
--
--         , responseUpdateUserSecurityProfiles $
--             updateUserSecurityProfilesResponse
--
--         , responseGetContactAttributes $
--             getContactAttributesResponse
--
--         , responseDescribeUserHierarchyGroup $
--             describeUserHierarchyGroupResponse
--
--         , responseDescribeUser $
--             describeUserResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseGetFederationToken $
--             getFederationTokenResponse
--
--         , responseStopContact $
--             stopContactResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUpdateUserIdentityInfo $
--             updateUserIdentityInfoResponse
--
--         , responseListContactFlows $
--             listContactFlowsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseListHoursOfOperations $
--             listHoursOfOperationsResponse
--
--           ]
--     ]

-- Requests

requestListSecurityProfiles :: ListSecurityProfiles -> TestTree
requestListSecurityProfiles = req
    "ListSecurityProfiles"
    "fixture/ListSecurityProfiles.yaml"

requestUpdateUserHierarchy :: UpdateUserHierarchy -> TestTree
requestUpdateUserHierarchy = req
    "UpdateUserHierarchy"
    "fixture/UpdateUserHierarchy.yaml"

requestUpdateUserRoutingProfile :: UpdateUserRoutingProfile -> TestTree
requestUpdateUserRoutingProfile = req
    "UpdateUserRoutingProfile"
    "fixture/UpdateUserRoutingProfile.yaml"

requestStartOutboundVoiceContact :: StartOutboundVoiceContact -> TestTree
requestStartOutboundVoiceContact = req
    "StartOutboundVoiceContact"
    "fixture/StartOutboundVoiceContact.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetMetricData :: GetMetricData -> TestTree
requestGetMetricData = req
    "GetMetricData"
    "fixture/GetMetricData.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestListUserHierarchyGroups :: ListUserHierarchyGroups -> TestTree
requestListUserHierarchyGroups = req
    "ListUserHierarchyGroups"
    "fixture/ListUserHierarchyGroups.yaml"

requestListQueues :: ListQueues -> TestTree
requestListQueues = req
    "ListQueues"
    "fixture/ListQueues.yaml"

requestGetCurrentMetricData :: GetCurrentMetricData -> TestTree
requestGetCurrentMetricData = req
    "GetCurrentMetricData"
    "fixture/GetCurrentMetricData.yaml"

requestListRoutingProfiles :: ListRoutingProfiles -> TestTree
requestListRoutingProfiles = req
    "ListRoutingProfiles"
    "fixture/ListRoutingProfiles.yaml"

requestUpdateUserPhoneConfig :: UpdateUserPhoneConfig -> TestTree
requestUpdateUserPhoneConfig = req
    "UpdateUserPhoneConfig"
    "fixture/UpdateUserPhoneConfig.yaml"

requestDescribeUserHierarchyStructure :: DescribeUserHierarchyStructure -> TestTree
requestDescribeUserHierarchyStructure = req
    "DescribeUserHierarchyStructure"
    "fixture/DescribeUserHierarchyStructure.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers = req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestUpdateContactAttributes :: UpdateContactAttributes -> TestTree
requestUpdateContactAttributes = req
    "UpdateContactAttributes"
    "fixture/UpdateContactAttributes.yaml"

requestStartChatContact :: StartChatContact -> TestTree
requestStartChatContact = req
    "StartChatContact"
    "fixture/StartChatContact.yaml"

requestUpdateUserSecurityProfiles :: UpdateUserSecurityProfiles -> TestTree
requestUpdateUserSecurityProfiles = req
    "UpdateUserSecurityProfiles"
    "fixture/UpdateUserSecurityProfiles.yaml"

requestGetContactAttributes :: GetContactAttributes -> TestTree
requestGetContactAttributes = req
    "GetContactAttributes"
    "fixture/GetContactAttributes.yaml"

requestDescribeUserHierarchyGroup :: DescribeUserHierarchyGroup -> TestTree
requestDescribeUserHierarchyGroup = req
    "DescribeUserHierarchyGroup"
    "fixture/DescribeUserHierarchyGroup.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser = req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken = req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestStopContact :: StopContact -> TestTree
requestStopContact = req
    "StopContact"
    "fixture/StopContact.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateUserIdentityInfo :: UpdateUserIdentityInfo -> TestTree
requestUpdateUserIdentityInfo = req
    "UpdateUserIdentityInfo"
    "fixture/UpdateUserIdentityInfo.yaml"

requestListContactFlows :: ListContactFlows -> TestTree
requestListContactFlows = req
    "ListContactFlows"
    "fixture/ListContactFlows.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListHoursOfOperations :: ListHoursOfOperations -> TestTree
requestListHoursOfOperations = req
    "ListHoursOfOperations"
    "fixture/ListHoursOfOperations.yaml"

-- Responses

responseListSecurityProfiles :: ListSecurityProfilesResponse -> TestTree
responseListSecurityProfiles = res
    "ListSecurityProfilesResponse"
    "fixture/ListSecurityProfilesResponse.proto"
    connect
    (Proxy :: Proxy ListSecurityProfiles)

responseUpdateUserHierarchy :: UpdateUserHierarchyResponse -> TestTree
responseUpdateUserHierarchy = res
    "UpdateUserHierarchyResponse"
    "fixture/UpdateUserHierarchyResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserHierarchy)

responseUpdateUserRoutingProfile :: UpdateUserRoutingProfileResponse -> TestTree
responseUpdateUserRoutingProfile = res
    "UpdateUserRoutingProfileResponse"
    "fixture/UpdateUserRoutingProfileResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserRoutingProfile)

responseStartOutboundVoiceContact :: StartOutboundVoiceContactResponse -> TestTree
responseStartOutboundVoiceContact = res
    "StartOutboundVoiceContactResponse"
    "fixture/StartOutboundVoiceContactResponse.proto"
    connect
    (Proxy :: Proxy StartOutboundVoiceContact)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    connect
    (Proxy :: Proxy ListTagsForResource)

responseGetMetricData :: GetMetricDataResponse -> TestTree
responseGetMetricData = res
    "GetMetricDataResponse"
    "fixture/GetMetricDataResponse.proto"
    connect
    (Proxy :: Proxy GetMetricData)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    connect
    (Proxy :: Proxy ListUsers)

responseListUserHierarchyGroups :: ListUserHierarchyGroupsResponse -> TestTree
responseListUserHierarchyGroups = res
    "ListUserHierarchyGroupsResponse"
    "fixture/ListUserHierarchyGroupsResponse.proto"
    connect
    (Proxy :: Proxy ListUserHierarchyGroups)

responseListQueues :: ListQueuesResponse -> TestTree
responseListQueues = res
    "ListQueuesResponse"
    "fixture/ListQueuesResponse.proto"
    connect
    (Proxy :: Proxy ListQueues)

responseGetCurrentMetricData :: GetCurrentMetricDataResponse -> TestTree
responseGetCurrentMetricData = res
    "GetCurrentMetricDataResponse"
    "fixture/GetCurrentMetricDataResponse.proto"
    connect
    (Proxy :: Proxy GetCurrentMetricData)

responseListRoutingProfiles :: ListRoutingProfilesResponse -> TestTree
responseListRoutingProfiles = res
    "ListRoutingProfilesResponse"
    "fixture/ListRoutingProfilesResponse.proto"
    connect
    (Proxy :: Proxy ListRoutingProfiles)

responseUpdateUserPhoneConfig :: UpdateUserPhoneConfigResponse -> TestTree
responseUpdateUserPhoneConfig = res
    "UpdateUserPhoneConfigResponse"
    "fixture/UpdateUserPhoneConfigResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserPhoneConfig)

responseDescribeUserHierarchyStructure :: DescribeUserHierarchyStructureResponse -> TestTree
responseDescribeUserHierarchyStructure = res
    "DescribeUserHierarchyStructureResponse"
    "fixture/DescribeUserHierarchyStructureResponse.proto"
    connect
    (Proxy :: Proxy DescribeUserHierarchyStructure)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers = res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    connect
    (Proxy :: Proxy ListPhoneNumbers)

responseUpdateContactAttributes :: UpdateContactAttributesResponse -> TestTree
responseUpdateContactAttributes = res
    "UpdateContactAttributesResponse"
    "fixture/UpdateContactAttributesResponse.proto"
    connect
    (Proxy :: Proxy UpdateContactAttributes)

responseStartChatContact :: StartChatContactResponse -> TestTree
responseStartChatContact = res
    "StartChatContactResponse"
    "fixture/StartChatContactResponse.proto"
    connect
    (Proxy :: Proxy StartChatContact)

responseUpdateUserSecurityProfiles :: UpdateUserSecurityProfilesResponse -> TestTree
responseUpdateUserSecurityProfiles = res
    "UpdateUserSecurityProfilesResponse"
    "fixture/UpdateUserSecurityProfilesResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserSecurityProfiles)

responseGetContactAttributes :: GetContactAttributesResponse -> TestTree
responseGetContactAttributes = res
    "GetContactAttributesResponse"
    "fixture/GetContactAttributesResponse.proto"
    connect
    (Proxy :: Proxy GetContactAttributes)

responseDescribeUserHierarchyGroup :: DescribeUserHierarchyGroupResponse -> TestTree
responseDescribeUserHierarchyGroup = res
    "DescribeUserHierarchyGroupResponse"
    "fixture/DescribeUserHierarchyGroupResponse.proto"
    connect
    (Proxy :: Proxy DescribeUserHierarchyGroup)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser = res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    connect
    (Proxy :: Proxy DescribeUser)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    connect
    (Proxy :: Proxy CreateUser)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken = res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    connect
    (Proxy :: Proxy GetFederationToken)

responseStopContact :: StopContactResponse -> TestTree
responseStopContact = res
    "StopContactResponse"
    "fixture/StopContactResponse.proto"
    connect
    (Proxy :: Proxy StopContact)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    connect
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    connect
    (Proxy :: Proxy TagResource)

responseUpdateUserIdentityInfo :: UpdateUserIdentityInfoResponse -> TestTree
responseUpdateUserIdentityInfo = res
    "UpdateUserIdentityInfoResponse"
    "fixture/UpdateUserIdentityInfoResponse.proto"
    connect
    (Proxy :: Proxy UpdateUserIdentityInfo)

responseListContactFlows :: ListContactFlowsResponse -> TestTree
responseListContactFlows = res
    "ListContactFlowsResponse"
    "fixture/ListContactFlowsResponse.proto"
    connect
    (Proxy :: Proxy ListContactFlows)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    connect
    (Proxy :: Proxy UntagResource)

responseListHoursOfOperations :: ListHoursOfOperationsResponse -> TestTree
responseListHoursOfOperations = res
    "ListHoursOfOperationsResponse"
    "fixture/ListHoursOfOperationsResponse.proto"
    connect
    (Proxy :: Proxy ListHoursOfOperations)
