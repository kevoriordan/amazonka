{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.FMS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.FMS
import Test.AWS.FMS.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPolicies $
--             listPolicies
--
--         , requestGetComplianceDetail $
--             getComplianceDetail
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetNotificationChannel $
--             getNotificationChannel
--
--         , requestGetAdminAccount $
--             getAdminAccount
--
--         , requestListComplianceStatus $
--             listComplianceStatus
--
--         , requestPutPolicy $
--             putPolicy
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestDisassociateAdminAccount $
--             disassociateAdminAccount
--
--         , requestPutNotificationChannel $
--             putNotificationChannel
--
--         , requestDeleteNotificationChannel $
--             deleteNotificationChannel
--
--         , requestAssociateAdminAccount $
--             associateAdminAccount
--
--         , requestListMemberAccounts $
--             listMemberAccounts
--
--         , requestTagResource $
--             tagResource
--
--         , requestUntagResource $
--             untagResource
--
--         , requestGetPolicy $
--             getPolicy
--
--         , requestGetProtectionStatus $
--             getProtectionStatus
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             listPoliciesResponse
--
--         , responseGetComplianceDetail $
--             getComplianceDetailResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetNotificationChannel $
--             getNotificationChannelResponse
--
--         , responseGetAdminAccount $
--             getAdminAccountResponse
--
--         , responseListComplianceStatus $
--             listComplianceStatusResponse
--
--         , responsePutPolicy $
--             putPolicyResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseDisassociateAdminAccount $
--             disassociateAdminAccountResponse
--
--         , responsePutNotificationChannel $
--             putNotificationChannelResponse
--
--         , responseDeleteNotificationChannel $
--             deleteNotificationChannelResponse
--
--         , responseAssociateAdminAccount $
--             associateAdminAccountResponse
--
--         , responseListMemberAccounts $
--             listMemberAccountsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--         , responseGetProtectionStatus $
--             getProtectionStatusResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestGetComplianceDetail :: GetComplianceDetail -> TestTree
requestGetComplianceDetail = req
    "GetComplianceDetail"
    "fixture/GetComplianceDetail.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetNotificationChannel :: GetNotificationChannel -> TestTree
requestGetNotificationChannel = req
    "GetNotificationChannel"
    "fixture/GetNotificationChannel.yaml"

requestGetAdminAccount :: GetAdminAccount -> TestTree
requestGetAdminAccount = req
    "GetAdminAccount"
    "fixture/GetAdminAccount.yaml"

requestListComplianceStatus :: ListComplianceStatus -> TestTree
requestListComplianceStatus = req
    "ListComplianceStatus"
    "fixture/ListComplianceStatus.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy = req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDisassociateAdminAccount :: DisassociateAdminAccount -> TestTree
requestDisassociateAdminAccount = req
    "DisassociateAdminAccount"
    "fixture/DisassociateAdminAccount.yaml"

requestPutNotificationChannel :: PutNotificationChannel -> TestTree
requestPutNotificationChannel = req
    "PutNotificationChannel"
    "fixture/PutNotificationChannel.yaml"

requestDeleteNotificationChannel :: DeleteNotificationChannel -> TestTree
requestDeleteNotificationChannel = req
    "DeleteNotificationChannel"
    "fixture/DeleteNotificationChannel.yaml"

requestAssociateAdminAccount :: AssociateAdminAccount -> TestTree
requestAssociateAdminAccount = req
    "AssociateAdminAccount"
    "fixture/AssociateAdminAccount.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts = req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

requestGetProtectionStatus :: GetProtectionStatus -> TestTree
requestGetProtectionStatus = req
    "GetProtectionStatus"
    "fixture/GetProtectionStatus.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    fms
    (Proxy :: Proxy ListPolicies)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail = res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    fms
    (Proxy :: Proxy GetComplianceDetail)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    fms
    (Proxy :: Proxy ListTagsForResource)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel = res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy GetNotificationChannel)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount = res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy GetAdminAccount)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus = res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    fms
    (Proxy :: Proxy ListComplianceStatus)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy = res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    fms
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    fms
    (Proxy :: Proxy DeletePolicy)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount = res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy DisassociateAdminAccount)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel = res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy PutNotificationChannel)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel = res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy DeleteNotificationChannel)

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount = res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy AssociateAdminAccount)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts = res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    fms
    (Proxy :: Proxy ListMemberAccounts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    fms
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    fms
    (Proxy :: Proxy UntagResource)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    fms
    (Proxy :: Proxy GetPolicy)

responseGetProtectionStatus :: GetProtectionStatusResponse -> TestTree
responseGetProtectionStatus = res
    "GetProtectionStatusResponse"
    "fixture/GetProtectionStatusResponse.proto"
    fms
    (Proxy :: Proxy GetProtectionStatus)
