{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.EMR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.EMR where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.EMR
import Test.AWS.EMR.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRunJobFlow $
--             runJobFlow
--
--         , requestRemoveAutoScalingPolicy $
--             removeAutoScalingPolicy
--
--         , requestSetVisibleToAllUsers $
--             setVisibleToAllUsers
--
--         , requestTerminateJobFlows $
--             terminateJobFlows
--
--         , requestDescribeStep $
--             describeStep
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestDescribeCluster $
--             describeCluster
--
--         , requestListSecurityConfigurations $
--             listSecurityConfigurations
--
--         , requestCancelSteps $
--             cancelSteps
--
--         , requestCreateSecurityConfiguration $
--             createSecurityConfiguration
--
--         , requestSetTerminationProtection $
--             setTerminationProtection
--
--         , requestAddJobFlowSteps $
--             addJobFlowSteps
--
--         , requestModifyInstanceGroups $
--             modifyInstanceGroups
--
--         , requestListSteps $
--             listSteps
--
--         , requestAddInstanceFleet $
--             addInstanceFleet
--
--         , requestPutManagedScalingPolicy $
--             putManagedScalingPolicy
--
--         , requestAddInstanceGroups $
--             addInstanceGroups
--
--         , requestDeleteSecurityConfiguration $
--             deleteSecurityConfiguration
--
--         , requestModifyInstanceFleet $
--             modifyInstanceFleet
--
--         , requestListInstanceGroups $
--             listInstanceGroups
--
--         , requestGetBlockPublicAccessConfiguration $
--             getBlockPublicAccessConfiguration
--
--         , requestModifyCluster $
--             modifyCluster
--
--         , requestPutBlockPublicAccessConfiguration $
--             putBlockPublicAccessConfiguration
--
--         , requestListBootstrapActions $
--             listBootstrapActions
--
--         , requestAddTags $
--             addTags
--
--         , requestListInstances $
--             listInstances
--
--         , requestPutAutoScalingPolicy $
--             putAutoScalingPolicy
--
--         , requestListClusters $
--             listClusters
--
--         , requestDescribeSecurityConfiguration $
--             describeSecurityConfiguration
--
--         , requestGetManagedScalingPolicy $
--             getManagedScalingPolicy
--
--         , requestListInstanceFleets $
--             listInstanceFleets
--
--         , requestRemoveManagedScalingPolicy $
--             removeManagedScalingPolicy
--
--           ]

--     , testGroup "response"
--         [ responseRunJobFlow $
--             runJobFlowResponse
--
--         , responseRemoveAutoScalingPolicy $
--             removeAutoScalingPolicyResponse
--
--         , responseSetVisibleToAllUsers $
--             setVisibleToAllUsersResponse
--
--         , responseTerminateJobFlows $
--             terminateJobFlowsResponse
--
--         , responseDescribeStep $
--             describeStepResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseDescribeCluster $
--             describeClusterResponse
--
--         , responseListSecurityConfigurations $
--             listSecurityConfigurationsResponse
--
--         , responseCancelSteps $
--             cancelStepsResponse
--
--         , responseCreateSecurityConfiguration $
--             createSecurityConfigurationResponse
--
--         , responseSetTerminationProtection $
--             setTerminationProtectionResponse
--
--         , responseAddJobFlowSteps $
--             addJobFlowStepsResponse
--
--         , responseModifyInstanceGroups $
--             modifyInstanceGroupsResponse
--
--         , responseListSteps $
--             listStepsResponse
--
--         , responseAddInstanceFleet $
--             addInstanceFleetResponse
--
--         , responsePutManagedScalingPolicy $
--             putManagedScalingPolicyResponse
--
--         , responseAddInstanceGroups $
--             addInstanceGroupsResponse
--
--         , responseDeleteSecurityConfiguration $
--             deleteSecurityConfigurationResponse
--
--         , responseModifyInstanceFleet $
--             modifyInstanceFleetResponse
--
--         , responseListInstanceGroups $
--             listInstanceGroupsResponse
--
--         , responseGetBlockPublicAccessConfiguration $
--             getBlockPublicAccessConfigurationResponse
--
--         , responseModifyCluster $
--             modifyClusterResponse
--
--         , responsePutBlockPublicAccessConfiguration $
--             putBlockPublicAccessConfigurationResponse
--
--         , responseListBootstrapActions $
--             listBootstrapActionsResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListInstances $
--             listInstancesResponse
--
--         , responsePutAutoScalingPolicy $
--             putAutoScalingPolicyResponse
--
--         , responseListClusters $
--             listClustersResponse
--
--         , responseDescribeSecurityConfiguration $
--             describeSecurityConfigurationResponse
--
--         , responseGetManagedScalingPolicy $
--             getManagedScalingPolicyResponse
--
--         , responseListInstanceFleets $
--             listInstanceFleetsResponse
--
--         , responseRemoveManagedScalingPolicy $
--             removeManagedScalingPolicyResponse
--
--           ]
--     ]

-- Requests

requestRunJobFlow :: RunJobFlow -> TestTree
requestRunJobFlow = req
    "RunJobFlow"
    "fixture/RunJobFlow.yaml"

requestRemoveAutoScalingPolicy :: RemoveAutoScalingPolicy -> TestTree
requestRemoveAutoScalingPolicy = req
    "RemoveAutoScalingPolicy"
    "fixture/RemoveAutoScalingPolicy.yaml"

requestSetVisibleToAllUsers :: SetVisibleToAllUsers -> TestTree
requestSetVisibleToAllUsers = req
    "SetVisibleToAllUsers"
    "fixture/SetVisibleToAllUsers.yaml"

requestTerminateJobFlows :: TerminateJobFlows -> TestTree
requestTerminateJobFlows = req
    "TerminateJobFlows"
    "fixture/TerminateJobFlows.yaml"

requestDescribeStep :: DescribeStep -> TestTree
requestDescribeStep = req
    "DescribeStep"
    "fixture/DescribeStep.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster = req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestListSecurityConfigurations :: ListSecurityConfigurations -> TestTree
requestListSecurityConfigurations = req
    "ListSecurityConfigurations"
    "fixture/ListSecurityConfigurations.yaml"

requestCancelSteps :: CancelSteps -> TestTree
requestCancelSteps = req
    "CancelSteps"
    "fixture/CancelSteps.yaml"

requestCreateSecurityConfiguration :: CreateSecurityConfiguration -> TestTree
requestCreateSecurityConfiguration = req
    "CreateSecurityConfiguration"
    "fixture/CreateSecurityConfiguration.yaml"

requestSetTerminationProtection :: SetTerminationProtection -> TestTree
requestSetTerminationProtection = req
    "SetTerminationProtection"
    "fixture/SetTerminationProtection.yaml"

requestAddJobFlowSteps :: AddJobFlowSteps -> TestTree
requestAddJobFlowSteps = req
    "AddJobFlowSteps"
    "fixture/AddJobFlowSteps.yaml"

requestModifyInstanceGroups :: ModifyInstanceGroups -> TestTree
requestModifyInstanceGroups = req
    "ModifyInstanceGroups"
    "fixture/ModifyInstanceGroups.yaml"

requestListSteps :: ListSteps -> TestTree
requestListSteps = req
    "ListSteps"
    "fixture/ListSteps.yaml"

requestAddInstanceFleet :: AddInstanceFleet -> TestTree
requestAddInstanceFleet = req
    "AddInstanceFleet"
    "fixture/AddInstanceFleet.yaml"

requestPutManagedScalingPolicy :: PutManagedScalingPolicy -> TestTree
requestPutManagedScalingPolicy = req
    "PutManagedScalingPolicy"
    "fixture/PutManagedScalingPolicy.yaml"

requestAddInstanceGroups :: AddInstanceGroups -> TestTree
requestAddInstanceGroups = req
    "AddInstanceGroups"
    "fixture/AddInstanceGroups.yaml"

requestDeleteSecurityConfiguration :: DeleteSecurityConfiguration -> TestTree
requestDeleteSecurityConfiguration = req
    "DeleteSecurityConfiguration"
    "fixture/DeleteSecurityConfiguration.yaml"

requestModifyInstanceFleet :: ModifyInstanceFleet -> TestTree
requestModifyInstanceFleet = req
    "ModifyInstanceFleet"
    "fixture/ModifyInstanceFleet.yaml"

requestListInstanceGroups :: ListInstanceGroups -> TestTree
requestListInstanceGroups = req
    "ListInstanceGroups"
    "fixture/ListInstanceGroups.yaml"

requestGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfiguration -> TestTree
requestGetBlockPublicAccessConfiguration = req
    "GetBlockPublicAccessConfiguration"
    "fixture/GetBlockPublicAccessConfiguration.yaml"

requestModifyCluster :: ModifyCluster -> TestTree
requestModifyCluster = req
    "ModifyCluster"
    "fixture/ModifyCluster.yaml"

requestPutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfiguration -> TestTree
requestPutBlockPublicAccessConfiguration = req
    "PutBlockPublicAccessConfiguration"
    "fixture/PutBlockPublicAccessConfiguration.yaml"

requestListBootstrapActions :: ListBootstrapActions -> TestTree
requestListBootstrapActions = req
    "ListBootstrapActions"
    "fixture/ListBootstrapActions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances = req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestPutAutoScalingPolicy :: PutAutoScalingPolicy -> TestTree
requestPutAutoScalingPolicy = req
    "PutAutoScalingPolicy"
    "fixture/PutAutoScalingPolicy.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters = req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestDescribeSecurityConfiguration :: DescribeSecurityConfiguration -> TestTree
requestDescribeSecurityConfiguration = req
    "DescribeSecurityConfiguration"
    "fixture/DescribeSecurityConfiguration.yaml"

requestGetManagedScalingPolicy :: GetManagedScalingPolicy -> TestTree
requestGetManagedScalingPolicy = req
    "GetManagedScalingPolicy"
    "fixture/GetManagedScalingPolicy.yaml"

requestListInstanceFleets :: ListInstanceFleets -> TestTree
requestListInstanceFleets = req
    "ListInstanceFleets"
    "fixture/ListInstanceFleets.yaml"

requestRemoveManagedScalingPolicy :: RemoveManagedScalingPolicy -> TestTree
requestRemoveManagedScalingPolicy = req
    "RemoveManagedScalingPolicy"
    "fixture/RemoveManagedScalingPolicy.yaml"

-- Responses

responseRunJobFlow :: RunJobFlowResponse -> TestTree
responseRunJobFlow = res
    "RunJobFlowResponse"
    "fixture/RunJobFlowResponse.proto"
    emr
    (Proxy :: Proxy RunJobFlow)

responseRemoveAutoScalingPolicy :: RemoveAutoScalingPolicyResponse -> TestTree
responseRemoveAutoScalingPolicy = res
    "RemoveAutoScalingPolicyResponse"
    "fixture/RemoveAutoScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy RemoveAutoScalingPolicy)

responseSetVisibleToAllUsers :: SetVisibleToAllUsersResponse -> TestTree
responseSetVisibleToAllUsers = res
    "SetVisibleToAllUsersResponse"
    "fixture/SetVisibleToAllUsersResponse.proto"
    emr
    (Proxy :: Proxy SetVisibleToAllUsers)

responseTerminateJobFlows :: TerminateJobFlowsResponse -> TestTree
responseTerminateJobFlows = res
    "TerminateJobFlowsResponse"
    "fixture/TerminateJobFlowsResponse.proto"
    emr
    (Proxy :: Proxy TerminateJobFlows)

responseDescribeStep :: DescribeStepResponse -> TestTree
responseDescribeStep = res
    "DescribeStepResponse"
    "fixture/DescribeStepResponse.proto"
    emr
    (Proxy :: Proxy DescribeStep)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    emr
    (Proxy :: Proxy RemoveTags)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster = res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    emr
    (Proxy :: Proxy DescribeCluster)

responseListSecurityConfigurations :: ListSecurityConfigurationsResponse -> TestTree
responseListSecurityConfigurations = res
    "ListSecurityConfigurationsResponse"
    "fixture/ListSecurityConfigurationsResponse.proto"
    emr
    (Proxy :: Proxy ListSecurityConfigurations)

responseCancelSteps :: CancelStepsResponse -> TestTree
responseCancelSteps = res
    "CancelStepsResponse"
    "fixture/CancelStepsResponse.proto"
    emr
    (Proxy :: Proxy CancelSteps)

responseCreateSecurityConfiguration :: CreateSecurityConfigurationResponse -> TestTree
responseCreateSecurityConfiguration = res
    "CreateSecurityConfigurationResponse"
    "fixture/CreateSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy CreateSecurityConfiguration)

responseSetTerminationProtection :: SetTerminationProtectionResponse -> TestTree
responseSetTerminationProtection = res
    "SetTerminationProtectionResponse"
    "fixture/SetTerminationProtectionResponse.proto"
    emr
    (Proxy :: Proxy SetTerminationProtection)

responseAddJobFlowSteps :: AddJobFlowStepsResponse -> TestTree
responseAddJobFlowSteps = res
    "AddJobFlowStepsResponse"
    "fixture/AddJobFlowStepsResponse.proto"
    emr
    (Proxy :: Proxy AddJobFlowSteps)

responseModifyInstanceGroups :: ModifyInstanceGroupsResponse -> TestTree
responseModifyInstanceGroups = res
    "ModifyInstanceGroupsResponse"
    "fixture/ModifyInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ModifyInstanceGroups)

responseListSteps :: ListStepsResponse -> TestTree
responseListSteps = res
    "ListStepsResponse"
    "fixture/ListStepsResponse.proto"
    emr
    (Proxy :: Proxy ListSteps)

responseAddInstanceFleet :: AddInstanceFleetResponse -> TestTree
responseAddInstanceFleet = res
    "AddInstanceFleetResponse"
    "fixture/AddInstanceFleetResponse.proto"
    emr
    (Proxy :: Proxy AddInstanceFleet)

responsePutManagedScalingPolicy :: PutManagedScalingPolicyResponse -> TestTree
responsePutManagedScalingPolicy = res
    "PutManagedScalingPolicyResponse"
    "fixture/PutManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy PutManagedScalingPolicy)

responseAddInstanceGroups :: AddInstanceGroupsResponse -> TestTree
responseAddInstanceGroups = res
    "AddInstanceGroupsResponse"
    "fixture/AddInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy AddInstanceGroups)

responseDeleteSecurityConfiguration :: DeleteSecurityConfigurationResponse -> TestTree
responseDeleteSecurityConfiguration = res
    "DeleteSecurityConfigurationResponse"
    "fixture/DeleteSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy DeleteSecurityConfiguration)

responseModifyInstanceFleet :: ModifyInstanceFleetResponse -> TestTree
responseModifyInstanceFleet = res
    "ModifyInstanceFleetResponse"
    "fixture/ModifyInstanceFleetResponse.proto"
    emr
    (Proxy :: Proxy ModifyInstanceFleet)

responseListInstanceGroups :: ListInstanceGroupsResponse -> TestTree
responseListInstanceGroups = res
    "ListInstanceGroupsResponse"
    "fixture/ListInstanceGroupsResponse.proto"
    emr
    (Proxy :: Proxy ListInstanceGroups)

responseGetBlockPublicAccessConfiguration :: GetBlockPublicAccessConfigurationResponse -> TestTree
responseGetBlockPublicAccessConfiguration = res
    "GetBlockPublicAccessConfigurationResponse"
    "fixture/GetBlockPublicAccessConfigurationResponse.proto"
    emr
    (Proxy :: Proxy GetBlockPublicAccessConfiguration)

responseModifyCluster :: ModifyClusterResponse -> TestTree
responseModifyCluster = res
    "ModifyClusterResponse"
    "fixture/ModifyClusterResponse.proto"
    emr
    (Proxy :: Proxy ModifyCluster)

responsePutBlockPublicAccessConfiguration :: PutBlockPublicAccessConfigurationResponse -> TestTree
responsePutBlockPublicAccessConfiguration = res
    "PutBlockPublicAccessConfigurationResponse"
    "fixture/PutBlockPublicAccessConfigurationResponse.proto"
    emr
    (Proxy :: Proxy PutBlockPublicAccessConfiguration)

responseListBootstrapActions :: ListBootstrapActionsResponse -> TestTree
responseListBootstrapActions = res
    "ListBootstrapActionsResponse"
    "fixture/ListBootstrapActionsResponse.proto"
    emr
    (Proxy :: Proxy ListBootstrapActions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    emr
    (Proxy :: Proxy AddTags)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances = res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    emr
    (Proxy :: Proxy ListInstances)

responsePutAutoScalingPolicy :: PutAutoScalingPolicyResponse -> TestTree
responsePutAutoScalingPolicy = res
    "PutAutoScalingPolicyResponse"
    "fixture/PutAutoScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy PutAutoScalingPolicy)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters = res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    emr
    (Proxy :: Proxy ListClusters)

responseDescribeSecurityConfiguration :: DescribeSecurityConfigurationResponse -> TestTree
responseDescribeSecurityConfiguration = res
    "DescribeSecurityConfigurationResponse"
    "fixture/DescribeSecurityConfigurationResponse.proto"
    emr
    (Proxy :: Proxy DescribeSecurityConfiguration)

responseGetManagedScalingPolicy :: GetManagedScalingPolicyResponse -> TestTree
responseGetManagedScalingPolicy = res
    "GetManagedScalingPolicyResponse"
    "fixture/GetManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy GetManagedScalingPolicy)

responseListInstanceFleets :: ListInstanceFleetsResponse -> TestTree
responseListInstanceFleets = res
    "ListInstanceFleetsResponse"
    "fixture/ListInstanceFleetsResponse.proto"
    emr
    (Proxy :: Proxy ListInstanceFleets)

responseRemoveManagedScalingPolicy :: RemoveManagedScalingPolicyResponse -> TestTree
responseRemoveManagedScalingPolicy = res
    "RemoveManagedScalingPolicyResponse"
    "fixture/RemoveManagedScalingPolicyResponse.proto"
    emr
    (Proxy :: Proxy RemoveManagedScalingPolicy)
