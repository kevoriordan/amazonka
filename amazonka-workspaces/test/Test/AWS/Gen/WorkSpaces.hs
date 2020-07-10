{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WorkSpaces
import Test.AWS.WorkSpaces.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeAccount $
--             describeAccount
--
--         , requestRevokeIPRules $
--             revokeIPRules
--
--         , requestDescribeWorkspaceImages $
--             describeWorkspaceImages
--
--         , requestModifyWorkspaceProperties $
--             modifyWorkspaceProperties
--
--         , requestDeregisterWorkspaceDirectory $
--             deregisterWorkspaceDirectory
--
--         , requestMigrateWorkspace $
--             migrateWorkspace
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDescribeWorkspaceDirectories $
--             describeWorkspaceDirectories
--
--         , requestDisassociateIPGroups $
--             disassociateIPGroups
--
--         , requestDescribeWorkspaceBundles $
--             describeWorkspaceBundles
--
--         , requestAuthorizeIPRules $
--             authorizeIPRules
--
--         , requestRebuildWorkspaces $
--             rebuildWorkspaces
--
--         , requestImportWorkspaceImage $
--             importWorkspaceImage
--
--         , requestModifyWorkspaceState $
--             modifyWorkspaceState
--
--         , requestCreateIPGroup $
--             createIPGroup
--
--         , requestModifyWorkspaceCreationProperties $
--             modifyWorkspaceCreationProperties
--
--         , requestRegisterWorkspaceDirectory $
--             registerWorkspaceDirectory
--
--         , requestRestoreWorkspace $
--             restoreWorkspace
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestModifyWorkspaceAccessProperties $
--             modifyWorkspaceAccessProperties
--
--         , requestUpdateRulesOfIPGroup $
--             updateRulesOfIPGroup
--
--         , requestDeleteWorkspaceImage $
--             deleteWorkspaceImage
--
--         , requestStopWorkspaces $
--             stopWorkspaces
--
--         , requestAssociateIPGroups $
--             associateIPGroups
--
--         , requestModifySelfservicePermissions $
--             modifySelfservicePermissions
--
--         , requestDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatus
--
--         , requestRebootWorkspaces $
--             rebootWorkspaces
--
--         , requestDeleteIPGroup $
--             deleteIPGroup
--
--         , requestCopyWorkspaceImage $
--             copyWorkspaceImage
--
--         , requestDescribeWorkspaceSnapshots $
--             describeWorkspaceSnapshots
--
--         , requestTerminateWorkspaces $
--             terminateWorkspaces
--
--         , requestCreateWorkspaces $
--             createWorkspaces
--
--         , requestDescribeClientProperties $
--             describeClientProperties
--
--         , requestModifyClientProperties $
--             modifyClientProperties
--
--         , requestDescribeIPGroups $
--             describeIPGroups
--
--         , requestListAvailableManagementCidrRanges $
--             listAvailableManagementCidrRanges
--
--         , requestDescribeWorkspaces $
--             describeWorkspaces
--
--         , requestStartWorkspaces $
--             startWorkspaces
--
--         , requestDescribeAccountModifications $
--             describeAccountModifications
--
--         , requestModifyAccount $
--             modifyAccount
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAccount $
--             describeAccountResponse
--
--         , responseRevokeIPRules $
--             revokeIPRulesResponse
--
--         , responseDescribeWorkspaceImages $
--             describeWorkspaceImagesResponse
--
--         , responseModifyWorkspaceProperties $
--             modifyWorkspacePropertiesResponse
--
--         , responseDeregisterWorkspaceDirectory $
--             deregisterWorkspaceDirectoryResponse
--
--         , responseMigrateWorkspace $
--             migrateWorkspaceResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDescribeWorkspaceDirectories $
--             describeWorkspaceDirectoriesResponse
--
--         , responseDisassociateIPGroups $
--             disassociateIPGroupsResponse
--
--         , responseDescribeWorkspaceBundles $
--             describeWorkspaceBundlesResponse
--
--         , responseAuthorizeIPRules $
--             authorizeIPRulesResponse
--
--         , responseRebuildWorkspaces $
--             rebuildWorkspacesResponse
--
--         , responseImportWorkspaceImage $
--             importWorkspaceImageResponse
--
--         , responseModifyWorkspaceState $
--             modifyWorkspaceStateResponse
--
--         , responseCreateIPGroup $
--             createIPGroupResponse
--
--         , responseModifyWorkspaceCreationProperties $
--             modifyWorkspaceCreationPropertiesResponse
--
--         , responseRegisterWorkspaceDirectory $
--             registerWorkspaceDirectoryResponse
--
--         , responseRestoreWorkspace $
--             restoreWorkspaceResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseModifyWorkspaceAccessProperties $
--             modifyWorkspaceAccessPropertiesResponse
--
--         , responseUpdateRulesOfIPGroup $
--             updateRulesOfIPGroupResponse
--
--         , responseDeleteWorkspaceImage $
--             deleteWorkspaceImageResponse
--
--         , responseStopWorkspaces $
--             stopWorkspacesResponse
--
--         , responseAssociateIPGroups $
--             associateIPGroupsResponse
--
--         , responseModifySelfservicePermissions $
--             modifySelfservicePermissionsResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatusResponse
--
--         , responseRebootWorkspaces $
--             rebootWorkspacesResponse
--
--         , responseDeleteIPGroup $
--             deleteIPGroupResponse
--
--         , responseCopyWorkspaceImage $
--             copyWorkspaceImageResponse
--
--         , responseDescribeWorkspaceSnapshots $
--             describeWorkspaceSnapshotsResponse
--
--         , responseTerminateWorkspaces $
--             terminateWorkspacesResponse
--
--         , responseCreateWorkspaces $
--             createWorkspacesResponse
--
--         , responseDescribeClientProperties $
--             describeClientPropertiesResponse
--
--         , responseModifyClientProperties $
--             modifyClientPropertiesResponse
--
--         , responseDescribeIPGroups $
--             describeIPGroupsResponse
--
--         , responseListAvailableManagementCidrRanges $
--             listAvailableManagementCidrRangesResponse
--
--         , responseDescribeWorkspaces $
--             describeWorkspacesResponse
--
--         , responseStartWorkspaces $
--             startWorkspacesResponse
--
--         , responseDescribeAccountModifications $
--             describeAccountModificationsResponse
--
--         , responseModifyAccount $
--             modifyAccountResponse
--
--           ]
--     ]

-- Requests

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount = req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestRevokeIPRules :: RevokeIPRules -> TestTree
requestRevokeIPRules = req
    "RevokeIPRules"
    "fixture/RevokeIPRules.yaml"

requestDescribeWorkspaceImages :: DescribeWorkspaceImages -> TestTree
requestDescribeWorkspaceImages = req
    "DescribeWorkspaceImages"
    "fixture/DescribeWorkspaceImages.yaml"

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties = req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectory -> TestTree
requestDeregisterWorkspaceDirectory = req
    "DeregisterWorkspaceDirectory"
    "fixture/DeregisterWorkspaceDirectory.yaml"

requestMigrateWorkspace :: MigrateWorkspace -> TestTree
requestMigrateWorkspace = req
    "MigrateWorkspace"
    "fixture/MigrateWorkspace.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories = req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

requestDisassociateIPGroups :: DisassociateIPGroups -> TestTree
requestDisassociateIPGroups = req
    "DisassociateIPGroups"
    "fixture/DisassociateIPGroups.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles = req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestAuthorizeIPRules :: AuthorizeIPRules -> TestTree
requestAuthorizeIPRules = req
    "AuthorizeIPRules"
    "fixture/AuthorizeIPRules.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces = req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestImportWorkspaceImage :: ImportWorkspaceImage -> TestTree
requestImportWorkspaceImage = req
    "ImportWorkspaceImage"
    "fixture/ImportWorkspaceImage.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState = req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

requestCreateIPGroup :: CreateIPGroup -> TestTree
requestCreateIPGroup = req
    "CreateIPGroup"
    "fixture/CreateIPGroup.yaml"

requestModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationProperties -> TestTree
requestModifyWorkspaceCreationProperties = req
    "ModifyWorkspaceCreationProperties"
    "fixture/ModifyWorkspaceCreationProperties.yaml"

requestRegisterWorkspaceDirectory :: RegisterWorkspaceDirectory -> TestTree
requestRegisterWorkspaceDirectory = req
    "RegisterWorkspaceDirectory"
    "fixture/RegisterWorkspaceDirectory.yaml"

requestRestoreWorkspace :: RestoreWorkspace -> TestTree
requestRestoreWorkspace = req
    "RestoreWorkspace"
    "fixture/RestoreWorkspace.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessProperties -> TestTree
requestModifyWorkspaceAccessProperties = req
    "ModifyWorkspaceAccessProperties"
    "fixture/ModifyWorkspaceAccessProperties.yaml"

requestUpdateRulesOfIPGroup :: UpdateRulesOfIPGroup -> TestTree
requestUpdateRulesOfIPGroup = req
    "UpdateRulesOfIPGroup"
    "fixture/UpdateRulesOfIPGroup.yaml"

requestDeleteWorkspaceImage :: DeleteWorkspaceImage -> TestTree
requestDeleteWorkspaceImage = req
    "DeleteWorkspaceImage"
    "fixture/DeleteWorkspaceImage.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces = req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestAssociateIPGroups :: AssociateIPGroups -> TestTree
requestAssociateIPGroups = req
    "AssociateIPGroups"
    "fixture/AssociateIPGroups.yaml"

requestModifySelfservicePermissions :: ModifySelfservicePermissions -> TestTree
requestModifySelfservicePermissions = req
    "ModifySelfservicePermissions"
    "fixture/ModifySelfservicePermissions.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus = req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces = req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDeleteIPGroup :: DeleteIPGroup -> TestTree
requestDeleteIPGroup = req
    "DeleteIPGroup"
    "fixture/DeleteIPGroup.yaml"

requestCopyWorkspaceImage :: CopyWorkspaceImage -> TestTree
requestCopyWorkspaceImage = req
    "CopyWorkspaceImage"
    "fixture/CopyWorkspaceImage.yaml"

requestDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshots -> TestTree
requestDescribeWorkspaceSnapshots = req
    "DescribeWorkspaceSnapshots"
    "fixture/DescribeWorkspaceSnapshots.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces = req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces = req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDescribeClientProperties :: DescribeClientProperties -> TestTree
requestDescribeClientProperties = req
    "DescribeClientProperties"
    "fixture/DescribeClientProperties.yaml"

requestModifyClientProperties :: ModifyClientProperties -> TestTree
requestModifyClientProperties = req
    "ModifyClientProperties"
    "fixture/ModifyClientProperties.yaml"

requestDescribeIPGroups :: DescribeIPGroups -> TestTree
requestDescribeIPGroups = req
    "DescribeIPGroups"
    "fixture/DescribeIPGroups.yaml"

requestListAvailableManagementCidrRanges :: ListAvailableManagementCidrRanges -> TestTree
requestListAvailableManagementCidrRanges = req
    "ListAvailableManagementCidrRanges"
    "fixture/ListAvailableManagementCidrRanges.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces = req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces = req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

requestDescribeAccountModifications :: DescribeAccountModifications -> TestTree
requestDescribeAccountModifications = req
    "DescribeAccountModifications"
    "fixture/DescribeAccountModifications.yaml"

requestModifyAccount :: ModifyAccount -> TestTree
requestModifyAccount = req
    "ModifyAccount"
    "fixture/ModifyAccount.yaml"

-- Responses

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount = res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeAccount)

responseRevokeIPRules :: RevokeIPRulesResponse -> TestTree
responseRevokeIPRules = res
    "RevokeIPRulesResponse"
    "fixture/RevokeIPRulesResponse.proto"
    workSpaces
    (Proxy :: Proxy RevokeIPRules)

responseDescribeWorkspaceImages :: DescribeWorkspaceImagesResponse -> TestTree
responseDescribeWorkspaceImages = res
    "DescribeWorkspaceImagesResponse"
    "fixture/DescribeWorkspaceImagesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceImages)

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties = res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDeregisterWorkspaceDirectory :: DeregisterWorkspaceDirectoryResponse -> TestTree
responseDeregisterWorkspaceDirectory = res
    "DeregisterWorkspaceDirectoryResponse"
    "fixture/DeregisterWorkspaceDirectoryResponse.proto"
    workSpaces
    (Proxy :: Proxy DeregisterWorkspaceDirectory)

responseMigrateWorkspace :: MigrateWorkspaceResponse -> TestTree
responseMigrateWorkspace = res
    "MigrateWorkspaceResponse"
    "fixture/MigrateWorkspaceResponse.proto"
    workSpaces
    (Proxy :: Proxy MigrateWorkspace)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeTags)

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories = res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceDirectories)

responseDisassociateIPGroups :: DisassociateIPGroupsResponse -> TestTree
responseDisassociateIPGroups = res
    "DisassociateIPGroupsResponse"
    "fixture/DisassociateIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy DisassociateIPGroups)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles = res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseAuthorizeIPRules :: AuthorizeIPRulesResponse -> TestTree
responseAuthorizeIPRules = res
    "AuthorizeIPRulesResponse"
    "fixture/AuthorizeIPRulesResponse.proto"
    workSpaces
    (Proxy :: Proxy AuthorizeIPRules)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces = res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy RebuildWorkspaces)

responseImportWorkspaceImage :: ImportWorkspaceImageResponse -> TestTree
responseImportWorkspaceImage = res
    "ImportWorkspaceImageResponse"
    "fixture/ImportWorkspaceImageResponse.proto"
    workSpaces
    (Proxy :: Proxy ImportWorkspaceImage)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState = res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceState)

responseCreateIPGroup :: CreateIPGroupResponse -> TestTree
responseCreateIPGroup = res
    "CreateIPGroupResponse"
    "fixture/CreateIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateIPGroup)

responseModifyWorkspaceCreationProperties :: ModifyWorkspaceCreationPropertiesResponse -> TestTree
responseModifyWorkspaceCreationProperties = res
    "ModifyWorkspaceCreationPropertiesResponse"
    "fixture/ModifyWorkspaceCreationPropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceCreationProperties)

responseRegisterWorkspaceDirectory :: RegisterWorkspaceDirectoryResponse -> TestTree
responseRegisterWorkspaceDirectory = res
    "RegisterWorkspaceDirectoryResponse"
    "fixture/RegisterWorkspaceDirectoryResponse.proto"
    workSpaces
    (Proxy :: Proxy RegisterWorkspaceDirectory)

responseRestoreWorkspace :: RestoreWorkspaceResponse -> TestTree
responseRestoreWorkspace = res
    "RestoreWorkspaceResponse"
    "fixture/RestoreWorkspaceResponse.proto"
    workSpaces
    (Proxy :: Proxy RestoreWorkspace)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy DeleteTags)

responseModifyWorkspaceAccessProperties :: ModifyWorkspaceAccessPropertiesResponse -> TestTree
responseModifyWorkspaceAccessProperties = res
    "ModifyWorkspaceAccessPropertiesResponse"
    "fixture/ModifyWorkspaceAccessPropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceAccessProperties)

responseUpdateRulesOfIPGroup :: UpdateRulesOfIPGroupResponse -> TestTree
responseUpdateRulesOfIPGroup = res
    "UpdateRulesOfIPGroupResponse"
    "fixture/UpdateRulesOfIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy UpdateRulesOfIPGroup)

responseDeleteWorkspaceImage :: DeleteWorkspaceImageResponse -> TestTree
responseDeleteWorkspaceImage = res
    "DeleteWorkspaceImageResponse"
    "fixture/DeleteWorkspaceImageResponse.proto"
    workSpaces
    (Proxy :: Proxy DeleteWorkspaceImage)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces = res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy StopWorkspaces)

responseAssociateIPGroups :: AssociateIPGroupsResponse -> TestTree
responseAssociateIPGroups = res
    "AssociateIPGroupsResponse"
    "fixture/AssociateIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy AssociateIPGroups)

responseModifySelfservicePermissions :: ModifySelfservicePermissionsResponse -> TestTree
responseModifySelfservicePermissions = res
    "ModifySelfservicePermissionsResponse"
    "fixture/ModifySelfservicePermissionsResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifySelfservicePermissions)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus = res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces = res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy RebootWorkspaces)

responseDeleteIPGroup :: DeleteIPGroupResponse -> TestTree
responseDeleteIPGroup = res
    "DeleteIPGroupResponse"
    "fixture/DeleteIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy DeleteIPGroup)

responseCopyWorkspaceImage :: CopyWorkspaceImageResponse -> TestTree
responseCopyWorkspaceImage = res
    "CopyWorkspaceImageResponse"
    "fixture/CopyWorkspaceImageResponse.proto"
    workSpaces
    (Proxy :: Proxy CopyWorkspaceImage)

responseDescribeWorkspaceSnapshots :: DescribeWorkspaceSnapshotsResponse -> TestTree
responseDescribeWorkspaceSnapshots = res
    "DescribeWorkspaceSnapshotsResponse"
    "fixture/DescribeWorkspaceSnapshotsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceSnapshots)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces = res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy TerminateWorkspaces)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces = res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateWorkspaces)

responseDescribeClientProperties :: DescribeClientPropertiesResponse -> TestTree
responseDescribeClientProperties = res
    "DescribeClientPropertiesResponse"
    "fixture/DescribeClientPropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeClientProperties)

responseModifyClientProperties :: ModifyClientPropertiesResponse -> TestTree
responseModifyClientProperties = res
    "ModifyClientPropertiesResponse"
    "fixture/ModifyClientPropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyClientProperties)

responseDescribeIPGroups :: DescribeIPGroupsResponse -> TestTree
responseDescribeIPGroups = res
    "DescribeIPGroupsResponse"
    "fixture/DescribeIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeIPGroups)

responseListAvailableManagementCidrRanges :: ListAvailableManagementCidrRangesResponse -> TestTree
responseListAvailableManagementCidrRanges = res
    "ListAvailableManagementCidrRangesResponse"
    "fixture/ListAvailableManagementCidrRangesResponse.proto"
    workSpaces
    (Proxy :: Proxy ListAvailableManagementCidrRanges)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces = res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaces)

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces = res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy StartWorkspaces)

responseDescribeAccountModifications :: DescribeAccountModificationsResponse -> TestTree
responseDescribeAccountModifications = res
    "DescribeAccountModificationsResponse"
    "fixture/DescribeAccountModificationsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeAccountModifications)

responseModifyAccount :: ModifyAccountResponse -> TestTree
responseModifyAccount = res
    "ModifyAccountResponse"
    "fixture/ModifyAccountResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyAccount)
