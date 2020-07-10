{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Product (
    module Network.AWS.WorkSpaces.Types.AccountModification,
    module Network.AWS.WorkSpaces.Types.ClientProperties,
    module Network.AWS.WorkSpaces.Types.ClientPropertiesResult,
    module Network.AWS.WorkSpaces.Types.ComputeType,
    module Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties,
    module Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest,
    module Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest,
    module Network.AWS.WorkSpaces.Types.IPRuleItem,
    module Network.AWS.WorkSpaces.Types.ModificationState,
    module Network.AWS.WorkSpaces.Types.OperatingSystem,
    module Network.AWS.WorkSpaces.Types.RebootRequest,
    module Network.AWS.WorkSpaces.Types.RebuildRequest,
    module Network.AWS.WorkSpaces.Types.RootStorage,
    module Network.AWS.WorkSpaces.Types.SelfservicePermissions,
    module Network.AWS.WorkSpaces.Types.Snapshot,
    module Network.AWS.WorkSpaces.Types.StartRequest,
    module Network.AWS.WorkSpaces.Types.StopRequest,
    module Network.AWS.WorkSpaces.Types.Tag,
    module Network.AWS.WorkSpaces.Types.TerminateRequest,
    module Network.AWS.WorkSpaces.Types.UserStorage,
    module Network.AWS.WorkSpaces.Types.Workspace,
    module Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties,
    module Network.AWS.WorkSpaces.Types.WorkspaceBundle,
    module Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus,
    module Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties,
    module Network.AWS.WorkSpaces.Types.WorkspaceDirectory,
    module Network.AWS.WorkSpaces.Types.WorkspaceImage,
    module Network.AWS.WorkSpaces.Types.WorkspaceProperties,
    module Network.AWS.WorkSpaces.Types.WorkspaceRequest,
    module Network.AWS.WorkSpaces.Types.WorkspacesIPGroup
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.AccountModification
import Network.AWS.WorkSpaces.Types.ClientProperties
import Network.AWS.WorkSpaces.Types.ClientPropertiesResult
import Network.AWS.WorkSpaces.Types.ComputeType
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.FailedCreateWorkspaceRequest
import Network.AWS.WorkSpaces.Types.FailedWorkspaceChangeRequest
import Network.AWS.WorkSpaces.Types.IPRuleItem
import Network.AWS.WorkSpaces.Types.ModificationState
import Network.AWS.WorkSpaces.Types.OperatingSystem
import Network.AWS.WorkSpaces.Types.RebootRequest
import Network.AWS.WorkSpaces.Types.RebuildRequest
import Network.AWS.WorkSpaces.Types.RootStorage
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Snapshot
import Network.AWS.WorkSpaces.Types.StartRequest
import Network.AWS.WorkSpaces.Types.StopRequest
import Network.AWS.WorkSpaces.Types.Tag
import Network.AWS.WorkSpaces.Types.TerminateRequest
import Network.AWS.WorkSpaces.Types.UserStorage
import Network.AWS.WorkSpaces.Types.Workspace
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceBundle
import Network.AWS.WorkSpaces.Types.WorkspaceConnectionStatus
import Network.AWS.WorkSpaces.Types.WorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectory
import Network.AWS.WorkSpaces.Types.WorkspaceImage
import Network.AWS.WorkSpaces.Types.WorkspaceProperties
import Network.AWS.WorkSpaces.Types.WorkspaceRequest
import Network.AWS.WorkSpaces.Types.WorkspacesIPGroup
