{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.Product (
    module Network.AWS.CodeCommit.Types.Approval,
    module Network.AWS.CodeCommit.Types.ApprovalRule,
    module Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata,
    module Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata,
    module Network.AWS.CodeCommit.Types.ApprovalRuleTemplate,
    module Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata,
    module Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError,
    module Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError,
    module Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError,
    module Network.AWS.CodeCommit.Types.BatchGetCommitsError,
    module Network.AWS.CodeCommit.Types.BlobMetadata,
    module Network.AWS.CodeCommit.Types.BranchInfo,
    module Network.AWS.CodeCommit.Types.Comment,
    module Network.AWS.CodeCommit.Types.CommentsForComparedCommit,
    module Network.AWS.CodeCommit.Types.CommentsForPullRequest,
    module Network.AWS.CodeCommit.Types.Commit,
    module Network.AWS.CodeCommit.Types.Conflict,
    module Network.AWS.CodeCommit.Types.ConflictMetadata,
    module Network.AWS.CodeCommit.Types.ConflictResolution,
    module Network.AWS.CodeCommit.Types.DeleteFileEntry,
    module Network.AWS.CodeCommit.Types.Difference,
    module Network.AWS.CodeCommit.Types.Evaluation,
    module Network.AWS.CodeCommit.Types.File,
    module Network.AWS.CodeCommit.Types.FileMetadata,
    module Network.AWS.CodeCommit.Types.FileModes,
    module Network.AWS.CodeCommit.Types.FileSizes,
    module Network.AWS.CodeCommit.Types.Folder,
    module Network.AWS.CodeCommit.Types.IsBinaryFile,
    module Network.AWS.CodeCommit.Types.Location,
    module Network.AWS.CodeCommit.Types.MergeHunk,
    module Network.AWS.CodeCommit.Types.MergeHunkDetail,
    module Network.AWS.CodeCommit.Types.MergeMetadata,
    module Network.AWS.CodeCommit.Types.MergeOperations,
    module Network.AWS.CodeCommit.Types.ObjectTypes,
    module Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate,
    module Network.AWS.CodeCommit.Types.PullRequest,
    module Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata,
    module Network.AWS.CodeCommit.Types.PullRequestEvent,
    module Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata,
    module Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata,
    module Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata,
    module Network.AWS.CodeCommit.Types.PullRequestTarget,
    module Network.AWS.CodeCommit.Types.PutFileEntry,
    module Network.AWS.CodeCommit.Types.ReplaceContentEntry,
    module Network.AWS.CodeCommit.Types.RepositoryMetadata,
    module Network.AWS.CodeCommit.Types.RepositoryNameIdPair,
    module Network.AWS.CodeCommit.Types.RepositoryTrigger,
    module Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure,
    module Network.AWS.CodeCommit.Types.SetFileModeEntry,
    module Network.AWS.CodeCommit.Types.SourceFileSpecifier,
    module Network.AWS.CodeCommit.Types.SubModule,
    module Network.AWS.CodeCommit.Types.SymbolicLink,
    module Network.AWS.CodeCommit.Types.Target,
    module Network.AWS.CodeCommit.Types.UserInfo
  ) where

import Network.AWS.CodeCommit.Types.Approval
import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
import Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Network.AWS.CodeCommit.Types.BatchGetCommitsError
import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.BranchInfo
import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.CommentsForComparedCommit
import Network.AWS.CodeCommit.Types.CommentsForPullRequest
import Network.AWS.CodeCommit.Types.Commit
import Network.AWS.CodeCommit.Types.Conflict
import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.ConflictResolution
import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.Difference
import Network.AWS.CodeCommit.Types.Evaluation
import Network.AWS.CodeCommit.Types.File
import Network.AWS.CodeCommit.Types.FileMetadata
import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.Folder
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.Location
import Network.AWS.CodeCommit.Types.MergeHunk
import Network.AWS.CodeCommit.Types.MergeHunkDetail
import Network.AWS.CodeCommit.Types.MergeMetadata
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.ObjectTypes
import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.PullRequest
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEvent
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestTarget
import Network.AWS.CodeCommit.Types.PutFileEntry
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.RepositoryMetadata
import Network.AWS.CodeCommit.Types.RepositoryNameIdPair
import Network.AWS.CodeCommit.Types.RepositoryTrigger
import Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import Network.AWS.CodeCommit.Types.SubModule
import Network.AWS.CodeCommit.Types.SymbolicLink
import Network.AWS.CodeCommit.Types.Target
import Network.AWS.CodeCommit.Types.UserInfo
import Network.AWS.Lens
import Network.AWS.Prelude
