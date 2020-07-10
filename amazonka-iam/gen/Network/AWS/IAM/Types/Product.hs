{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.Product (
    module Network.AWS.IAM.Types.AccessDetail,
    module Network.AWS.IAM.Types.AccessKeyInfo,
    module Network.AWS.IAM.Types.AccessKeyLastUsed,
    module Network.AWS.IAM.Types.AccessKeyMetadata,
    module Network.AWS.IAM.Types.AttachedPermissionsBoundary,
    module Network.AWS.IAM.Types.AttachedPolicy,
    module Network.AWS.IAM.Types.ContextEntry,
    module Network.AWS.IAM.Types.DeletionTaskFailureReasonType,
    module Network.AWS.IAM.Types.EntityDetails,
    module Network.AWS.IAM.Types.EntityInfo,
    module Network.AWS.IAM.Types.ErrorDetails,
    module Network.AWS.IAM.Types.EvaluationResult,
    module Network.AWS.IAM.Types.GetContextKeysForPolicyResponse,
    module Network.AWS.IAM.Types.Group,
    module Network.AWS.IAM.Types.GroupDetail,
    module Network.AWS.IAM.Types.InstanceProfile,
    module Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry,
    module Network.AWS.IAM.Types.LoginProfile,
    module Network.AWS.IAM.Types.MFADevice,
    module Network.AWS.IAM.Types.ManagedPolicyDetail,
    module Network.AWS.IAM.Types.OpenIdConnectProviderListEntry,
    module Network.AWS.IAM.Types.OrganizationsDecisionDetail,
    module Network.AWS.IAM.Types.PasswordPolicy,
    module Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail,
    module Network.AWS.IAM.Types.Policy,
    module Network.AWS.IAM.Types.PolicyDetail,
    module Network.AWS.IAM.Types.PolicyGrantingServiceAccess,
    module Network.AWS.IAM.Types.PolicyGroup,
    module Network.AWS.IAM.Types.PolicyRole,
    module Network.AWS.IAM.Types.PolicyUser,
    module Network.AWS.IAM.Types.PolicyVersion,
    module Network.AWS.IAM.Types.Position,
    module Network.AWS.IAM.Types.ResourceSpecificResult,
    module Network.AWS.IAM.Types.Role,
    module Network.AWS.IAM.Types.RoleDetail,
    module Network.AWS.IAM.Types.RoleLastUsed,
    module Network.AWS.IAM.Types.RoleUsageType,
    module Network.AWS.IAM.Types.SAMLProviderListEntry,
    module Network.AWS.IAM.Types.SSHPublicKey,
    module Network.AWS.IAM.Types.SSHPublicKeyMetadata,
    module Network.AWS.IAM.Types.ServerCertificate,
    module Network.AWS.IAM.Types.ServerCertificateMetadata,
    module Network.AWS.IAM.Types.ServiceLastAccessed,
    module Network.AWS.IAM.Types.ServiceSpecificCredential,
    module Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata,
    module Network.AWS.IAM.Types.SigningCertificate,
    module Network.AWS.IAM.Types.SimulatePolicyResponse,
    module Network.AWS.IAM.Types.Statement,
    module Network.AWS.IAM.Types.Tag,
    module Network.AWS.IAM.Types.User,
    module Network.AWS.IAM.Types.UserDetail,
    module Network.AWS.IAM.Types.VirtualMFADevice
  ) where

import Network.AWS.IAM.Types.AccessDetail
import Network.AWS.IAM.Types.AccessKeyInfo
import Network.AWS.IAM.Types.AccessKeyLastUsed
import Network.AWS.IAM.Types.AccessKeyMetadata
import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.ContextEntry
import Network.AWS.IAM.Types.DeletionTaskFailureReasonType
import Network.AWS.IAM.Types.EntityDetails
import Network.AWS.IAM.Types.EntityInfo
import Network.AWS.IAM.Types.ErrorDetails
import Network.AWS.IAM.Types.EvaluationResult
import Network.AWS.IAM.Types.GetContextKeysForPolicyResponse
import Network.AWS.IAM.Types.Group
import Network.AWS.IAM.Types.GroupDetail
import Network.AWS.IAM.Types.InstanceProfile
import Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
import Network.AWS.IAM.Types.LoginProfile
import Network.AWS.IAM.Types.MFADevice
import Network.AWS.IAM.Types.ManagedPolicyDetail
import Network.AWS.IAM.Types.OpenIdConnectProviderListEntry
import Network.AWS.IAM.Types.OrganizationsDecisionDetail
import Network.AWS.IAM.Types.PasswordPolicy
import Network.AWS.IAM.Types.PermissionsBoundaryDecisionDetail
import Network.AWS.IAM.Types.Policy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
import Network.AWS.IAM.Types.PolicyGroup
import Network.AWS.IAM.Types.PolicyRole
import Network.AWS.IAM.Types.PolicyUser
import Network.AWS.IAM.Types.PolicyVersion
import Network.AWS.IAM.Types.Position
import Network.AWS.IAM.Types.ResourceSpecificResult
import Network.AWS.IAM.Types.Role
import Network.AWS.IAM.Types.RoleDetail
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.RoleUsageType
import Network.AWS.IAM.Types.SAMLProviderListEntry
import Network.AWS.IAM.Types.SSHPublicKey
import Network.AWS.IAM.Types.SSHPublicKeyMetadata
import Network.AWS.IAM.Types.ServerCertificate
import Network.AWS.IAM.Types.ServerCertificateMetadata
import Network.AWS.IAM.Types.ServiceLastAccessed
import Network.AWS.IAM.Types.ServiceSpecificCredential
import Network.AWS.IAM.Types.ServiceSpecificCredentialMetadata
import Network.AWS.IAM.Types.SigningCertificate
import Network.AWS.IAM.Types.SimulatePolicyResponse
import Network.AWS.IAM.Types.Statement
import Network.AWS.IAM.Types.Tag
import Network.AWS.IAM.Types.User
import Network.AWS.IAM.Types.UserDetail
import Network.AWS.IAM.Types.VirtualMFADevice
import Network.AWS.Lens
import Network.AWS.Prelude
