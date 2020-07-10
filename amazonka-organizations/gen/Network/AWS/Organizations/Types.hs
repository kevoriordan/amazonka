{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types
    (
    -- * Service Configuration
      organizations

    -- * Errors
    , _FinalizingOrganizationException
    , _AccountNotRegisteredException
    , _PolicyInUseException
    , _AccountAlreadyRegisteredException
    , _DuplicateOrganizationalUnitException
    , _PolicyChangesInProgressException
    , _AccessDeniedForDependencyException
    , _PolicyNotAttachedException
    , _AlreadyInOrganizationException
    , _CreateAccountStatusNotFoundException
    , _RootNotFoundException
    , _TooManyRequestsException
    , _ServiceException
    , _HandshakeConstraintViolationException
    , _PolicyNotFoundException
    , _AccessDeniedException
    , _PolicyTypeAlreadyEnabledException
    , _DestinationParentNotFoundException
    , _AccountOwnerNotVerifiedException
    , _EffectivePolicyNotFoundException
    , _DuplicateAccountException
    , _ChildNotFoundException
    , _InvalidHandshakeTransitionException
    , _ConstraintViolationException
    , _PolicyTypeNotAvailableForOrganizationException
    , _HandshakeNotFoundException
    , _AWSOrganizationsNotInUseException
    , _ParentNotFoundException
    , _DuplicatePolicyException
    , _InvalidInputException
    , _SourceParentNotFoundException
    , _TargetNotFoundException
    , _AccountNotFoundException
    , _MasterCannotLeaveOrganizationException
    , _ConcurrentModificationException
    , _DuplicateHandshakeException
    , _OrganizationalUnitNotEmptyException
    , _MalformedPolicyDocumentException
    , _PolicyTypeNotEnabledException
    , _OrganizationNotEmptyException
    , _OrganizationalUnitNotFoundException
    , _DuplicatePolicyAttachmentException
    , _UnsupportedAPIEndpointException
    , _HandshakeAlreadyInStateException

    -- * AccountJoinedMethod
    , AccountJoinedMethod (..)

    -- * AccountStatus
    , AccountStatus (..)

    -- * ActionType
    , ActionType (..)

    -- * ChildType
    , ChildType (..)

    -- * CreateAccountFailureReason
    , CreateAccountFailureReason (..)

    -- * CreateAccountState
    , CreateAccountState (..)

    -- * EffectivePolicyType
    , EffectivePolicyType (..)

    -- * HandshakePartyType
    , HandshakePartyType (..)

    -- * HandshakeResourceType
    , HandshakeResourceType (..)

    -- * HandshakeState
    , HandshakeState (..)

    -- * IAMUserAccessToBilling
    , IAMUserAccessToBilling (..)

    -- * OrganizationFeatureSet
    , OrganizationFeatureSet (..)

    -- * ParentType
    , ParentType (..)

    -- * PolicyType
    , PolicyType (..)

    -- * PolicyTypeStatus
    , PolicyTypeStatus (..)

    -- * TargetType
    , TargetType (..)

    -- * Account
    , Account
    , account
    , aStatus
    , aJoinedMethod
    , aEmail
    , aARN
    , aJoinedTimestamp
    , aName
    , aId

    -- * Child
    , Child
    , child
    , cId
    , cType

    -- * CreateAccountStatus
    , CreateAccountStatus
    , createAccountStatus
    , casFailureReason
    , casState
    , casCompletedTimestamp
    , casAccountName
    , casAccountId
    , casId
    , casGovCloudAccountId
    , casRequestedTimestamp

    -- * DelegatedAdministrator
    , DelegatedAdministrator
    , delegatedAdministrator
    , daStatus
    , daJoinedMethod
    , daEmail
    , daARN
    , daJoinedTimestamp
    , daDelegationEnabledDate
    , daName
    , daId

    -- * DelegatedService
    , DelegatedService
    , delegatedService
    , dsServicePrincipal
    , dsDelegationEnabledDate

    -- * EffectivePolicy
    , EffectivePolicy
    , effectivePolicy
    , epTargetId
    , epPolicyType
    , epLastUpdatedTimestamp
    , epPolicyContent

    -- * EnabledServicePrincipal
    , EnabledServicePrincipal
    , enabledServicePrincipal
    , espServicePrincipal
    , espDateEnabled

    -- * Handshake
    , Handshake
    , handshake
    , hState
    , hARN
    , hAction
    , hResources
    , hId
    , hExpirationTimestamp
    , hParties
    , hRequestedTimestamp

    -- * HandshakeFilter
    , HandshakeFilter
    , handshakeFilter
    , hfParentHandshakeId
    , hfActionType

    -- * HandshakeParty
    , HandshakeParty
    , handshakeParty
    , hpId
    , hpType

    -- * HandshakeResource
    , HandshakeResource
    , handshakeResource
    , hrValue
    , hrResources
    , hrType

    -- * Organization
    , Organization
    , organization
    , oARN
    , oMasterAccountId
    , oMasterAccountARN
    , oMasterAccountEmail
    , oAvailablePolicyTypes
    , oId
    , oFeatureSet

    -- * OrganizationalUnit
    , OrganizationalUnit
    , organizationalUnit
    , ouARN
    , ouName
    , ouId

    -- * Parent
    , Parent
    , parent
    , pId
    , pType

    -- * Policy
    , Policy
    , policy
    , pContent
    , pPolicySummary

    -- * PolicySummary
    , PolicySummary
    , policySummary
    , psARN
    , psName
    , psId
    , psAWSManaged
    , psType
    , psDescription

    -- * PolicyTargetSummary
    , PolicyTargetSummary
    , policyTargetSummary
    , polTargetId
    , polARN
    , polName
    , polType

    -- * PolicyTypeSummary
    , PolicyTypeSummary
    , policyTypeSummary
    , ptsStatus
    , ptsType

    -- * Root
    , Root
    , root
    , rARN
    , rName
    , rId
    , rPolicyTypes

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Organizations.Types.ChildType
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountState
import Network.AWS.Organizations.Types.EffectivePolicyType
import Network.AWS.Organizations.Types.HandshakePartyType
import Network.AWS.Organizations.Types.HandshakeResourceType
import Network.AWS.Organizations.Types.HandshakeState
import Network.AWS.Organizations.Types.IAMUserAccessToBilling
import Network.AWS.Organizations.Types.OrganizationFeatureSet
import Network.AWS.Organizations.Types.ParentType
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Organizations.Types.PolicyTypeStatus
import Network.AWS.Organizations.Types.TargetType
import Network.AWS.Organizations.Types.Account
import Network.AWS.Organizations.Types.Child
import Network.AWS.Organizations.Types.CreateAccountStatus
import Network.AWS.Organizations.Types.DelegatedAdministrator
import Network.AWS.Organizations.Types.DelegatedService
import Network.AWS.Organizations.Types.EffectivePolicy
import Network.AWS.Organizations.Types.EnabledServicePrincipal
import Network.AWS.Organizations.Types.Handshake
import Network.AWS.Organizations.Types.HandshakeFilter
import Network.AWS.Organizations.Types.HandshakeParty
import Network.AWS.Organizations.Types.HandshakeResource
import Network.AWS.Organizations.Types.Organization
import Network.AWS.Organizations.Types.OrganizationalUnit
import Network.AWS.Organizations.Types.Parent
import Network.AWS.Organizations.Types.Policy
import Network.AWS.Organizations.Types.PolicySummary
import Network.AWS.Organizations.Types.PolicyTargetSummary
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Organizations.Types.Root
import Network.AWS.Organizations.Types.Tag

-- | API version @2016-11-28@ of the Amazon Organizations SDK configuration.
organizations :: Service
organizations
  = Service{_svcAbbrev = "Organizations",
            _svcSigner = v4, _svcPrefix = "organizations",
            _svcVersion = "2016-11-28",
            _svcEndpoint = defaultEndpoint organizations,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Organizations",
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

-- | AWS Organizations couldn't perform the operation because your organization hasn't finished initializing. This can take up to an hour. Try again later. If after one hour you continue to receive this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
_FinalizingOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_FinalizingOrganizationException
  = _MatchServiceError organizations
      "FinalizingOrganizationException"

-- | The specified account is not a delegated administrator for this AWS service. 
--
--
_AccountNotRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountNotRegisteredException
  = _MatchServiceError organizations
      "AccountNotRegisteredException"

-- | The policy is attached to one or more entities. You must detach it from all roots, OUs, and accounts before performing this operation.
--
--
_PolicyInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyInUseException
  = _MatchServiceError organizations
      "PolicyInUseException"

-- | The specified account is already a delegated administrator for this AWS service.
--
--
_AccountAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountAlreadyRegisteredException
  = _MatchServiceError organizations
      "AccountAlreadyRegisteredException"

-- | An OU with the same name already exists.
--
--
_DuplicateOrganizationalUnitException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateOrganizationalUnitException
  = _MatchServiceError organizations
      "DuplicateOrganizationalUnitException"

-- | Changes to the effective policy are in progress, and its contents can't be returned. Try the operation again later. 
--
--
_PolicyChangesInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyChangesInProgressException
  = _MatchServiceError organizations
      "PolicyChangesInProgressException"

-- | The operation that you attempted requires you to have the @iam:CreateServiceLinkedRole@ for @organizations.amazonaws.com@ permission so that AWS Organizations can create the required service-linked role. You don't have that permission.
--
--
_AccessDeniedForDependencyException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedForDependencyException
  = _MatchServiceError organizations
      "AccessDeniedForDependencyException"

-- | The policy isn't attached to the specified target in the specified root.
--
--
_PolicyNotAttachedException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotAttachedException
  = _MatchServiceError organizations
      "PolicyNotAttachedException"

-- | This account is already a member of an organization. An account can belong to only one organization at a time.
--
--
_AlreadyInOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_AlreadyInOrganizationException
  = _MatchServiceError organizations
      "AlreadyInOrganizationException"

-- | We can't find an create account request with the @CreateAccountRequestId@ that you specified.
--
--
_CreateAccountStatusNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_CreateAccountStatusNotFoundException
  = _MatchServiceError organizations
      "CreateAccountStatusNotFoundException"

-- | We can't find a root with the @RootId@ that you specified.
--
--
_RootNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_RootNotFoundException
  = _MatchServiceError organizations
      "RootNotFoundException"

-- | You have sent too many requests in too short a period of time. The limit helps protect against denial-of-service attacks. Try again later.
--
--
-- For information on limits that affect AWS Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_limits.html Limits of AWS Organizations> in the /AWS Organizations User Guide./ 
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException
  = _MatchServiceError organizations
      "TooManyRequestsException"

-- | AWS Organizations can't complete your request because of an internal service error. Try again later.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException
  = _MatchServiceError organizations "ServiceException"

-- | The requested operation would violate the constraint identified in the reason code.
--
--
--     * ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. Note that deleted and closed accounts still count toward your limit.
--
-- /Important:/ If you get this exception immediately after creating the organization, wait one hour and try again. If after an hour it continues to fail with this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--     * ALREADY_IN_AN_ORGANIZATION: The handshake request is invalid because the invited account is already a member of an organization.
--
--     * HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes that you can send in one day.
--
--     * INVITE_DISABLED_DURING_ENABLE_ALL_FEATURES: You can't issue new invitations to join an organization while it's in the process of enabling all features. You can resume inviting accounts after you finalize the process when all accounts have agreed to the change.
--
--     * ORGANIZATION_ALREADY_HAS_ALL_FEATURES: The handshake request is invalid because the organization has already enabled all features.
--
--     * ORGANIZATION_FROM_DIFFERENT_SELLER_OF_RECORD: The request failed because the account is from a different marketplace than the accounts in the organization. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be from the same marketplace.
--
--     * ORGANIZATION_MEMBERSHIP_CHANGE_RATE_LIMIT_EXCEEDED: You attempted to change the membership of an account too quickly after its previous change.
--
--     * PAYMENT_INSTRUMENT_REQUIRED: You can't complete the operation with an account that doesn't have a payment instrument, such as a credit card, associated with it.
--
--
--
_HandshakeConstraintViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeConstraintViolationException
  = _MatchServiceError organizations
      "HandshakeConstraintViolationException"

-- | We can't find a policy with the @PolicyId@ that you specified.
--
--
_PolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyNotFoundException
  = _MatchServiceError organizations
      "PolicyNotFoundException"

-- | You don't have permissions to perform the requested operation. The user or role that is making the request must have at least one IAM permissions policy attached that grants the required permissions. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access Management> in the /IAM User Guide./ 
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError organizations
      "AccessDeniedException"

-- | The specified policy type is already enabled in the specified root.
--
--
_PolicyTypeAlreadyEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeAlreadyEnabledException
  = _MatchServiceError organizations
      "PolicyTypeAlreadyEnabledException"

-- | We can't find the destination container (a root or OU) with the @ParentId@ that you specified.
--
--
_DestinationParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_DestinationParentNotFoundException
  = _MatchServiceError organizations
      "DestinationParentNotFoundException"

-- | You can't invite an existing account to your organization until you verify that you own the email address associated with the master account. For more information, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_create.html#about-email-verification Email Address Verification> in the /AWS Organizations User Guide./ 
--
--
_AccountOwnerNotVerifiedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountOwnerNotVerifiedException
  = _MatchServiceError organizations
      "AccountOwnerNotVerifiedException"

-- | If you ran this action on the master account, this policy type is not enabled. If you ran the action on a member account, the account doesn't have an effective policy of this type. Contact the administrator of your organization about attaching a policy of this type to the account. 
--
--
_EffectivePolicyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EffectivePolicyNotFoundException
  = _MatchServiceError organizations
      "EffectivePolicyNotFoundException"

-- | That account is already present in the specified destination.
--
--
_DuplicateAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateAccountException
  = _MatchServiceError organizations
      "DuplicateAccountException"

-- | We can't find an organizational unit (OU) or AWS account with the @ChildId@ that you specified.
--
--
_ChildNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ChildNotFoundException
  = _MatchServiceError organizations
      "ChildNotFoundException"

-- | You can't perform the operation on the handshake in its current state. For example, you can't cancel a handshake that was already accepted or accept a handshake that was already declined.
--
--
_InvalidHandshakeTransitionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidHandshakeTransitionException
  = _MatchServiceError organizations
      "InvalidHandshakeTransitionException"

-- | Performing this operation violates a minimum or maximum value limit. For example, attempting to remove the last service control policy (SCP) from an OU or root, inviting or creating too many accounts to the organization, or attaching too many policies to an account, OU, or root. This exception includes a reason that contains additional information about the violated limit.
--
--
-- Some of the reasons in the following list might not be applicable to this specific API or operation:
--
--     * ACCOUNT_CANNOT_LEAVE_WITHOUT_EULA: You attempted to remove an account from the organization that doesn't yet have enough information to exist as a standalone account. This account requires you to first agree to the AWS Customer Agreement. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./ 
--
--     * ACCOUNT_CANNOT_LEAVE_WITHOUT_PHONE_VERIFICATION: You attempted to remove an account from the organization that doesn't yet have enough information to exist as a standalone account. This account requires you to first complete phone verification. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./ 
--
--     * ACCOUNT_CREATION_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of accounts that you can create in one day.
--
--     * ACCOUNT_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the limit on the number of accounts in an organization. If you need more accounts, contact <https://console.aws.amazon.com/support/home#/ AWS Support> to request an increase in your limit. 
--
-- Or the number of invitations that you tried to send would cause you to exceed the limit of accounts in your organization. Send fewer invitations or contact AWS Support to request an increase in the number of accounts.
--
-- /Important:/ If you get receive this exception when running a command immediately after creating the organization, wait one hour and try again. If after an hour it continues to fail with this error, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--     * CANNOT_REGISTER_MASTER_AS_DELEGATED_ADMINISTRATOR: You can designate only a member account as a delegated administrator.
--
--     * CANNOT_REMOVE_DELEGATED_ADMINISTRATOR_FROM_ORG: To complete this operation, you must first deregister this account as a delegated administrator. 
--
--     * DELEGATED_ADMINISTRATOR_EXISTS_FOR_THIS_SERVICE: To complete this operation, you must first deregister all delegated administrators for this service.
--
--     * HANDSHAKE_RATE_LIMIT_EXCEEDED: You attempted to exceed the number of handshakes that you can send in one day.
--
--     * MASTER_ACCOUNT_ADDRESS_DOES_NOT_MATCH_MARKETPLACE: To create an account in this organization, you first must migrate the organization's master account to the marketplace that corresponds to the master account's address. For example, accounts with India addresses must be associated with the AISPL marketplace. All accounts in an organization must be associated with the same marketplace.
--
--     * MASTER_ACCOUNT_MISSING_CONTACT_INFO: To complete this operation, you must first provide contact a valid address and phone number for the master account. Then try the operation again.
--
--     * MASTER_ACCOUNT_NOT_GOVCLOUD_ENABLED: To complete this operation, the master account must have an associated account in the AWS GovCloud (US-West) Region. For more information, see <http://docs.aws.amazon.com/govcloud-us/latest/UserGuide/govcloud-organizations.html AWS Organizations> in the /AWS GovCloud User Guide./ 
--
--     * MASTER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To create an organization with this master account, you first must associate a valid payment instrument, such as a credit card, with the account. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./ 
--
--     * MAX_DELEGATED_ADMINISTRATORS_FOR_SERVICE_LIMIT_EXCEEDED: You attempted to register more delegated administrators than allowed for the service principal. 
--
--     * MAX_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to exceed the number of policies of a certain type that can be attached to an entity at one time.
--
--     * MAX_TAG_LIMIT_EXCEEDED: You have exceeded the number of tags allowed on this resource. 
--
--     * MEMBER_ACCOUNT_PAYMENT_INSTRUMENT_REQUIRED: To complete this operation with this member account, you first must associate a valid payment instrument, such as a credit card, with the account. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./ 
--
--     * MIN_POLICY_TYPE_ATTACHMENT_LIMIT_EXCEEDED: You attempted to detach a policy from an entity that would cause the entity to have fewer than the minimum number of policies of a certain type required.
--
--     * OU_DEPTH_LIMIT_EXCEEDED: You attempted to create an OU tree that is too many levels deep.
--
--     * ORGANIZATION_NOT_IN_ALL_FEATURES_MODE: You attempted to perform an operation that requires the organization to be configured to support all features. An organization that supports only consolidated billing features can't perform this operation.
--
--     * OU_NUMBER_LIMIT_EXCEEDED: You attempted to exceed the number of OUs that you can have in an organization.
--
--     * POLICY_NUMBER_LIMIT_EXCEEDED. You attempted to exceed the number of policies that you can have in an organization.
--
--
--
_ConstraintViolationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConstraintViolationException
  = _MatchServiceError organizations
      "ConstraintViolationException"

-- | You can't use the specified policy type with the feature set currently enabled for this organization. For example, you can enable SCPs only after you enable all features in the organization. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies.html#enable_policies_on_root Enabling and Disabling a Policy Type on a Root> in the /AWS Organizations User Guide./ 
--
--
_PolicyTypeNotAvailableForOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotAvailableForOrganizationException
  = _MatchServiceError organizations
      "PolicyTypeNotAvailableForOrganizationException"

-- | We can't find a handshake with the @HandshakeId@ that you specified.
--
--
_HandshakeNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeNotFoundException
  = _MatchServiceError organizations
      "HandshakeNotFoundException"

-- | Your account isn't a member of an organization. To make this request, you must use the credentials of an account that belongs to an organization.
--
--
_AWSOrganizationsNotInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_AWSOrganizationsNotInUseException
  = _MatchServiceError organizations
      "AWSOrganizationsNotInUseException"

-- | We can't find a root or OU with the @ParentId@ that you specified.
--
--
_ParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ParentNotFoundException
  = _MatchServiceError organizations
      "ParentNotFoundException"

-- | A policy with the same name already exists.
--
--
_DuplicatePolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyException
  = _MatchServiceError organizations
      "DuplicatePolicyException"

-- | The requested operation failed because you provided invalid values for one or more of the request parameters. This exception includes a reason that contains additional information about the violated limit:
--
--
--     * IMMUTABLE_POLICY: You specified a policy that is managed by AWS and can't be modified.
--
--     * INPUT_REQUIRED: You must include a value for all required parameters.
--
--     * INVALID_ENUM: You specified an invalid value.
--
--     * INVALID_FULL_NAME_TARGET: You specified a full name that contains invalid characters.
--
--     * INVALID_LIST_MEMBER: You provided a list to a parameter that contains at least one invalid value.
--
--     * INVALID_PAGINATION_TOKEN: Get the value for the @NextToken@ parameter from the response to a previous call of the operation.
--
--     * INVALID_PARTY_TYPE_TARGET: You specified the wrong type of entity (account, organization, or email) as a party.
--
--     * INVALID_PATTERN: You provided a value that doesn't match the required pattern.
--
--     * INVALID_PATTERN_TARGET_ID: You specified a policy target ID that doesn't match the required pattern.
--
--     * INVALID_ROLE_NAME: You provided a role name that isn't valid. A role name can't begin with the reserved prefix @AWSServiceRoleFor@ .
--
--     * INVALID_SYNTAX_ORGANIZATION_ARN: You specified an invalid Amazon Resource Name (ARN) for the organization.
--
--     * INVALID_SYNTAX_POLICY_ID: You specified an invalid policy ID. 
--
--     * INVALID_SYSTEM_TAGS_PARAMETER: You specified a tag key that is a system tag. You can’t add, edit, or delete system tag keys because they're reserved for AWS use. System tags don’t count against your tags per resource limit.
--
--     * MAX_FILTER_LIMIT_EXCEEDED: You can specify only one filter parameter for the operation.
--
--     * MAX_LENGTH_EXCEEDED: You provided a string parameter that is longer than allowed.
--
--     * MAX_VALUE_EXCEEDED: You provided a numeric parameter that has a larger value than allowed.
--
--     * MIN_LENGTH_EXCEEDED: You provided a string parameter that is shorter than allowed.
--
--     * MIN_VALUE_EXCEEDED: You provided a numeric parameter that has a smaller value than allowed.
--
--     * MOVING_ACCOUNT_BETWEEN_DIFFERENT_ROOTS: You can move an account only between entities in the same root.
--
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError organizations
      "InvalidInputException"

-- | We can't find a source root or OU with the @ParentId@ that you specified.
--
--
_SourceParentNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SourceParentNotFoundException
  = _MatchServiceError organizations
      "SourceParentNotFoundException"

-- | We can't find a root, OU, or account with the @TargetId@ that you specified.
--
--
_TargetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetNotFoundException
  = _MatchServiceError organizations
      "TargetNotFoundException"

-- | We can't find an AWS account with the @AccountId@ that you specified, or the account whose credentials you used to make this request isn't a member of an organization.
--
--
_AccountNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountNotFoundException
  = _MatchServiceError organizations
      "AccountNotFoundException"

-- | You can't remove a master account from an organization. If you want the master account to become a member account in another organization, you must first delete the current organization of the master account.
--
--
_MasterCannotLeaveOrganizationException :: AsError a => Getting (First ServiceError) a ServiceError
_MasterCannotLeaveOrganizationException
  = _MatchServiceError organizations
      "MasterCannotLeaveOrganizationException"

-- | The target of the operation is currently being modified by a different request. Try again later.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException
  = _MatchServiceError organizations
      "ConcurrentModificationException"

-- | A handshake with the same action and target already exists. For example, if you invited an account to join your organization, the invited account might already have a pending invitation from this organization. If you intend to resend an invitation to an account, ensure that existing handshakes that might be considered duplicates are canceled or declined.
--
--
_DuplicateHandshakeException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateHandshakeException
  = _MatchServiceError organizations
      "DuplicateHandshakeException"

-- | The specified OU is not empty. Move all accounts to another root or to other OUs, remove all child OUs, and try the operation again.
--
--
_OrganizationalUnitNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationalUnitNotEmptyException
  = _MatchServiceError organizations
      "OrganizationalUnitNotEmptyException"

-- | The provided policy document doesn't meet the requirements of the specified policy type. For example, the syntax might be incorrect. For details about service control policy syntax, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./ 
--
--
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException
  = _MatchServiceError organizations
      "MalformedPolicyDocumentException"

-- | The specified policy type isn't currently enabled in this root. You can't attach policies of the specified type to entities in a root until you enable that type in the root. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide./ 
--
--
_PolicyTypeNotEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyTypeNotEnabledException
  = _MatchServiceError organizations
      "PolicyTypeNotEnabledException"

-- | The organization isn't empty. To delete an organization, you must first remove all accounts except the master account, delete all OUs, and delete all policies.
--
--
_OrganizationNotEmptyException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationNotEmptyException
  = _MatchServiceError organizations
      "OrganizationNotEmptyException"

-- | We can't find an OU with the @OrganizationalUnitId@ that you specified.
--
--
_OrganizationalUnitNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_OrganizationalUnitNotFoundException
  = _MatchServiceError organizations
      "OrganizationalUnitNotFoundException"

-- | The selected policy is already attached to the specified target.
--
--
_DuplicatePolicyAttachmentException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicatePolicyAttachmentException
  = _MatchServiceError organizations
      "DuplicatePolicyAttachmentException"

-- | This action isn't available in the current Region.
--
--
_UnsupportedAPIEndpointException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedAPIEndpointException
  = _MatchServiceError organizations
      "UnsupportedAPIEndpointException"

-- | The specified handshake is already in the requested state. For example, you can't accept a handshake that was already accepted.
--
--
_HandshakeAlreadyInStateException :: AsError a => Getting (First ServiceError) a ServiceError
_HandshakeAlreadyInStateException
  = _MatchServiceError organizations
      "HandshakeAlreadyInStateException"
