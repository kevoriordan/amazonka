{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types
    (
    -- * Service Configuration
      cognitoIdentityProvider

    -- * Errors
    , _InvalidOAuthFlowException
    , _InvalidSmsRoleAccessPolicyException
    , _InvalidUserPoolConfigurationException
    , _GroupExistsException
    , _ExpiredCodeException
    , _UnexpectedLambdaException
    , _UserNotFoundException
    , _TooManyFailedAttemptsException
    , _UserPoolAddOnNotEnabledException
    , _InvalidEmailRoleAccessPolicyException
    , _TooManyRequestsException
    , _InvalidParameterException
    , _UserPoolTaggingException
    , _InvalidSmsRoleTrustRelationshipException
    , _UnsupportedUserStateException
    , _UsernameExistsException
    , _ResourceNotFoundException
    , _InternalErrorException
    , _UserNotConfirmedException
    , _EnableSoftwareTokenMFAException
    , _UnsupportedIdentityProviderException
    , _NotAuthorizedException
    , _PreconditionNotMetException
    , _InvalidLambdaResponseException
    , _SoftwareTokenMFANotFoundException
    , _UserLambdaValidationException
    , _ConcurrentModificationException
    , _PasswordResetRequiredException
    , _LimitExceededException
    , _MFAMethodNotFoundException
    , _InvalidPasswordException
    , _DuplicateProviderException
    , _AliasExistsException
    , _UserImportInProgressException
    , _CodeMismatchException
    , _ScopeDoesNotExistException
    , _CodeDeliveryFailureException

    -- * AccountTakeoverEventActionType
    , AccountTakeoverEventActionType (..)

    -- * AdvancedSecurityModeType
    , AdvancedSecurityModeType (..)

    -- * AliasAttributeType
    , AliasAttributeType (..)

    -- * AttributeDataType
    , AttributeDataType (..)

    -- * AuthFlowType
    , AuthFlowType (..)

    -- * ChallengeName
    , ChallengeName (..)

    -- * ChallengeNameType
    , ChallengeNameType (..)

    -- * ChallengeResponse
    , ChallengeResponse (..)

    -- * CompromisedCredentialsEventActionType
    , CompromisedCredentialsEventActionType (..)

    -- * DefaultEmailOptionType
    , DefaultEmailOptionType (..)

    -- * DeliveryMediumType
    , DeliveryMediumType (..)

    -- * DeviceRememberedStatusType
    , DeviceRememberedStatusType (..)

    -- * DomainStatusType
    , DomainStatusType (..)

    -- * EmailSendingAccountType
    , EmailSendingAccountType (..)

    -- * EventFilterType
    , EventFilterType (..)

    -- * EventResponseType
    , EventResponseType (..)

    -- * EventType
    , EventType (..)

    -- * ExplicitAuthFlowsType
    , ExplicitAuthFlowsType (..)

    -- * FeedbackValueType
    , FeedbackValueType (..)

    -- * IdentityProviderTypeType
    , IdentityProviderTypeType (..)

    -- * MessageActionType
    , MessageActionType (..)

    -- * OAuthFlowType
    , OAuthFlowType (..)

    -- * PreventUserExistenceErrorTypes
    , PreventUserExistenceErrorTypes (..)

    -- * RecoveryOptionNameType
    , RecoveryOptionNameType (..)

    -- * RiskDecisionType
    , RiskDecisionType (..)

    -- * RiskLevelType
    , RiskLevelType (..)

    -- * StatusType
    , StatusType (..)

    -- * UserImportJobStatusType
    , UserImportJobStatusType (..)

    -- * UserPoolMFAType
    , UserPoolMFAType (..)

    -- * UserStatusType
    , UserStatusType (..)

    -- * UsernameAttributeType
    , UsernameAttributeType (..)

    -- * VerifiedAttributeType
    , VerifiedAttributeType (..)

    -- * VerifySoftwareTokenResponseType
    , VerifySoftwareTokenResponseType (..)

    -- * AccountRecoverySettingType
    , AccountRecoverySettingType
    , accountRecoverySettingType
    , arstRecoveryMechanisms

    -- * AccountTakeoverActionType
    , AccountTakeoverActionType
    , accountTakeoverActionType
    , atatNotify
    , atatEventAction

    -- * AccountTakeoverActionsType
    , AccountTakeoverActionsType
    , accountTakeoverActionsType
    , atatLowAction
    , atatHighAction
    , atatMediumAction

    -- * AccountTakeoverRiskConfigurationType
    , AccountTakeoverRiskConfigurationType
    , accountTakeoverRiskConfigurationType
    , atrctNotifyConfiguration
    , atrctActions

    -- * AdminCreateUserConfigType
    , AdminCreateUserConfigType
    , adminCreateUserConfigType
    , acuctAllowAdminCreateUserOnly
    , acuctUnusedAccountValidityDays
    , acuctInviteMessageTemplate

    -- * AnalyticsConfigurationType
    , AnalyticsConfigurationType
    , analyticsConfigurationType
    , actUserDataShared
    , actApplicationId
    , actRoleARN
    , actExternalId

    -- * AnalyticsMetadataType
    , AnalyticsMetadataType
    , analyticsMetadataType
    , amtAnalyticsEndpointId

    -- * AttributeType
    , AttributeType
    , attributeType
    , atValue
    , atName

    -- * AuthEventType
    , AuthEventType
    , authEventType
    , aetEventRisk
    , aetEventResponse
    , aetEventContextData
    , aetChallengeResponses
    , aetEventType
    , aetCreationDate
    , aetEventFeedback
    , aetEventId

    -- * AuthenticationResultType
    , AuthenticationResultType
    , authenticationResultType
    , artAccessToken
    , artRefreshToken
    , artNewDeviceMetadata
    , artExpiresIn
    , artTokenType
    , artIdToken

    -- * ChallengeResponseType
    , ChallengeResponseType
    , challengeResponseType
    , crtChallengeName
    , crtChallengeResponse

    -- * CodeDeliveryDetailsType
    , CodeDeliveryDetailsType
    , codeDeliveryDetailsType
    , cddtDestination
    , cddtDeliveryMedium
    , cddtAttributeName

    -- * CompromisedCredentialsActionsType
    , CompromisedCredentialsActionsType
    , compromisedCredentialsActionsType
    , ccatEventAction

    -- * CompromisedCredentialsRiskConfigurationType
    , CompromisedCredentialsRiskConfigurationType
    , compromisedCredentialsRiskConfigurationType
    , ccrctEventFilter
    , ccrctActions

    -- * ContextDataType
    , ContextDataType
    , contextDataType
    , cdtEncodedData
    , cdtIPAddress
    , cdtServerName
    , cdtServerPath
    , cdtHTTPHeaders

    -- * CustomDomainConfigType
    , CustomDomainConfigType
    , customDomainConfigType
    , cdctCertificateARN

    -- * DeviceConfigurationType
    , DeviceConfigurationType
    , deviceConfigurationType
    , dctChallengeRequiredOnNewDevice
    , dctDeviceOnlyRememberedOnUserPrompt

    -- * DeviceSecretVerifierConfigType
    , DeviceSecretVerifierConfigType
    , deviceSecretVerifierConfigType
    , dsvctPasswordVerifier
    , dsvctSalt

    -- * DeviceType
    , DeviceType
    , deviceType
    , dtDeviceLastModifiedDate
    , dtDeviceCreateDate
    , dtDeviceAttributes
    , dtDeviceKey
    , dtDeviceLastAuthenticatedDate

    -- * DomainDescriptionType
    , DomainDescriptionType
    , domainDescriptionType
    , ddtStatus
    , ddtCloudFrontDistribution
    , ddtUserPoolId
    , ddtDomain
    , ddtAWSAccountId
    , ddtCustomDomainConfig
    , ddtVersion
    , ddtS3Bucket

    -- * EmailConfigurationType
    , EmailConfigurationType
    , emailConfigurationType
    , ectSourceARN
    , ectFrom
    , ectConfigurationSet
    , ectReplyToEmailAddress
    , ectEmailSendingAccount

    -- * EventContextDataType
    , EventContextDataType
    , eventContextDataType
    , ecdtIPAddress
    , ecdtCountry
    , ecdtCity
    , ecdtDeviceName
    , ecdtTimezone

    -- * EventFeedbackType
    , EventFeedbackType
    , eventFeedbackType
    , eftFeedbackDate
    , eftFeedbackValue
    , eftProvider

    -- * EventRiskType
    , EventRiskType
    , eventRiskType
    , ertCompromisedCredentialsDetected
    , ertRiskLevel
    , ertRiskDecision

    -- * GroupType
    , GroupType
    , groupType
    , gtLastModifiedDate
    , gtUserPoolId
    , gtCreationDate
    , gtPrecedence
    , gtGroupName
    , gtDescription
    , gtRoleARN

    -- * HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphHeaderValue
    , httphHeaderName

    -- * IdentityProviderType
    , IdentityProviderType
    , identityProviderType
    , iptLastModifiedDate
    , iptUserPoolId
    , iptProviderType
    , iptCreationDate
    , iptIdpIdentifiers
    , iptAttributeMapping
    , iptProviderDetails
    , iptProviderName

    -- * LambdaConfigType
    , LambdaConfigType
    , lambdaConfigType
    , lctPreAuthentication
    , lctCreateAuthChallenge
    , lctVerifyAuthChallengeResponse
    , lctPostAuthentication
    , lctCustomMessage
    , lctDefineAuthChallenge
    , lctPostConfirmation
    , lctPreTokenGeneration
    , lctUserMigration
    , lctPreSignUp

    -- * MFAOptionType
    , MFAOptionType
    , mfaOptionType
    , motDeliveryMedium
    , motAttributeName

    -- * MessageTemplateType
    , MessageTemplateType
    , messageTemplateType
    , mttEmailSubject
    , mttSMSMessage
    , mttEmailMessage

    -- * NewDeviceMetadataType
    , NewDeviceMetadataType
    , newDeviceMetadataType
    , ndmtDeviceGroupKey
    , ndmtDeviceKey

    -- * NotifyConfigurationType
    , NotifyConfigurationType
    , notifyConfigurationType
    , nctNoActionEmail
    , nctFrom
    , nctReplyTo
    , nctBlockEmail
    , nctMFAEmail
    , nctSourceARN

    -- * NotifyEmailType
    , NotifyEmailType
    , notifyEmailType
    , netTextBody
    , netHTMLBody
    , netSubject

    -- * NumberAttributeConstraintsType
    , NumberAttributeConstraintsType
    , numberAttributeConstraintsType
    , nactMaxValue
    , nactMinValue

    -- * PasswordPolicyType
    , PasswordPolicyType
    , passwordPolicyType
    , pptRequireNumbers
    , pptRequireUppercase
    , pptRequireLowercase
    , pptMinimumLength
    , pptRequireSymbols
    , pptTemporaryPasswordValidityDays

    -- * ProviderDescription
    , ProviderDescription
    , providerDescription
    , pdLastModifiedDate
    , pdProviderType
    , pdCreationDate
    , pdProviderName

    -- * ProviderUserIdentifierType
    , ProviderUserIdentifierType
    , providerUserIdentifierType
    , puitProviderAttributeValue
    , puitProviderAttributeName
    , puitProviderName

    -- * RecoveryOptionType
    , RecoveryOptionType
    , recoveryOptionType
    , rotPriority
    , rotName

    -- * ResourceServerScopeType
    , ResourceServerScopeType
    , resourceServerScopeType
    , rsstScopeName
    , rsstScopeDescription

    -- * ResourceServerType
    , ResourceServerType
    , resourceServerType
    , rstUserPoolId
    , rstIdentifier
    , rstScopes
    , rstName

    -- * RiskConfigurationType
    , RiskConfigurationType
    , riskConfigurationType
    , rctRiskExceptionConfiguration
    , rctClientId
    , rctAccountTakeoverRiskConfiguration
    , rctLastModifiedDate
    , rctUserPoolId
    , rctCompromisedCredentialsRiskConfiguration

    -- * RiskExceptionConfigurationType
    , RiskExceptionConfigurationType
    , riskExceptionConfigurationType
    , rectSkippedIPRangeList
    , rectBlockedIPRangeList

    -- * SMSMFASettingsType
    , SMSMFASettingsType
    , sMSMFASettingsType
    , smsmstEnabled
    , smsmstPreferredMFA

    -- * SchemaAttributeType
    , SchemaAttributeType
    , schemaAttributeType
    , satNumberAttributeConstraints
    , satRequired
    , satAttributeDataType
    , satStringAttributeConstraints
    , satName
    , satDeveloperOnlyAttribute
    , satMutable

    -- * SmsConfigurationType
    , SmsConfigurationType
    , smsConfigurationType
    , sctExternalId
    , sctSNSCallerARN

    -- * SmsMFAConfigType
    , SmsMFAConfigType
    , smsMFAConfigType
    , smctSmsAuthenticationMessage
    , smctSmsConfiguration

    -- * SoftwareTokenMFAConfigType
    , SoftwareTokenMFAConfigType
    , softwareTokenMFAConfigType
    , stmctEnabled

    -- * SoftwareTokenMFASettingsType
    , SoftwareTokenMFASettingsType
    , softwareTokenMFASettingsType
    , stmstEnabled
    , stmstPreferredMFA

    -- * StringAttributeConstraintsType
    , StringAttributeConstraintsType
    , stringAttributeConstraintsType
    , sactMaxLength
    , sactMinLength

    -- * UICustomizationType
    , UICustomizationType
    , uICustomizationType
    , uictClientId
    , uictLastModifiedDate
    , uictUserPoolId
    , uictCSS
    , uictCSSVersion
    , uictImageURL
    , uictCreationDate

    -- * UserContextDataType
    , UserContextDataType
    , userContextDataType
    , ucdtEncodedData

    -- * UserImportJobType
    , UserImportJobType
    , userImportJobType
    , uijtStatus
    , uijtSkippedUsers
    , uijtJobId
    , uijtUserPoolId
    , uijtJobName
    , uijtPreSignedURL
    , uijtFailedUsers
    , uijtStartDate
    , uijtCompletionMessage
    , uijtCreationDate
    , uijtCompletionDate
    , uijtCloudWatchLogsRoleARN
    , uijtImportedUsers

    -- * UserPoolAddOnsType
    , UserPoolAddOnsType
    , userPoolAddOnsType
    , upaotAdvancedSecurityMode

    -- * UserPoolClientDescription
    , UserPoolClientDescription
    , userPoolClientDescription
    , upcdClientId
    , upcdUserPoolId
    , upcdClientName

    -- * UserPoolClientType
    , UserPoolClientType
    , userPoolClientType
    , upctRefreshTokenValidity
    , upctClientId
    , upctExplicitAuthFlows
    , upctClientSecret
    , upctLastModifiedDate
    , upctSupportedIdentityProviders
    , upctLogoutURLs
    , upctAllowedOAuthFlowsUserPoolClient
    , upctUserPoolId
    , upctDefaultRedirectURI
    , upctWriteAttributes
    , upctPreventUserExistenceErrors
    , upctCreationDate
    , upctReadAttributes
    , upctAllowedOAuthScopes
    , upctAllowedOAuthFlows
    , upctAnalyticsConfiguration
    , upctClientName
    , upctCallbackURLs

    -- * UserPoolDescriptionType
    , UserPoolDescriptionType
    , userPoolDescriptionType
    , updtStatus
    , updtLastModifiedDate
    , updtName
    , updtId
    , updtCreationDate
    , updtLambdaConfig

    -- * UserPoolPolicyType
    , UserPoolPolicyType
    , userPoolPolicyType
    , upptPasswordPolicy

    -- * UserPoolType
    , UserPoolType
    , userPoolType
    , uptStatus
    , uptUserPoolTags
    , uptEmailConfigurationFailure
    , uptLastModifiedDate
    , uptVerificationMessageTemplate
    , uptEstimatedNumberOfUsers
    , uptARN
    , uptDomain
    , uptCustomDomain
    , uptEmailVerificationMessage
    , uptSmsAuthenticationMessage
    , uptUserPoolAddOns
    , uptSchemaAttributes
    , uptEmailVerificationSubject
    , uptUsernameAttributes
    , uptAliasAttributes
    , uptAccountRecoverySetting
    , uptEmailConfiguration
    , uptSmsVerificationMessage
    , uptName
    , uptMFAConfiguration
    , uptId
    , uptSmsConfigurationFailure
    , uptCreationDate
    , uptLambdaConfig
    , uptSmsConfiguration
    , uptAdminCreateUserConfig
    , uptDeviceConfiguration
    , uptAutoVerifiedAttributes
    , uptPolicies
    , uptUsernameConfiguration

    -- * UserType
    , UserType
    , userType
    , utEnabled
    , utUserStatus
    , utUsername
    , utUserCreateDate
    , utAttributes
    , utMFAOptions
    , utUserLastModifiedDate

    -- * UsernameConfigurationType
    , UsernameConfigurationType
    , usernameConfigurationType
    , uctCaseSensitive

    -- * VerificationMessageTemplateType
    , VerificationMessageTemplateType
    , verificationMessageTemplateType
    , vmttDefaultEmailOption
    , vmttEmailSubject
    , vmttEmailSubjectByLink
    , vmttSmsMessage
    , vmttEmailMessageByLink
    , vmttEmailMessage
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
import Network.AWS.CognitoIdentityProvider.Types.AttributeDataType
import Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeName
import Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsEventActionType
import Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
import Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
import Network.AWS.CognitoIdentityProvider.Types.DeviceRememberedStatusType
import Network.AWS.CognitoIdentityProvider.Types.DomainStatusType
import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
import Network.AWS.CognitoIdentityProvider.Types.EventFilterType
import Network.AWS.CognitoIdentityProvider.Types.EventResponseType
import Network.AWS.CognitoIdentityProvider.Types.EventType
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Network.AWS.CognitoIdentityProvider.Types.FeedbackValueType
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
import Network.AWS.CognitoIdentityProvider.Types.MessageActionType
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
import Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
import Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
import Network.AWS.CognitoIdentityProvider.Types.StatusType
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
import Network.AWS.CognitoIdentityProvider.Types.VerifySoftwareTokenResponseType
import Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.AuthEventType
import Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType
import Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType
import Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType
import Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ContextDataType
import Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType
import Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
import Network.AWS.CognitoIdentityProvider.Types.DeviceType
import Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
import Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType
import Network.AWS.CognitoIdentityProvider.Types.EventRiskType
import Network.AWS.CognitoIdentityProvider.Types.GroupType
import Network.AWS.CognitoIdentityProvider.Types.HTTPHeader
import Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType
import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
import Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import Network.AWS.CognitoIdentityProvider.Types.ProviderDescription
import Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType
import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
import Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.SmsMFAConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType
import Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFASettingsType
import Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
import Network.AWS.CognitoIdentityProvider.Types.UICustomizationType
import Network.AWS.CognitoIdentityProvider.Types.UserContextDataType
import Network.AWS.CognitoIdentityProvider.Types.UserImportJobType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
import Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolType
import Network.AWS.CognitoIdentityProvider.Types.UserType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType

-- | API version @2016-04-18@ of the Amazon Cognito Identity Provider SDK configuration.
cognitoIdentityProvider :: Service
cognitoIdentityProvider
  = Service{_svcAbbrev = "CognitoIdentityProvider",
            _svcSigner = v4, _svcPrefix = "cognito-idp",
            _svcVersion = "2016-04-18",
            _svcEndpoint =
              defaultEndpoint cognitoIdentityProvider,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "CognitoIdentityProvider",
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

-- | This exception is thrown when the specified OAuth flow is invalid.
--
--
_InvalidOAuthFlowException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOAuthFlowException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidOAuthFlowException"

-- | This exception is returned when the role provided for SMS configuration does not have permission to publish using Amazon SNS.
--
--
_InvalidSmsRoleAccessPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSmsRoleAccessPolicyException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidSmsRoleAccessPolicyException"

-- | This exception is thrown when the user pool configuration is invalid.
--
--
_InvalidUserPoolConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUserPoolConfigurationException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidUserPoolConfigurationException"

-- | This exception is thrown when Amazon Cognito encounters a group that already exists in the user pool.
--
--
_GroupExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_GroupExistsException
  = _MatchServiceError cognitoIdentityProvider
      "GroupExistsException"

-- | This exception is thrown if a code has expired.
--
--
_ExpiredCodeException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredCodeException
  = _MatchServiceError cognitoIdentityProvider
      "ExpiredCodeException"

-- | This exception is thrown when the Amazon Cognito service encounters an unexpected exception with the AWS Lambda service.
--
--
_UnexpectedLambdaException :: AsError a => Getting (First ServiceError) a ServiceError
_UnexpectedLambdaException
  = _MatchServiceError cognitoIdentityProvider
      "UnexpectedLambdaException"

-- | This exception is thrown when a user is not found.
--
--
_UserNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_UserNotFoundException
  = _MatchServiceError cognitoIdentityProvider
      "UserNotFoundException"

-- | This exception is thrown when the user has made too many failed attempts for a given action (e.g., sign in).
--
--
_TooManyFailedAttemptsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyFailedAttemptsException
  = _MatchServiceError cognitoIdentityProvider
      "TooManyFailedAttemptsException"

-- | This exception is thrown when user pool add-ons are not enabled.
--
--
_UserPoolAddOnNotEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_UserPoolAddOnNotEnabledException
  = _MatchServiceError cognitoIdentityProvider
      "UserPoolAddOnNotEnabledException"

-- | This exception is thrown when Amazon Cognito is not allowed to use your email identity. HTTP status code: 400.
--
--
_InvalidEmailRoleAccessPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEmailRoleAccessPolicyException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidEmailRoleAccessPolicyException"

-- | This exception is thrown when the user has made too many requests for a given operation.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException
  = _MatchServiceError cognitoIdentityProvider
      "TooManyRequestsException"

-- | This exception is thrown when the Amazon Cognito service encounters an invalid parameter.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidParameterException"

-- | This exception is thrown when a user pool tag cannot be set or updated.
--
--
_UserPoolTaggingException :: AsError a => Getting (First ServiceError) a ServiceError
_UserPoolTaggingException
  = _MatchServiceError cognitoIdentityProvider
      "UserPoolTaggingException"

-- | This exception is thrown when the trust relationship is invalid for the role provided for SMS configuration. This can happen if you do not trust __cognito-idp.amazonaws.com__ or the external ID provided in the role does not match what is provided in the SMS configuration for the user pool.
--
--
_InvalidSmsRoleTrustRelationshipException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSmsRoleTrustRelationshipException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidSmsRoleTrustRelationshipException"

-- | The request failed because the user is in an unsupported state.
--
--
_UnsupportedUserStateException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedUserStateException
  = _MatchServiceError cognitoIdentityProvider
      "UnsupportedUserStateException"

-- | This exception is thrown when Amazon Cognito encounters a user name that already exists in the user pool.
--
--
_UsernameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_UsernameExistsException
  = _MatchServiceError cognitoIdentityProvider
      "UsernameExistsException"

-- | This exception is thrown when the Amazon Cognito service cannot find the requested resource.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError cognitoIdentityProvider
      "ResourceNotFoundException"

-- | This exception is thrown when Amazon Cognito encounters an internal error.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException
  = _MatchServiceError cognitoIdentityProvider
      "InternalErrorException"

-- | This exception is thrown when a user is not confirmed successfully.
--
--
_UserNotConfirmedException :: AsError a => Getting (First ServiceError) a ServiceError
_UserNotConfirmedException
  = _MatchServiceError cognitoIdentityProvider
      "UserNotConfirmedException"

-- | This exception is thrown when there is a code mismatch and the service fails to configure the software token TOTP multi-factor authentication (MFA).
--
--
_EnableSoftwareTokenMFAException :: AsError a => Getting (First ServiceError) a ServiceError
_EnableSoftwareTokenMFAException
  = _MatchServiceError cognitoIdentityProvider
      "EnableSoftwareTokenMFAException"

-- | This exception is thrown when the specified identifier is not supported.
--
--
_UnsupportedIdentityProviderException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedIdentityProviderException
  = _MatchServiceError cognitoIdentityProvider
      "UnsupportedIdentityProviderException"

-- | This exception is thrown when a user is not authorized.
--
--
_NotAuthorizedException :: AsError a => Getting (First ServiceError) a ServiceError
_NotAuthorizedException
  = _MatchServiceError cognitoIdentityProvider
      "NotAuthorizedException"

-- | This exception is thrown when a precondition is not met.
--
--
_PreconditionNotMetException :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionNotMetException
  = _MatchServiceError cognitoIdentityProvider
      "PreconditionNotMetException"

-- | This exception is thrown when the Amazon Cognito service encounters an invalid AWS Lambda response.
--
--
_InvalidLambdaResponseException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLambdaResponseException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidLambdaResponseException"

-- | This exception is thrown when the software token TOTP multi-factor authentication (MFA) is not enabled for the user pool.
--
--
_SoftwareTokenMFANotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SoftwareTokenMFANotFoundException
  = _MatchServiceError cognitoIdentityProvider
      "SoftwareTokenMFANotFoundException"

-- | This exception is thrown when the Amazon Cognito service encounters a user validation exception with the AWS Lambda service.
--
--
_UserLambdaValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_UserLambdaValidationException
  = _MatchServiceError cognitoIdentityProvider
      "UserLambdaValidationException"

-- | This exception is thrown if two or more modifications are happening concurrently.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException
  = _MatchServiceError cognitoIdentityProvider
      "ConcurrentModificationException"

-- | This exception is thrown when a password reset is required.
--
--
_PasswordResetRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_PasswordResetRequiredException
  = _MatchServiceError cognitoIdentityProvider
      "PasswordResetRequiredException"

-- | This exception is thrown when a user exceeds the limit for a requested AWS resource.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError cognitoIdentityProvider
      "LimitExceededException"

-- | This exception is thrown when Amazon Cognito cannot find a multi-factor authentication (MFA) method.
--
--
_MFAMethodNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_MFAMethodNotFoundException
  = _MatchServiceError cognitoIdentityProvider
      "MFAMethodNotFoundException"

-- | This exception is thrown when the Amazon Cognito service encounters an invalid password.
--
--
_InvalidPasswordException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPasswordException
  = _MatchServiceError cognitoIdentityProvider
      "InvalidPasswordException"

-- | This exception is thrown when the provider is already supported by the user pool.
--
--
_DuplicateProviderException :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateProviderException
  = _MatchServiceError cognitoIdentityProvider
      "DuplicateProviderException"

-- | This exception is thrown when a user tries to confirm the account with an email or phone number that has already been supplied as an alias from a different account. This exception tells user that an account with this email or phone already exists.
--
--
_AliasExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_AliasExistsException
  = _MatchServiceError cognitoIdentityProvider
      "AliasExistsException"

-- | This exception is thrown when you are trying to modify a user pool while a user import job is in progress for that pool.
--
--
_UserImportInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_UserImportInProgressException
  = _MatchServiceError cognitoIdentityProvider
      "UserImportInProgressException"

-- | This exception is thrown if the provided code does not match what the server was expecting.
--
--
_CodeMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeMismatchException
  = _MatchServiceError cognitoIdentityProvider
      "CodeMismatchException"

-- | This exception is thrown when the specified scope does not exist.
--
--
_ScopeDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ScopeDoesNotExistException
  = _MatchServiceError cognitoIdentityProvider
      "ScopeDoesNotExistException"

-- | This exception is thrown when a verification code fails to deliver successfully.
--
--
_CodeDeliveryFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeDeliveryFailureException
  = _MatchServiceError cognitoIdentityProvider
      "CodeDeliveryFailureException"
