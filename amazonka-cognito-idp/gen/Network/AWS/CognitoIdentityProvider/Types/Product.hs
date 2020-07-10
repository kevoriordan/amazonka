{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.Product (
    module Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType,
    module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType,
    module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType,
    module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType,
    module Network.AWS.CognitoIdentityProvider.Types.AttributeType,
    module Network.AWS.CognitoIdentityProvider.Types.AuthEventType,
    module Network.AWS.CognitoIdentityProvider.Types.AuthenticationResultType,
    module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponseType,
    module Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType,
    module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsActionsType,
    module Network.AWS.CognitoIdentityProvider.Types.CompromisedCredentialsRiskConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.ContextDataType,
    module Network.AWS.CognitoIdentityProvider.Types.CustomDomainConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.DeviceType,
    module Network.AWS.CognitoIdentityProvider.Types.DomainDescriptionType,
    module Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.EventContextDataType,
    module Network.AWS.CognitoIdentityProvider.Types.EventFeedbackType,
    module Network.AWS.CognitoIdentityProvider.Types.EventRiskType,
    module Network.AWS.CognitoIdentityProvider.Types.GroupType,
    module Network.AWS.CognitoIdentityProvider.Types.HTTPHeader,
    module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderType,
    module Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.MFAOptionType,
    module Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType,
    module Network.AWS.CognitoIdentityProvider.Types.NewDeviceMetadataType,
    module Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType,
    module Network.AWS.CognitoIdentityProvider.Types.NumberAttributeConstraintsType,
    module Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType,
    module Network.AWS.CognitoIdentityProvider.Types.ProviderDescription,
    module Network.AWS.CognitoIdentityProvider.Types.ProviderUserIdentifierType,
    module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType,
    module Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType,
    module Network.AWS.CognitoIdentityProvider.Types.ResourceServerType,
    module Network.AWS.CognitoIdentityProvider.Types.RiskConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.RiskExceptionConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.SMSMFASettingsType,
    module Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType,
    module Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.SmsMFAConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFAConfigType,
    module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFASettingsType,
    module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType,
    module Network.AWS.CognitoIdentityProvider.Types.UICustomizationType,
    module Network.AWS.CognitoIdentityProvider.Types.UserContextDataType,
    module Network.AWS.CognitoIdentityProvider.Types.UserImportJobType,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType,
    module Network.AWS.CognitoIdentityProvider.Types.UserPoolType,
    module Network.AWS.CognitoIdentityProvider.Types.UserType,
    module Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType,
    module Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
  ) where

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
import Network.AWS.Lens
import Network.AWS.Prelude
