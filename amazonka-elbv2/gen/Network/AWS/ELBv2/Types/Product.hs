{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.Product where

import Network.AWS.ELBv2.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an action.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aFixedResponseConfig       :: !(Maybe FixedResponseActionConfig)
  , _aTargetGroupARN            :: !(Maybe Text)
  , _aRedirectConfig            :: !(Maybe RedirectActionConfig)
  , _aAuthenticateCognitoConfig :: !(Maybe AuthenticateCognitoActionConfig)
  , _aOrder                     :: !(Maybe Nat)
  , _aAuthenticateOidcConfig    :: !(Maybe AuthenticateOidcActionConfig)
  , _aType                      :: !ActionTypeEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aFixedResponseConfig' - [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
--
-- * 'aTargetGroupARN' - The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ .
--
-- * 'aRedirectConfig' - [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
--
-- * 'aAuthenticateCognitoConfig' - [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
--
-- * 'aOrder' - The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first. The final action to be performed must be a @forward@ or a @fixed-response@ action.
--
-- * 'aAuthenticateOidcConfig' - [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
--
-- * 'aType' - The type of action. Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ .
action
    :: ActionTypeEnum -- ^ 'aType'
    -> Action
action pType_ =
  Action'
    { _aFixedResponseConfig = Nothing
    , _aTargetGroupARN = Nothing
    , _aRedirectConfig = Nothing
    , _aAuthenticateCognitoConfig = Nothing
    , _aOrder = Nothing
    , _aAuthenticateOidcConfig = Nothing
    , _aType = pType_
    }


-- | [Application Load Balancer] Information for creating an action that returns a custom HTTP response. Specify only when @Type@ is @fixed-response@ .
aFixedResponseConfig :: Lens' Action (Maybe FixedResponseActionConfig)
aFixedResponseConfig = lens _aFixedResponseConfig (\ s a -> s{_aFixedResponseConfig = a})

-- | The Amazon Resource Name (ARN) of the target group. Specify only when @Type@ is @forward@ .
aTargetGroupARN :: Lens' Action (Maybe Text)
aTargetGroupARN = lens _aTargetGroupARN (\ s a -> s{_aTargetGroupARN = a})

-- | [Application Load Balancer] Information for creating a redirect action. Specify only when @Type@ is @redirect@ .
aRedirectConfig :: Lens' Action (Maybe RedirectActionConfig)
aRedirectConfig = lens _aRedirectConfig (\ s a -> s{_aRedirectConfig = a})

-- | [HTTPS listeners] Information for using Amazon Cognito to authenticate users. Specify only when @Type@ is @authenticate-cognito@ .
aAuthenticateCognitoConfig :: Lens' Action (Maybe AuthenticateCognitoActionConfig)
aAuthenticateCognitoConfig = lens _aAuthenticateCognitoConfig (\ s a -> s{_aAuthenticateCognitoConfig = a})

-- | The order for the action. This value is required for rules with multiple actions. The action with the lowest value for order is performed first. The final action to be performed must be a @forward@ or a @fixed-response@ action.
aOrder :: Lens' Action (Maybe Natural)
aOrder = lens _aOrder (\ s a -> s{_aOrder = a}) . mapping _Nat

-- | [HTTPS listeners] Information about an identity provider that is compliant with OpenID Connect (OIDC). Specify only when @Type@ is @authenticate-oidc@ .
aAuthenticateOidcConfig :: Lens' Action (Maybe AuthenticateOidcActionConfig)
aAuthenticateOidcConfig = lens _aAuthenticateOidcConfig (\ s a -> s{_aAuthenticateOidcConfig = a})

-- | The type of action. Each rule must include exactly one of the following types of actions: @forward@ , @fixed-response@ , or @redirect@ .
aType :: Lens' Action ActionTypeEnum
aType = lens _aType (\ s a -> s{_aType = a})

instance FromXML Action where
        parseXML x
          = Action' <$>
              (x .@? "FixedResponseConfig") <*>
                (x .@? "TargetGroupArn")
                <*> (x .@? "RedirectConfig")
                <*> (x .@? "AuthenticateCognitoConfig")
                <*> (x .@? "Order")
                <*> (x .@? "AuthenticateOidcConfig")
                <*> (x .@ "Type")

instance Hashable Action where

instance NFData Action where

instance ToQuery Action where
        toQuery Action'{..}
          = mconcat
              ["FixedResponseConfig" =: _aFixedResponseConfig,
               "TargetGroupArn" =: _aTargetGroupARN,
               "RedirectConfig" =: _aRedirectConfig,
               "AuthenticateCognitoConfig" =:
                 _aAuthenticateCognitoConfig,
               "Order" =: _aOrder,
               "AuthenticateOidcConfig" =: _aAuthenticateOidcConfig,
               "Type" =: _aType]

-- | Request parameters to use when integrating with Amazon Cognito to authenticate users.
--
--
--
-- /See:/ 'authenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { _acacAuthenticationRequestExtraParams :: !(Maybe (Map Text Text))
  , _acacScope :: !(Maybe Text)
  , _acacOnUnauthenticatedRequest :: !(Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
  , _acacSessionCookieName :: !(Maybe Text)
  , _acacSessionTimeout :: !(Maybe Integer)
  , _acacUserPoolARN :: !Text
  , _acacUserPoolClientId :: !Text
  , _acacUserPoolDomain :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthenticateCognitoActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acacAuthenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- * 'acacScope' - The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- * 'acacOnUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
-- * 'acacSessionCookieName' - The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- * 'acacSessionTimeout' - The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- * 'acacUserPoolARN' - The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
--
-- * 'acacUserPoolClientId' - The ID of the Amazon Cognito user pool client.
--
-- * 'acacUserPoolDomain' - The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
authenticateCognitoActionConfig
    :: Text -- ^ 'acacUserPoolARN'
    -> Text -- ^ 'acacUserPoolClientId'
    -> Text -- ^ 'acacUserPoolDomain'
    -> AuthenticateCognitoActionConfig
authenticateCognitoActionConfig pUserPoolARN_ pUserPoolClientId_ pUserPoolDomain_ =
  AuthenticateCognitoActionConfig'
    { _acacAuthenticationRequestExtraParams = Nothing
    , _acacScope = Nothing
    , _acacOnUnauthenticatedRequest = Nothing
    , _acacSessionCookieName = Nothing
    , _acacSessionTimeout = Nothing
    , _acacUserPoolARN = pUserPoolARN_
    , _acacUserPoolClientId = pUserPoolClientId_
    , _acacUserPoolDomain = pUserPoolDomain_
    }


-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
acacAuthenticationRequestExtraParams :: Lens' AuthenticateCognitoActionConfig (HashMap Text Text)
acacAuthenticationRequestExtraParams = lens _acacAuthenticationRequestExtraParams (\ s a -> s{_acacAuthenticationRequestExtraParams = a}) . _Default . _Map

-- | The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
acacScope :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacScope = lens _acacScope (\ s a -> s{_acacScope = a})

-- | The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
acacOnUnauthenticatedRequest :: Lens' AuthenticateCognitoActionConfig (Maybe AuthenticateCognitoActionConditionalBehaviorEnum)
acacOnUnauthenticatedRequest = lens _acacOnUnauthenticatedRequest (\ s a -> s{_acacOnUnauthenticatedRequest = a})

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
acacSessionCookieName :: Lens' AuthenticateCognitoActionConfig (Maybe Text)
acacSessionCookieName = lens _acacSessionCookieName (\ s a -> s{_acacSessionCookieName = a})

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
acacSessionTimeout :: Lens' AuthenticateCognitoActionConfig (Maybe Integer)
acacSessionTimeout = lens _acacSessionTimeout (\ s a -> s{_acacSessionTimeout = a})

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
acacUserPoolARN :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolARN = lens _acacUserPoolARN (\ s a -> s{_acacUserPoolARN = a})

-- | The ID of the Amazon Cognito user pool client.
acacUserPoolClientId :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolClientId = lens _acacUserPoolClientId (\ s a -> s{_acacUserPoolClientId = a})

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
acacUserPoolDomain :: Lens' AuthenticateCognitoActionConfig Text
acacUserPoolDomain = lens _acacUserPoolDomain (\ s a -> s{_acacUserPoolDomain = a})

instance FromXML AuthenticateCognitoActionConfig
         where
        parseXML x
          = AuthenticateCognitoActionConfig' <$>
              (x .@? "AuthenticationRequestExtraParams" .!@ mempty
                 >>= may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "Scope")
                <*> (x .@? "OnUnauthenticatedRequest")
                <*> (x .@? "SessionCookieName")
                <*> (x .@? "SessionTimeout")
                <*> (x .@ "UserPoolArn")
                <*> (x .@ "UserPoolClientId")
                <*> (x .@ "UserPoolDomain")

instance Hashable AuthenticateCognitoActionConfig
         where

instance NFData AuthenticateCognitoActionConfig where

instance ToQuery AuthenticateCognitoActionConfig
         where
        toQuery AuthenticateCognitoActionConfig'{..}
          = mconcat
              ["AuthenticationRequestExtraParams" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _acacAuthenticationRequestExtraParams),
               "Scope" =: _acacScope,
               "OnUnauthenticatedRequest" =:
                 _acacOnUnauthenticatedRequest,
               "SessionCookieName" =: _acacSessionCookieName,
               "SessionTimeout" =: _acacSessionTimeout,
               "UserPoolArn" =: _acacUserPoolARN,
               "UserPoolClientId" =: _acacUserPoolClientId,
               "UserPoolDomain" =: _acacUserPoolDomain]

-- | Request parameters when using an identity provider (IdP) that is compliant with OpenID Connect (OIDC) to authenticate users.
--
--
--
-- /See:/ 'authenticateOidcActionConfig' smart constructor.
data AuthenticateOidcActionConfig = AuthenticateOidcActionConfig'
  { _aoacAuthenticationRequestExtraParams :: !(Maybe (Map Text Text))
  , _aoacScope :: !(Maybe Text)
  , _aoacOnUnauthenticatedRequest :: !(Maybe AuthenticateOidcActionConditionalBehaviorEnum)
  , _aoacSessionCookieName :: !(Maybe Text)
  , _aoacSessionTimeout :: !(Maybe Integer)
  , _aoacIssuer :: !Text
  , _aoacAuthorizationEndpoint :: !Text
  , _aoacTokenEndpoint :: !Text
  , _aoacUserInfoEndpoint :: !Text
  , _aoacClientId :: !Text
  , _aoacClientSecret :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AuthenticateOidcActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aoacAuthenticationRequestExtraParams' - The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- * 'aoacScope' - The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- * 'aoacOnUnauthenticatedRequest' - The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
-- * 'aoacSessionCookieName' - The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- * 'aoacSessionTimeout' - The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- * 'aoacIssuer' - The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacAuthorizationEndpoint' - The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacTokenEndpoint' - The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacUserInfoEndpoint' - The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
--
-- * 'aoacClientId' - The OAuth 2.0 client identifier.
--
-- * 'aoacClientSecret' - The OAuth 2.0 client secret.
authenticateOidcActionConfig
    :: Text -- ^ 'aoacIssuer'
    -> Text -- ^ 'aoacAuthorizationEndpoint'
    -> Text -- ^ 'aoacTokenEndpoint'
    -> Text -- ^ 'aoacUserInfoEndpoint'
    -> Text -- ^ 'aoacClientId'
    -> Text -- ^ 'aoacClientSecret'
    -> AuthenticateOidcActionConfig
authenticateOidcActionConfig pIssuer_ pAuthorizationEndpoint_ pTokenEndpoint_ pUserInfoEndpoint_ pClientId_ pClientSecret_ =
  AuthenticateOidcActionConfig'
    { _aoacAuthenticationRequestExtraParams = Nothing
    , _aoacScope = Nothing
    , _aoacOnUnauthenticatedRequest = Nothing
    , _aoacSessionCookieName = Nothing
    , _aoacSessionTimeout = Nothing
    , _aoacIssuer = pIssuer_
    , _aoacAuthorizationEndpoint = pAuthorizationEndpoint_
    , _aoacTokenEndpoint = pTokenEndpoint_
    , _aoacUserInfoEndpoint = pUserInfoEndpoint_
    , _aoacClientId = pClientId_
    , _aoacClientSecret = pClientSecret_
    }


-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
aoacAuthenticationRequestExtraParams :: Lens' AuthenticateOidcActionConfig (HashMap Text Text)
aoacAuthenticationRequestExtraParams = lens _aoacAuthenticationRequestExtraParams (\ s a -> s{_aoacAuthenticationRequestExtraParams = a}) . _Default . _Map

-- | The set of user claims to be requested from the IdP. The default is @openid@ . To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
aoacScope :: Lens' AuthenticateOidcActionConfig (Maybe Text)
aoacScope = lens _aoacScope (\ s a -> s{_aoacScope = a})

-- | The behavior if the user is not authenticated. The following are possible values:     * deny- Return an HTTP 401 Unauthorized error.     * allow- Allow the request to be forwarded to the target.     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
aoacOnUnauthenticatedRequest :: Lens' AuthenticateOidcActionConfig (Maybe AuthenticateOidcActionConditionalBehaviorEnum)
aoacOnUnauthenticatedRequest = lens _aoacOnUnauthenticatedRequest (\ s a -> s{_aoacOnUnauthenticatedRequest = a})

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
aoacSessionCookieName :: Lens' AuthenticateOidcActionConfig (Maybe Text)
aoacSessionCookieName = lens _aoacSessionCookieName (\ s a -> s{_aoacSessionCookieName = a})

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
aoacSessionTimeout :: Lens' AuthenticateOidcActionConfig (Maybe Integer)
aoacSessionTimeout = lens _aoacSessionTimeout (\ s a -> s{_aoacSessionTimeout = a})

-- | The OIDC issuer identifier of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacIssuer :: Lens' AuthenticateOidcActionConfig Text
aoacIssuer = lens _aoacIssuer (\ s a -> s{_aoacIssuer = a})

-- | The authorization endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacAuthorizationEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacAuthorizationEndpoint = lens _aoacAuthorizationEndpoint (\ s a -> s{_aoacAuthorizationEndpoint = a})

-- | The token endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacTokenEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacTokenEndpoint = lens _aoacTokenEndpoint (\ s a -> s{_aoacTokenEndpoint = a})

-- | The user info endpoint of the IdP. This must be a full URL, including the HTTPS protocol, the domain, and the path.
aoacUserInfoEndpoint :: Lens' AuthenticateOidcActionConfig Text
aoacUserInfoEndpoint = lens _aoacUserInfoEndpoint (\ s a -> s{_aoacUserInfoEndpoint = a})

-- | The OAuth 2.0 client identifier.
aoacClientId :: Lens' AuthenticateOidcActionConfig Text
aoacClientId = lens _aoacClientId (\ s a -> s{_aoacClientId = a})

-- | The OAuth 2.0 client secret.
aoacClientSecret :: Lens' AuthenticateOidcActionConfig Text
aoacClientSecret = lens _aoacClientSecret (\ s a -> s{_aoacClientSecret = a})

instance FromXML AuthenticateOidcActionConfig where
        parseXML x
          = AuthenticateOidcActionConfig' <$>
              (x .@? "AuthenticationRequestExtraParams" .!@ mempty
                 >>= may (parseXMLMap "entry" "key" "value"))
                <*> (x .@? "Scope")
                <*> (x .@? "OnUnauthenticatedRequest")
                <*> (x .@? "SessionCookieName")
                <*> (x .@? "SessionTimeout")
                <*> (x .@ "Issuer")
                <*> (x .@ "AuthorizationEndpoint")
                <*> (x .@ "TokenEndpoint")
                <*> (x .@ "UserInfoEndpoint")
                <*> (x .@ "ClientId")
                <*> (x .@ "ClientSecret")

instance Hashable AuthenticateOidcActionConfig where

instance NFData AuthenticateOidcActionConfig where

instance ToQuery AuthenticateOidcActionConfig where
        toQuery AuthenticateOidcActionConfig'{..}
          = mconcat
              ["AuthenticationRequestExtraParams" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _aoacAuthenticationRequestExtraParams),
               "Scope" =: _aoacScope,
               "OnUnauthenticatedRequest" =:
                 _aoacOnUnauthenticatedRequest,
               "SessionCookieName" =: _aoacSessionCookieName,
               "SessionTimeout" =: _aoacSessionTimeout,
               "Issuer" =: _aoacIssuer,
               "AuthorizationEndpoint" =:
                 _aoacAuthorizationEndpoint,
               "TokenEndpoint" =: _aoacTokenEndpoint,
               "UserInfoEndpoint" =: _aoacUserInfoEndpoint,
               "ClientId" =: _aoacClientId,
               "ClientSecret" =: _aoacClientSecret]

-- | Information about an Availability Zone.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { _azSubnetId              :: !(Maybe Text)
  , _azZoneName              :: !(Maybe Text)
  , _azLoadBalancerAddresses :: !(Maybe [LoadBalancerAddress])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azSubnetId' - The ID of the subnet.
--
-- * 'azZoneName' - The name of the Availability Zone.
--
-- * 'azLoadBalancerAddresses' - [Network Load Balancers] The static IP address.
availabilityZone
    :: AvailabilityZone
availabilityZone =
  AvailabilityZone'
    { _azSubnetId = Nothing
    , _azZoneName = Nothing
    , _azLoadBalancerAddresses = Nothing
    }


-- | The ID of the subnet.
azSubnetId :: Lens' AvailabilityZone (Maybe Text)
azSubnetId = lens _azSubnetId (\ s a -> s{_azSubnetId = a})

-- | The name of the Availability Zone.
azZoneName :: Lens' AvailabilityZone (Maybe Text)
azZoneName = lens _azZoneName (\ s a -> s{_azZoneName = a})

-- | [Network Load Balancers] The static IP address.
azLoadBalancerAddresses :: Lens' AvailabilityZone [LoadBalancerAddress]
azLoadBalancerAddresses = lens _azLoadBalancerAddresses (\ s a -> s{_azLoadBalancerAddresses = a}) . _Default . _Coerce

instance FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' <$>
              (x .@? "SubnetId") <*> (x .@? "ZoneName") <*>
                (x .@? "LoadBalancerAddresses" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Information about an SSL server certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cCertificateARN :: !(Maybe Text)
  , _cIsDefault      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCertificateARN' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cIsDefault' - Indicates whether the certificate is the default certificate. Do not set @IsDefault@ when specifying a certificate as an input parameter.
certificate
    :: Certificate
certificate = Certificate' {_cCertificateARN = Nothing, _cIsDefault = Nothing}


-- | The Amazon Resource Name (ARN) of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\ s a -> s{_cCertificateARN = a})

-- | Indicates whether the certificate is the default certificate. Do not set @IsDefault@ when specifying a certificate as an input parameter.
cIsDefault :: Lens' Certificate (Maybe Bool)
cIsDefault = lens _cIsDefault (\ s a -> s{_cIsDefault = a})

instance FromXML Certificate where
        parseXML x
          = Certificate' <$>
              (x .@? "CertificateArn") <*> (x .@? "IsDefault")

instance Hashable Certificate where

instance NFData Certificate where

instance ToQuery Certificate where
        toQuery Certificate'{..}
          = mconcat
              ["CertificateArn" =: _cCertificateARN,
               "IsDefault" =: _cIsDefault]

-- | Information about a cipher used in a policy.
--
--
--
-- /See:/ 'cipher' smart constructor.
data Cipher = Cipher'
  { _cPriority :: !(Maybe Int)
  , _cName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cipher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cPriority' - The priority of the cipher.
--
-- * 'cName' - The name of the cipher.
cipher
    :: Cipher
cipher = Cipher' {_cPriority = Nothing, _cName = Nothing}


-- | The priority of the cipher.
cPriority :: Lens' Cipher (Maybe Int)
cPriority = lens _cPriority (\ s a -> s{_cPriority = a})

-- | The name of the cipher.
cName :: Lens' Cipher (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

instance FromXML Cipher where
        parseXML x
          = Cipher' <$> (x .@? "Priority") <*> (x .@? "Name")

instance Hashable Cipher where

instance NFData Cipher where

-- | Information about an action that returns a custom HTTP response.
--
--
--
-- /See:/ 'fixedResponseActionConfig' smart constructor.
data FixedResponseActionConfig = FixedResponseActionConfig'
  { _fracMessageBody :: !(Maybe Text)
  , _fracContentType :: !(Maybe Text)
  , _fracStatusCode  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FixedResponseActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fracMessageBody' - The message.
--
-- * 'fracContentType' - The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
--
-- * 'fracStatusCode' - The HTTP response code (2XX, 4XX, or 5XX).
fixedResponseActionConfig
    :: Text -- ^ 'fracStatusCode'
    -> FixedResponseActionConfig
fixedResponseActionConfig pStatusCode_ =
  FixedResponseActionConfig'
    { _fracMessageBody = Nothing
    , _fracContentType = Nothing
    , _fracStatusCode = pStatusCode_
    }


-- | The message.
fracMessageBody :: Lens' FixedResponseActionConfig (Maybe Text)
fracMessageBody = lens _fracMessageBody (\ s a -> s{_fracMessageBody = a})

-- | The content type. Valid Values: text/plain | text/css | text/html | application/javascript | application/json
fracContentType :: Lens' FixedResponseActionConfig (Maybe Text)
fracContentType = lens _fracContentType (\ s a -> s{_fracContentType = a})

-- | The HTTP response code (2XX, 4XX, or 5XX).
fracStatusCode :: Lens' FixedResponseActionConfig Text
fracStatusCode = lens _fracStatusCode (\ s a -> s{_fracStatusCode = a})

instance FromXML FixedResponseActionConfig where
        parseXML x
          = FixedResponseActionConfig' <$>
              (x .@? "MessageBody") <*> (x .@? "ContentType") <*>
                (x .@ "StatusCode")

instance Hashable FixedResponseActionConfig where

instance NFData FixedResponseActionConfig where

instance ToQuery FixedResponseActionConfig where
        toQuery FixedResponseActionConfig'{..}
          = mconcat
              ["MessageBody" =: _fracMessageBody,
               "ContentType" =: _fracContentType,
               "StatusCode" =: _fracStatusCode]

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
--
--
-- /See:/ 'limit' smart constructor.
data Limit = Limit'
  { _lMax  :: !(Maybe Text)
  , _lName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Limit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lMax' - The maximum value of the limit.
--
-- * 'lName' - The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
limit
    :: Limit
limit = Limit' {_lMax = Nothing, _lName = Nothing}


-- | The maximum value of the limit.
lMax :: Lens' Limit (Maybe Text)
lMax = lens _lMax (\ s a -> s{_lMax = a})

-- | The name of the limit. The possible values are:     * application-load-balancers     * listeners-per-application-load-balancer     * listeners-per-network-load-balancer     * network-load-balancers     * rules-per-application-load-balancer     * target-groups     * targets-per-application-load-balancer     * targets-per-availability-zone-per-network-load-balancer     * targets-per-network-load-balancer
lName :: Lens' Limit (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a})

instance FromXML Limit where
        parseXML x
          = Limit' <$> (x .@? "Max") <*> (x .@? "Name")

instance Hashable Limit where

instance NFData Limit where

-- | Information about a listener.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lSSLPolicy       :: !(Maybe Text)
  , _lListenerARN     :: !(Maybe Text)
  , _lProtocol        :: !(Maybe ProtocolEnum)
  , _lDefaultActions  :: !(Maybe [Action])
  , _lCertificates    :: !(Maybe [Certificate])
  , _lLoadBalancerARN :: !(Maybe Text)
  , _lPort            :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSSLPolicy' - The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
--
-- * 'lListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'lProtocol' - The protocol for connections from clients to the load balancer.
--
-- * 'lDefaultActions' - The default actions for the listener.
--
-- * 'lCertificates' - The SSL server certificate. You must provide a certificate if the protocol is HTTPS or TLS.
--
-- * 'lLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lPort' - The port on which the load balancer is listening.
listener
    :: Listener
listener =
  Listener'
    { _lSSLPolicy = Nothing
    , _lListenerARN = Nothing
    , _lProtocol = Nothing
    , _lDefaultActions = Nothing
    , _lCertificates = Nothing
    , _lLoadBalancerARN = Nothing
    , _lPort = Nothing
    }


-- | The security policy that defines which ciphers and protocols are supported. The default is the current predefined security policy.
lSSLPolicy :: Lens' Listener (Maybe Text)
lSSLPolicy = lens _lSSLPolicy (\ s a -> s{_lSSLPolicy = a})

-- | The Amazon Resource Name (ARN) of the listener.
lListenerARN :: Lens' Listener (Maybe Text)
lListenerARN = lens _lListenerARN (\ s a -> s{_lListenerARN = a})

-- | The protocol for connections from clients to the load balancer.
lProtocol :: Lens' Listener (Maybe ProtocolEnum)
lProtocol = lens _lProtocol (\ s a -> s{_lProtocol = a})

-- | The default actions for the listener.
lDefaultActions :: Lens' Listener [Action]
lDefaultActions = lens _lDefaultActions (\ s a -> s{_lDefaultActions = a}) . _Default . _Coerce

-- | The SSL server certificate. You must provide a certificate if the protocol is HTTPS or TLS.
lCertificates :: Lens' Listener [Certificate]
lCertificates = lens _lCertificates (\ s a -> s{_lCertificates = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lLoadBalancerARN :: Lens' Listener (Maybe Text)
lLoadBalancerARN = lens _lLoadBalancerARN (\ s a -> s{_lLoadBalancerARN = a})

-- | The port on which the load balancer is listening.
lPort :: Lens' Listener (Maybe Natural)
lPort = lens _lPort (\ s a -> s{_lPort = a}) . mapping _Nat

instance FromXML Listener where
        parseXML x
          = Listener' <$>
              (x .@? "SslPolicy") <*> (x .@? "ListenerArn") <*>
                (x .@? "Protocol")
                <*>
                (x .@? "DefaultActions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Certificates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "Port")

instance Hashable Listener where

instance NFData Listener where

-- | Information about a load balancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { _lbState                 :: !(Maybe LoadBalancerState)
  , _lbSecurityGroups        :: !(Maybe [Text])
  , _lbLoadBalancerName      :: !(Maybe Text)
  , _lbCreatedTime           :: !(Maybe ISO8601)
  , _lbVPCId                 :: !(Maybe Text)
  , _lbCanonicalHostedZoneId :: !(Maybe Text)
  , _lbAvailabilityZones     :: !(Maybe [AvailabilityZone])
  , _lbLoadBalancerARN       :: !(Maybe Text)
  , _lbIPAddressType         :: !(Maybe IPAddressType)
  , _lbScheme                :: !(Maybe LoadBalancerSchemeEnum)
  , _lbType                  :: !(Maybe LoadBalancerTypeEnum)
  , _lbDNSName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbState' - The state of the load balancer.
--
-- * 'lbSecurityGroups' - The IDs of the security groups for the load balancer.
--
-- * 'lbLoadBalancerName' - The name of the load balancer.
--
-- * 'lbCreatedTime' - The date and time the load balancer was created.
--
-- * 'lbVPCId' - The ID of the VPC for the load balancer.
--
-- * 'lbCanonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load balancer.
--
-- * 'lbAvailabilityZones' - The Availability Zones for the load balancer.
--
-- * 'lbLoadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
--
-- * 'lbIPAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
--
-- * 'lbScheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
--
-- * 'lbType' - The type of load balancer.
--
-- * 'lbDNSName' - The public DNS name of the load balancer.
loadBalancer
    :: LoadBalancer
loadBalancer =
  LoadBalancer'
    { _lbState = Nothing
    , _lbSecurityGroups = Nothing
    , _lbLoadBalancerName = Nothing
    , _lbCreatedTime = Nothing
    , _lbVPCId = Nothing
    , _lbCanonicalHostedZoneId = Nothing
    , _lbAvailabilityZones = Nothing
    , _lbLoadBalancerARN = Nothing
    , _lbIPAddressType = Nothing
    , _lbScheme = Nothing
    , _lbType = Nothing
    , _lbDNSName = Nothing
    }


-- | The state of the load balancer.
lbState :: Lens' LoadBalancer (Maybe LoadBalancerState)
lbState = lens _lbState (\ s a -> s{_lbState = a})

-- | The IDs of the security groups for the load balancer.
lbSecurityGroups :: Lens' LoadBalancer [Text]
lbSecurityGroups = lens _lbSecurityGroups (\ s a -> s{_lbSecurityGroups = a}) . _Default . _Coerce

-- | The name of the load balancer.
lbLoadBalancerName :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerName = lens _lbLoadBalancerName (\ s a -> s{_lbLoadBalancerName = a})

-- | The date and time the load balancer was created.
lbCreatedTime :: Lens' LoadBalancer (Maybe UTCTime)
lbCreatedTime = lens _lbCreatedTime (\ s a -> s{_lbCreatedTime = a}) . mapping _Time

-- | The ID of the VPC for the load balancer.
lbVPCId :: Lens' LoadBalancer (Maybe Text)
lbVPCId = lens _lbVPCId (\ s a -> s{_lbVPCId = a})

-- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
lbCanonicalHostedZoneId :: Lens' LoadBalancer (Maybe Text)
lbCanonicalHostedZoneId = lens _lbCanonicalHostedZoneId (\ s a -> s{_lbCanonicalHostedZoneId = a})

-- | The Availability Zones for the load balancer.
lbAvailabilityZones :: Lens' LoadBalancer [AvailabilityZone]
lbAvailabilityZones = lens _lbAvailabilityZones (\ s a -> s{_lbAvailabilityZones = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
lbLoadBalancerARN :: Lens' LoadBalancer (Maybe Text)
lbLoadBalancerARN = lens _lbLoadBalancerARN (\ s a -> s{_lbLoadBalancerARN = a})

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
lbIPAddressType :: Lens' LoadBalancer (Maybe IPAddressType)
lbIPAddressType = lens _lbIPAddressType (\ s a -> s{_lbIPAddressType = a})

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet. The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can only route requests from clients with access to the VPC for the load balancer.
lbScheme :: Lens' LoadBalancer (Maybe LoadBalancerSchemeEnum)
lbScheme = lens _lbScheme (\ s a -> s{_lbScheme = a})

-- | The type of load balancer.
lbType :: Lens' LoadBalancer (Maybe LoadBalancerTypeEnum)
lbType = lens _lbType (\ s a -> s{_lbType = a})

-- | The public DNS name of the load balancer.
lbDNSName :: Lens' LoadBalancer (Maybe Text)
lbDNSName = lens _lbDNSName (\ s a -> s{_lbDNSName = a})

instance FromXML LoadBalancer where
        parseXML x
          = LoadBalancer' <$>
              (x .@? "State") <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerName")
                <*> (x .@? "CreatedTime")
                <*> (x .@? "VpcId")
                <*> (x .@? "CanonicalHostedZoneId")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "LoadBalancerArn")
                <*> (x .@? "IpAddressType")
                <*> (x .@? "Scheme")
                <*> (x .@? "Type")
                <*> (x .@? "DNSName")

instance Hashable LoadBalancer where

instance NFData LoadBalancer where

-- | Information about a static IP address for a load balancer.
--
--
--
-- /See:/ 'loadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { _lbaIPAddress    :: !(Maybe Text)
  , _lbaAllocationId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaIPAddress' - The static IP address.
--
-- * 'lbaAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address.
loadBalancerAddress
    :: LoadBalancerAddress
loadBalancerAddress =
  LoadBalancerAddress' {_lbaIPAddress = Nothing, _lbaAllocationId = Nothing}


-- | The static IP address.
lbaIPAddress :: Lens' LoadBalancerAddress (Maybe Text)
lbaIPAddress = lens _lbaIPAddress (\ s a -> s{_lbaIPAddress = a})

-- | [Network Load Balancers] The allocation ID of the Elastic IP address.
lbaAllocationId :: Lens' LoadBalancerAddress (Maybe Text)
lbaAllocationId = lens _lbaAllocationId (\ s a -> s{_lbaAllocationId = a})

instance FromXML LoadBalancerAddress where
        parseXML x
          = LoadBalancerAddress' <$>
              (x .@? "IpAddress") <*> (x .@? "AllocationId")

instance Hashable LoadBalancerAddress where

instance NFData LoadBalancerAddress where

-- | Information about a load balancer attribute.
--
--
--
-- /See:/ 'loadBalancerAttribute' smart constructor.
data LoadBalancerAttribute = LoadBalancerAttribute'
  { _lbaValue :: !(Maybe Text)
  , _lbaKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbaValue' - The value of the attribute.
--
-- * 'lbaKey' - The name of the attribute. The following attributes are supported by both Application Load Balancers and Network Load Balancers:     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ . The following attributes are supported by only Application Load Balancers:     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . The following attributes are supported by only Network Load Balancers:     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
loadBalancerAttribute
    :: LoadBalancerAttribute
loadBalancerAttribute =
  LoadBalancerAttribute' {_lbaValue = Nothing, _lbaKey = Nothing}


-- | The value of the attribute.
lbaValue :: Lens' LoadBalancerAttribute (Maybe Text)
lbaValue = lens _lbaValue (\ s a -> s{_lbaValue = a})

-- | The name of the attribute. The following attributes are supported by both Application Load Balancers and Network Load Balancers:     * @deletion_protection.enabled@ - Indicates whether deletion protection is enabled. The value is @true@ or @false@ . The default is @false@ . The following attributes are supported by only Application Load Balancers:     * @access_logs.s3.enabled@ - Indicates whether access logs are enabled. The value is @true@ or @false@ . The default is @false@ .     * @access_logs.s3.bucket@ - The name of the S3 bucket for the access logs. This attribute is required if access logs are enabled. The bucket must exist in the same region as the load balancer and have a bucket policy that grants Elastic Load Balancing permissions to write to the bucket.     * @access_logs.s3.prefix@ - The prefix for the location in the S3 bucket for the access logs.     * @idle_timeout.timeout_seconds@ - The idle timeout value, in seconds. The valid range is 1-4000 seconds. The default is 60 seconds.     * @routing.http2.enabled@ - Indicates whether HTTP/2 is enabled. The value is @true@ or @false@ . The default is @true@ . The following attributes are supported by only Network Load Balancers:     * @load_balancing.cross_zone.enabled@ - Indicates whether cross-zone load balancing is enabled. The value is @true@ or @false@ . The default is @false@ .
lbaKey :: Lens' LoadBalancerAttribute (Maybe Text)
lbaKey = lens _lbaKey (\ s a -> s{_lbaKey = a})

instance FromXML LoadBalancerAttribute where
        parseXML x
          = LoadBalancerAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable LoadBalancerAttribute where

instance NFData LoadBalancerAttribute where

instance ToQuery LoadBalancerAttribute where
        toQuery LoadBalancerAttribute'{..}
          = mconcat ["Value" =: _lbaValue, "Key" =: _lbaKey]

-- | Information about the state of the load balancer.
--
--
--
-- /See:/ 'loadBalancerState' smart constructor.
data LoadBalancerState = LoadBalancerState'
  { _lbsReason :: !(Maybe Text)
  , _lbsCode   :: !(Maybe LoadBalancerStateEnum)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbsReason' - A description of the state.
--
-- * 'lbsCode' - The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
loadBalancerState
    :: LoadBalancerState
loadBalancerState =
  LoadBalancerState' {_lbsReason = Nothing, _lbsCode = Nothing}


-- | A description of the state.
lbsReason :: Lens' LoadBalancerState (Maybe Text)
lbsReason = lens _lbsReason (\ s a -> s{_lbsReason = a})

-- | The state code. The initial state of the load balancer is @provisioning@ . After the load balancer is fully set up and ready to route traffic, its state is @active@ . If the load balancer could not be set up, its state is @failed@ .
lbsCode :: Lens' LoadBalancerState (Maybe LoadBalancerStateEnum)
lbsCode = lens _lbsCode (\ s a -> s{_lbsCode = a})

instance FromXML LoadBalancerState where
        parseXML x
          = LoadBalancerState' <$>
              (x .@? "Reason") <*> (x .@? "Code")

instance Hashable LoadBalancerState where

instance NFData LoadBalancerState where

-- | Information to use when checking for a successful response from a target.
--
--
--
-- /See:/ 'matcher' smart constructor.
newtype Matcher = Matcher'
  { _mHTTPCode :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Matcher' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mHTTPCode' - The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200–399.
matcher
    :: Text -- ^ 'mHTTPCode'
    -> Matcher
matcher pHTTPCode_ = Matcher' {_mHTTPCode = pHTTPCode_}


-- | The HTTP codes. For Application Load Balancers, you can specify values between 200 and 499, and the default value is 200. You can specify multiple values (for example, "200,202") or a range of values (for example, "200-299"). For Network Load Balancers, this is 200–399.
mHTTPCode :: Lens' Matcher Text
mHTTPCode = lens _mHTTPCode (\ s a -> s{_mHTTPCode = a})

instance FromXML Matcher where
        parseXML x = Matcher' <$> (x .@ "HttpCode")

instance Hashable Matcher where

instance NFData Matcher where

instance ToQuery Matcher where
        toQuery Matcher'{..}
          = mconcat ["HttpCode" =: _mHTTPCode]

-- | Information about a redirect action.
--
--
-- A URI consists of the following components: protocol://hostname:port/path?query. You must modify at least one of the following components to avoid a redirect loop: protocol, hostname, port, or path. Any components that you do not modify retain their original values.
--
-- You can reuse URI components using the following reserved keywords:
--
--     * #{protocol}
--
--     * #{host}
--
--     * #{port}
--
--     * #{path} (the leading "/" is removed)
--
--     * #{query}
--
--
--
-- For example, you can change the path to "/new/#{path}", the hostname to "example.#{host}", or the query to "#{query}&value=xyz".
--
--
-- /See:/ 'redirectActionConfig' smart constructor.
data RedirectActionConfig = RedirectActionConfig'
  { _racPath       :: !(Maybe Text)
  , _racProtocol   :: !(Maybe Text)
  , _racQuery      :: !(Maybe Text)
  , _racHost       :: !(Maybe Text)
  , _racPort       :: !(Maybe Text)
  , _racStatusCode :: !RedirectActionStatusCodeEnum
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RedirectActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'racPath' - The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
--
-- * 'racProtocol' - The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
--
-- * 'racQuery' - The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
--
-- * 'racHost' - The hostname. This component is not percent-encoded. The hostname can contain #{host}.
--
-- * 'racPort' - The port. You can specify a value from 1 to 65535 or #{port}.
--
-- * 'racStatusCode' - The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
redirectActionConfig
    :: RedirectActionStatusCodeEnum -- ^ 'racStatusCode'
    -> RedirectActionConfig
redirectActionConfig pStatusCode_ =
  RedirectActionConfig'
    { _racPath = Nothing
    , _racProtocol = Nothing
    , _racQuery = Nothing
    , _racHost = Nothing
    , _racPort = Nothing
    , _racStatusCode = pStatusCode_
    }


-- | The absolute path, starting with the leading "/". This component is not percent-encoded. The path can contain #{host}, #{path}, and #{port}.
racPath :: Lens' RedirectActionConfig (Maybe Text)
racPath = lens _racPath (\ s a -> s{_racPath = a})

-- | The protocol. You can specify HTTP, HTTPS, or #{protocol}. You can redirect HTTP to HTTP, HTTP to HTTPS, and HTTPS to HTTPS. You cannot redirect HTTPS to HTTP.
racProtocol :: Lens' RedirectActionConfig (Maybe Text)
racProtocol = lens _racProtocol (\ s a -> s{_racProtocol = a})

-- | The query parameters, URL-encoded when necessary, but not percent-encoded. Do not include the leading "?", as it is automatically added. You can specify any of the reserved keywords.
racQuery :: Lens' RedirectActionConfig (Maybe Text)
racQuery = lens _racQuery (\ s a -> s{_racQuery = a})

-- | The hostname. This component is not percent-encoded. The hostname can contain #{host}.
racHost :: Lens' RedirectActionConfig (Maybe Text)
racHost = lens _racHost (\ s a -> s{_racHost = a})

-- | The port. You can specify a value from 1 to 65535 or #{port}.
racPort :: Lens' RedirectActionConfig (Maybe Text)
racPort = lens _racPort (\ s a -> s{_racPort = a})

-- | The HTTP redirect code. The redirect is either permanent (HTTP 301) or temporary (HTTP 302).
racStatusCode :: Lens' RedirectActionConfig RedirectActionStatusCodeEnum
racStatusCode = lens _racStatusCode (\ s a -> s{_racStatusCode = a})

instance FromXML RedirectActionConfig where
        parseXML x
          = RedirectActionConfig' <$>
              (x .@? "Path") <*> (x .@? "Protocol") <*>
                (x .@? "Query")
                <*> (x .@? "Host")
                <*> (x .@? "Port")
                <*> (x .@ "StatusCode")

instance Hashable RedirectActionConfig where

instance NFData RedirectActionConfig where

instance ToQuery RedirectActionConfig where
        toQuery RedirectActionConfig'{..}
          = mconcat
              ["Path" =: _racPath, "Protocol" =: _racProtocol,
               "Query" =: _racQuery, "Host" =: _racHost,
               "Port" =: _racPort, "StatusCode" =: _racStatusCode]

-- | Information about a rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rPriority   :: !(Maybe Text)
  , _rActions    :: !(Maybe [Action])
  , _rConditions :: !(Maybe [RuleCondition])
  , _rRuleARN    :: !(Maybe Text)
  , _rIsDefault  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rPriority' - The priority.
--
-- * 'rActions' - The actions.
--
-- * 'rConditions' - The conditions.
--
-- * 'rRuleARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'rIsDefault' - Indicates whether this is the default rule.
rule
    :: Rule
rule =
  Rule'
    { _rPriority = Nothing
    , _rActions = Nothing
    , _rConditions = Nothing
    , _rRuleARN = Nothing
    , _rIsDefault = Nothing
    }


-- | The priority.
rPriority :: Lens' Rule (Maybe Text)
rPriority = lens _rPriority (\ s a -> s{_rPriority = a})

-- | The actions.
rActions :: Lens' Rule [Action]
rActions = lens _rActions (\ s a -> s{_rActions = a}) . _Default . _Coerce

-- | The conditions.
rConditions :: Lens' Rule [RuleCondition]
rConditions = lens _rConditions (\ s a -> s{_rConditions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the rule.
rRuleARN :: Lens' Rule (Maybe Text)
rRuleARN = lens _rRuleARN (\ s a -> s{_rRuleARN = a})

-- | Indicates whether this is the default rule.
rIsDefault :: Lens' Rule (Maybe Bool)
rIsDefault = lens _rIsDefault (\ s a -> s{_rIsDefault = a})

instance FromXML Rule where
        parseXML x
          = Rule' <$>
              (x .@? "Priority") <*>
                (x .@? "Actions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "Conditions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "RuleArn")
                <*> (x .@? "IsDefault")

instance Hashable Rule where

instance NFData Rule where

-- | Information about a condition for a rule.
--
--
--
-- /See:/ 'ruleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { _rcField  :: !(Maybe Text)
  , _rcValues :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RuleCondition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcField' - The name of the field. The possible values are @host-header@ and @path-pattern@ .
--
-- * 'rcValues' - The condition value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. You can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern (for example, /img/*). A path pattern is case-sensitive, can be up to 128 characters in length, and can contain any of the following characters. You can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
ruleCondition
    :: RuleCondition
ruleCondition = RuleCondition' {_rcField = Nothing, _rcValues = Nothing}


-- | The name of the field. The possible values are @host-header@ and @path-pattern@ .
rcField :: Lens' RuleCondition (Maybe Text)
rcField = lens _rcField (\ s a -> s{_rcField = a})

-- | The condition value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. You can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern (for example, /img/*). A path pattern is case-sensitive, can be up to 128 characters in length, and can contain any of the following characters. You can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
rcValues :: Lens' RuleCondition [Text]
rcValues = lens _rcValues (\ s a -> s{_rcValues = a}) . _Default . _Coerce

instance FromXML RuleCondition where
        parseXML x
          = RuleCondition' <$>
              (x .@? "Field") <*>
                (x .@? "Values" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable RuleCondition where

instance NFData RuleCondition where

instance ToQuery RuleCondition where
        toQuery RuleCondition'{..}
          = mconcat
              ["Field" =: _rcField,
               "Values" =:
                 toQuery (toQueryList "member" <$> _rcValues)]

-- | Information about the priorities for the rules for a listener.
--
--
--
-- /See:/ 'rulePriorityPair' smart constructor.
data RulePriorityPair = RulePriorityPair'
  { _rppPriority :: !(Maybe Nat)
  , _rppRuleARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RulePriorityPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rppPriority' - The rule priority.
--
-- * 'rppRuleARN' - The Amazon Resource Name (ARN) of the rule.
rulePriorityPair
    :: RulePriorityPair
rulePriorityPair =
  RulePriorityPair' {_rppPriority = Nothing, _rppRuleARN = Nothing}


-- | The rule priority.
rppPriority :: Lens' RulePriorityPair (Maybe Natural)
rppPriority = lens _rppPriority (\ s a -> s{_rppPriority = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the rule.
rppRuleARN :: Lens' RulePriorityPair (Maybe Text)
rppRuleARN = lens _rppRuleARN (\ s a -> s{_rppRuleARN = a})

instance Hashable RulePriorityPair where

instance NFData RulePriorityPair where

instance ToQuery RulePriorityPair where
        toQuery RulePriorityPair'{..}
          = mconcat
              ["Priority" =: _rppPriority,
               "RuleArn" =: _rppRuleARN]

-- | Information about a policy used for SSL negotiation.
--
--
--
-- /See:/ 'sslPolicy' smart constructor.
data SSLPolicy = SSLPolicy'
  { _spCiphers      :: !(Maybe [Cipher])
  , _spName         :: !(Maybe Text)
  , _spSSLProtocols :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSLPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spCiphers' - The ciphers.
--
-- * 'spName' - The name of the policy.
--
-- * 'spSSLProtocols' - The protocols.
sslPolicy
    :: SSLPolicy
sslPolicy =
  SSLPolicy'
    {_spCiphers = Nothing, _spName = Nothing, _spSSLProtocols = Nothing}


-- | The ciphers.
spCiphers :: Lens' SSLPolicy [Cipher]
spCiphers = lens _spCiphers (\ s a -> s{_spCiphers = a}) . _Default . _Coerce

-- | The name of the policy.
spName :: Lens' SSLPolicy (Maybe Text)
spName = lens _spName (\ s a -> s{_spName = a})

-- | The protocols.
spSSLProtocols :: Lens' SSLPolicy [Text]
spSSLProtocols = lens _spSSLProtocols (\ s a -> s{_spSSLProtocols = a}) . _Default . _Coerce

instance FromXML SSLPolicy where
        parseXML x
          = SSLPolicy' <$>
              (x .@? "Ciphers" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "Name")
                <*>
                (x .@? "SslProtocols" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable SSLPolicy where

instance NFData SSLPolicy where

-- | Information about a subnet mapping.
--
--
--
-- /See:/ 'subnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { _smAllocationId :: !(Maybe Text)
  , _smSubnetId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubnetMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smAllocationId' - [Network Load Balancers] The allocation ID of the Elastic IP address.
--
-- * 'smSubnetId' - The ID of the subnet.
subnetMapping
    :: SubnetMapping
subnetMapping =
  SubnetMapping' {_smAllocationId = Nothing, _smSubnetId = Nothing}


-- | [Network Load Balancers] The allocation ID of the Elastic IP address.
smAllocationId :: Lens' SubnetMapping (Maybe Text)
smAllocationId = lens _smAllocationId (\ s a -> s{_smAllocationId = a})

-- | The ID of the subnet.
smSubnetId :: Lens' SubnetMapping (Maybe Text)
smSubnetId = lens _smSubnetId (\ s a -> s{_smSubnetId = a})

instance Hashable SubnetMapping where

instance NFData SubnetMapping where

instance ToQuery SubnetMapping where
        toQuery SubnetMapping'{..}
          = mconcat
              ["AllocationId" =: _smAllocationId,
               "SubnetId" =: _smSubnetId]

-- | Information about a tag.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of the tag.
--
-- * 'tagKey' - The key of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@ "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | The tags associated with a resource.
--
--
--
-- /See:/ 'tagDescription' smart constructor.
data TagDescription = TagDescription'
  { _tdResourceARN :: !(Maybe Text)
  , _tdTags        :: !(Maybe (List1 Tag))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdResourceARN' - The Amazon Resource Name (ARN) of the resource.
--
-- * 'tdTags' - Information about the tags.
tagDescription
    :: TagDescription
tagDescription = TagDescription' {_tdResourceARN = Nothing, _tdTags = Nothing}


-- | The Amazon Resource Name (ARN) of the resource.
tdResourceARN :: Lens' TagDescription (Maybe Text)
tdResourceARN = lens _tdResourceARN (\ s a -> s{_tdResourceARN = a})

-- | Information about the tags.
tdTags :: Lens' TagDescription (Maybe (NonEmpty Tag))
tdTags = lens _tdTags (\ s a -> s{_tdTags = a}) . mapping _List1

instance FromXML TagDescription where
        parseXML x
          = TagDescription' <$>
              (x .@? "ResourceArn") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList1 "member"))

instance Hashable TagDescription where

instance NFData TagDescription where

-- | Information about a target.
--
--
--
-- /See:/ 'targetDescription' smart constructor.
data TargetDescription = TargetDescription'
  { _tdAvailabilityZone :: !(Maybe Text)
  , _tdPort             :: !(Maybe Nat)
  , _tdId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdAvailabilityZone' - An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ . If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
--
-- * 'tdPort' - The port on which the target is listening.
--
-- * 'tdId' - The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
targetDescription
    :: Text -- ^ 'tdId'
    -> TargetDescription
targetDescription pId_ =
  TargetDescription'
    {_tdAvailabilityZone = Nothing, _tdPort = Nothing, _tdId = pId_}


-- | An Availability Zone or @all@ . This determines whether the target receives traffic from the load balancer nodes in the specified Availability Zone or from all enabled Availability Zones for the load balancer. This parameter is not supported if the target type of the target group is @instance@ . If the target type is @ip@ and the IP address is in a subnet of the VPC for the target group, the Availability Zone is automatically detected and this parameter is optional. If the IP address is outside the VPC, this parameter is required. With an Application Load Balancer, if the target type is @ip@ and the IP address is outside the VPC for the target group, the only supported value is @all@ . If the target type is @lambda@ , this parameter is optional and the only supported value is @all@ .
tdAvailabilityZone :: Lens' TargetDescription (Maybe Text)
tdAvailabilityZone = lens _tdAvailabilityZone (\ s a -> s{_tdAvailabilityZone = a})

-- | The port on which the target is listening.
tdPort :: Lens' TargetDescription (Maybe Natural)
tdPort = lens _tdPort (\ s a -> s{_tdPort = a}) . mapping _Nat

-- | The ID of the target. If the target type of the target group is @instance@ , specify an instance ID. If the target type is @ip@ , specify an IP address. If the target type is @lambda@ , specify the ARN of the Lambda function.
tdId :: Lens' TargetDescription Text
tdId = lens _tdId (\ s a -> s{_tdId = a})

instance FromXML TargetDescription where
        parseXML x
          = TargetDescription' <$>
              (x .@? "AvailabilityZone") <*> (x .@? "Port") <*>
                (x .@ "Id")

instance Hashable TargetDescription where

instance NFData TargetDescription where

instance ToQuery TargetDescription where
        toQuery TargetDescription'{..}
          = mconcat
              ["AvailabilityZone" =: _tdAvailabilityZone,
               "Port" =: _tdPort, "Id" =: _tdId]

-- | Information about a target group.
--
--
--
-- /See:/ 'targetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { _tgMatcher                    :: !(Maybe Matcher)
  , _tgHealthCheckPath            :: !(Maybe Text)
  , _tgHealthCheckEnabled         :: !(Maybe Bool)
  , _tgUnhealthyThresholdCount    :: !(Maybe Nat)
  , _tgVPCId                      :: !(Maybe Text)
  , _tgTargetGroupARN             :: !(Maybe Text)
  , _tgProtocol                   :: !(Maybe ProtocolEnum)
  , _tgHealthCheckIntervalSeconds :: !(Maybe Nat)
  , _tgTargetType                 :: !(Maybe TargetTypeEnum)
  , _tgHealthyThresholdCount      :: !(Maybe Nat)
  , _tgHealthCheckProtocol        :: !(Maybe ProtocolEnum)
  , _tgLoadBalancerARNs           :: !(Maybe [Text])
  , _tgHealthCheckTimeoutSeconds  :: !(Maybe Nat)
  , _tgHealthCheckPort            :: !(Maybe Text)
  , _tgTargetGroupName            :: !(Maybe Text)
  , _tgPort                       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgMatcher' - The HTTP codes to use when checking for a successful response from a target.
--
-- * 'tgHealthCheckPath' - The destination for the health check request.
--
-- * 'tgHealthCheckEnabled' - Indicates whether health checks are enabled.
--
-- * 'tgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy.
--
-- * 'tgVPCId' - The ID of the VPC for the targets.
--
-- * 'tgTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
--
-- * 'tgProtocol' - The protocol to use for routing traffic to the targets.
--
-- * 'tgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target.
--
-- * 'tgTargetType' - The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
--
-- * 'tgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- * 'tgHealthCheckProtocol' - The protocol to use to connect with the target.
--
-- * 'tgLoadBalancerARNs' - The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
--
-- * 'tgHealthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed health check.
--
-- * 'tgHealthCheckPort' - The port to use to connect with the target.
--
-- * 'tgTargetGroupName' - The name of the target group.
--
-- * 'tgPort' - The port on which the targets are listening.
targetGroup
    :: TargetGroup
targetGroup =
  TargetGroup'
    { _tgMatcher = Nothing
    , _tgHealthCheckPath = Nothing
    , _tgHealthCheckEnabled = Nothing
    , _tgUnhealthyThresholdCount = Nothing
    , _tgVPCId = Nothing
    , _tgTargetGroupARN = Nothing
    , _tgProtocol = Nothing
    , _tgHealthCheckIntervalSeconds = Nothing
    , _tgTargetType = Nothing
    , _tgHealthyThresholdCount = Nothing
    , _tgHealthCheckProtocol = Nothing
    , _tgLoadBalancerARNs = Nothing
    , _tgHealthCheckTimeoutSeconds = Nothing
    , _tgHealthCheckPort = Nothing
    , _tgTargetGroupName = Nothing
    , _tgPort = Nothing
    }


-- | The HTTP codes to use when checking for a successful response from a target.
tgMatcher :: Lens' TargetGroup (Maybe Matcher)
tgMatcher = lens _tgMatcher (\ s a -> s{_tgMatcher = a})

-- | The destination for the health check request.
tgHealthCheckPath :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPath = lens _tgHealthCheckPath (\ s a -> s{_tgHealthCheckPath = a})

-- | Indicates whether health checks are enabled.
tgHealthCheckEnabled :: Lens' TargetGroup (Maybe Bool)
tgHealthCheckEnabled = lens _tgHealthCheckEnabled (\ s a -> s{_tgHealthCheckEnabled = a})

-- | The number of consecutive health check failures required before considering the target unhealthy.
tgUnhealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgUnhealthyThresholdCount = lens _tgUnhealthyThresholdCount (\ s a -> s{_tgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The ID of the VPC for the targets.
tgVPCId :: Lens' TargetGroup (Maybe Text)
tgVPCId = lens _tgVPCId (\ s a -> s{_tgVPCId = a})

-- | The Amazon Resource Name (ARN) of the target group.
tgTargetGroupARN :: Lens' TargetGroup (Maybe Text)
tgTargetGroupARN = lens _tgTargetGroupARN (\ s a -> s{_tgTargetGroupARN = a})

-- | The protocol to use for routing traffic to the targets.
tgProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgProtocol = lens _tgProtocol (\ s a -> s{_tgProtocol = a})

-- | The approximate amount of time, in seconds, between health checks of an individual target.
tgHealthCheckIntervalSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckIntervalSeconds = lens _tgHealthCheckIntervalSeconds (\ s a -> s{_tgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
tgTargetType :: Lens' TargetGroup (Maybe TargetTypeEnum)
tgTargetType = lens _tgTargetType (\ s a -> s{_tgTargetType = a})

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
tgHealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgHealthyThresholdCount = lens _tgHealthyThresholdCount (\ s a -> s{_tgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol to use to connect with the target.
tgHealthCheckProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgHealthCheckProtocol = lens _tgHealthCheckProtocol (\ s a -> s{_tgHealthCheckProtocol = a})

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
tgLoadBalancerARNs :: Lens' TargetGroup [Text]
tgLoadBalancerARNs = lens _tgLoadBalancerARNs (\ s a -> s{_tgLoadBalancerARNs = a}) . _Default . _Coerce

-- | The amount of time, in seconds, during which no response means a failed health check.
tgHealthCheckTimeoutSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckTimeoutSeconds = lens _tgHealthCheckTimeoutSeconds (\ s a -> s{_tgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port to use to connect with the target.
tgHealthCheckPort :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPort = lens _tgHealthCheckPort (\ s a -> s{_tgHealthCheckPort = a})

-- | The name of the target group.
tgTargetGroupName :: Lens' TargetGroup (Maybe Text)
tgTargetGroupName = lens _tgTargetGroupName (\ s a -> s{_tgTargetGroupName = a})

-- | The port on which the targets are listening.
tgPort :: Lens' TargetGroup (Maybe Natural)
tgPort = lens _tgPort (\ s a -> s{_tgPort = a}) . mapping _Nat

instance FromXML TargetGroup where
        parseXML x
          = TargetGroup' <$>
              (x .@? "Matcher") <*> (x .@? "HealthCheckPath") <*>
                (x .@? "HealthCheckEnabled")
                <*> (x .@? "UnhealthyThresholdCount")
                <*> (x .@? "VpcId")
                <*> (x .@? "TargetGroupArn")
                <*> (x .@? "Protocol")
                <*> (x .@? "HealthCheckIntervalSeconds")
                <*> (x .@? "TargetType")
                <*> (x .@? "HealthyThresholdCount")
                <*> (x .@? "HealthCheckProtocol")
                <*>
                (x .@? "LoadBalancerArns" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckTimeoutSeconds")
                <*> (x .@? "HealthCheckPort")
                <*> (x .@? "TargetGroupName")
                <*> (x .@? "Port")

instance Hashable TargetGroup where

instance NFData TargetGroup where

-- | Information about a target group attribute.
--
--
--
-- /See:/ 'targetGroupAttribute' smart constructor.
data TargetGroupAttribute = TargetGroupAttribute'
  { _tgaValue :: !(Maybe Text)
  , _tgaKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetGroupAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgaValue' - The value of the attribute.
--
-- * 'tgaKey' - The name of the attribute. The following attribute is supported by both Application Load Balancers and Network Load Balancers:     * @deregistration_delay.timeout_seconds@ - The amount of time, in seconds, for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds. If the target is a Lambda function, this attribute is not supported. The following attributes are supported by Application Load Balancers if the target is not a Lambda function:     * @slow_start.duration_seconds@ - The time period, in seconds, during which a newly registered target receives a linearly increasing share of the traffic to the target group. After this time period ends, the target receives its full share of traffic. The range is 30-900 seconds (15 minutes). Slow start mode is disabled by default.     * @stickiness.enabled@ - Indicates whether sticky sessions are enabled. The value is @true@ or @false@ . The default is @false@ .     * @stickiness.type@ - The type of sticky sessions. The possible value is @lb_cookie@ .     * @stickiness.lb_cookie.duration_seconds@ - The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds). The following attribute is supported only if the target is a Lambda function.     * @lambda.multi_value_headers.enabled@ - Indicates whether the request and response headers exchanged between the load balancer and the Lambda function include arrays of values or strings. The value is @true@ or @false@ . The default is @false@ . If the value is @false@ and the request contains a duplicate header field name or query parameter key, the load balancer uses the last value sent by the client. The following attribute is supported only by Network Load Balancers:     * @proxy_protocol_v2.enabled@ - Indicates whether Proxy Protocol version 2 is enabled. The value is @true@ or @false@ . The default is @false@ .
targetGroupAttribute
    :: TargetGroupAttribute
targetGroupAttribute =
  TargetGroupAttribute' {_tgaValue = Nothing, _tgaKey = Nothing}


-- | The value of the attribute.
tgaValue :: Lens' TargetGroupAttribute (Maybe Text)
tgaValue = lens _tgaValue (\ s a -> s{_tgaValue = a})

-- | The name of the attribute. The following attribute is supported by both Application Load Balancers and Network Load Balancers:     * @deregistration_delay.timeout_seconds@ - The amount of time, in seconds, for Elastic Load Balancing to wait before changing the state of a deregistering target from @draining@ to @unused@ . The range is 0-3600 seconds. The default value is 300 seconds. If the target is a Lambda function, this attribute is not supported. The following attributes are supported by Application Load Balancers if the target is not a Lambda function:     * @slow_start.duration_seconds@ - The time period, in seconds, during which a newly registered target receives a linearly increasing share of the traffic to the target group. After this time period ends, the target receives its full share of traffic. The range is 30-900 seconds (15 minutes). Slow start mode is disabled by default.     * @stickiness.enabled@ - Indicates whether sticky sessions are enabled. The value is @true@ or @false@ . The default is @false@ .     * @stickiness.type@ - The type of sticky sessions. The possible value is @lb_cookie@ .     * @stickiness.lb_cookie.duration_seconds@ - The time period, in seconds, during which requests from a client should be routed to the same target. After this time period expires, the load balancer-generated cookie is considered stale. The range is 1 second to 1 week (604800 seconds). The default value is 1 day (86400 seconds). The following attribute is supported only if the target is a Lambda function.     * @lambda.multi_value_headers.enabled@ - Indicates whether the request and response headers exchanged between the load balancer and the Lambda function include arrays of values or strings. The value is @true@ or @false@ . The default is @false@ . If the value is @false@ and the request contains a duplicate header field name or query parameter key, the load balancer uses the last value sent by the client. The following attribute is supported only by Network Load Balancers:     * @proxy_protocol_v2.enabled@ - Indicates whether Proxy Protocol version 2 is enabled. The value is @true@ or @false@ . The default is @false@ .
tgaKey :: Lens' TargetGroupAttribute (Maybe Text)
tgaKey = lens _tgaKey (\ s a -> s{_tgaKey = a})

instance FromXML TargetGroupAttribute where
        parseXML x
          = TargetGroupAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable TargetGroupAttribute where

instance NFData TargetGroupAttribute where

instance ToQuery TargetGroupAttribute where
        toQuery TargetGroupAttribute'{..}
          = mconcat ["Value" =: _tgaValue, "Key" =: _tgaKey]

-- | Information about the current health of a target.
--
--
--
-- /See:/ 'targetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { _thState       :: !(Maybe TargetHealthStateEnum)
  , _thReason      :: !(Maybe TargetHealthReasonEnum)
  , _thDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thState' - The state of the target.
--
-- * 'thReason' - The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code.     * @Target.Timeout@ - The health check requests timed out.     * @Target.FailedHealthChecks@ - The health checks failed because the connection to the target timed out, the target response was malformed, or the target failed the health check for an unknown reason.     * @Elb.InternalError@ - The health checks failed due to an internal error. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired. If the target state is @unavailable@ , the reason code can be the following value:     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group.
--
-- * 'thDescription' - A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
targetHealth
    :: TargetHealth
targetHealth =
  TargetHealth'
    {_thState = Nothing, _thReason = Nothing, _thDescription = Nothing}


-- | The state of the target.
thState :: Lens' TargetHealth (Maybe TargetHealthStateEnum)
thState = lens _thState (\ s a -> s{_thState = a})

-- | The reason code. If the target state is @healthy@ , a reason code is not provided. If the target state is @initial@ , the reason code can be one of the following values:     * @Elb.RegistrationInProgress@ - The target is in the process of being registered with the load balancer.     * @Elb.InitialHealthChecking@ - The load balancer is still sending the target the minimum number of health checks required to determine its health status. If the target state is @unhealthy@ , the reason code can be one of the following values:     * @Target.ResponseCodeMismatch@ - The health checks did not return an expected HTTP code.     * @Target.Timeout@ - The health check requests timed out.     * @Target.FailedHealthChecks@ - The health checks failed because the connection to the target timed out, the target response was malformed, or the target failed the health check for an unknown reason.     * @Elb.InternalError@ - The health checks failed due to an internal error. If the target state is @unused@ , the reason code can be one of the following values:     * @Target.NotRegistered@ - The target is not registered with the target group.     * @Target.NotInUse@ - The target group is not used by any load balancer or the target is in an Availability Zone that is not enabled for its load balancer.     * @Target.IpUnusable@ - The target IP address is reserved for use by a load balancer.     * @Target.InvalidState@ - The target is in the stopped or terminated state. If the target state is @draining@ , the reason code can be the following value:     * @Target.DeregistrationInProgress@ - The target is in the process of being deregistered and the deregistration delay period has not expired. If the target state is @unavailable@ , the reason code can be the following value:     * @Target.HealthCheckDisabled@ - Health checks are disabled for the target group.
thReason :: Lens' TargetHealth (Maybe TargetHealthReasonEnum)
thReason = lens _thReason (\ s a -> s{_thReason = a})

-- | A description of the target health that provides additional details. If the state is @healthy@ , a description is not provided.
thDescription :: Lens' TargetHealth (Maybe Text)
thDescription = lens _thDescription (\ s a -> s{_thDescription = a})

instance FromXML TargetHealth where
        parseXML x
          = TargetHealth' <$>
              (x .@? "State") <*> (x .@? "Reason") <*>
                (x .@? "Description")

instance Hashable TargetHealth where

instance NFData TargetHealth where

-- | Information about the health of a target.
--
--
--
-- /See:/ 'targetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { _thdTargetHealth    :: !(Maybe TargetHealth)
  , _thdHealthCheckPort :: !(Maybe Text)
  , _thdTarget          :: !(Maybe TargetDescription)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetHealthDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'thdTargetHealth' - The health information for the target.
--
-- * 'thdHealthCheckPort' - The port to use to connect with the target.
--
-- * 'thdTarget' - The description of the target.
targetHealthDescription
    :: TargetHealthDescription
targetHealthDescription =
  TargetHealthDescription'
    { _thdTargetHealth = Nothing
    , _thdHealthCheckPort = Nothing
    , _thdTarget = Nothing
    }


-- | The health information for the target.
thdTargetHealth :: Lens' TargetHealthDescription (Maybe TargetHealth)
thdTargetHealth = lens _thdTargetHealth (\ s a -> s{_thdTargetHealth = a})

-- | The port to use to connect with the target.
thdHealthCheckPort :: Lens' TargetHealthDescription (Maybe Text)
thdHealthCheckPort = lens _thdHealthCheckPort (\ s a -> s{_thdHealthCheckPort = a})

-- | The description of the target.
thdTarget :: Lens' TargetHealthDescription (Maybe TargetDescription)
thdTarget = lens _thdTarget (\ s a -> s{_thdTarget = a})

instance FromXML TargetHealthDescription where
        parseXML x
          = TargetHealthDescription' <$>
              (x .@? "TargetHealth") <*> (x .@? "HealthCheckPort")
                <*> (x .@? "Target")

instance Hashable TargetHealthDescription where

instance NFData TargetHealthDescription where
