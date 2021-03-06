{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.ModifyListener
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the specified properties of the specified listener. Any properties that you do not specify remain unchanged.
--
--
-- Changing the protocol from HTTPS to HTTP, or from TLS to TCP, removes the security policy and default certificate properties. If you change the protocol from HTTP to HTTPS, or from TCP to TLS, you must add the security policy and default certificate properties.
--
-- To add an item to a list, remove an item from a list, or update an item in a list, you must provide the entire list. For example, to add an action, specify a list with the current actions plus the new action.
--
module Network.AWS.ELBv2.ModifyListener
    (
    -- * Creating a Request
      modifyListener
    , ModifyListener
    -- * Request Lenses
    , mlSSLPolicy
    , mlProtocol
    , mlDefaultActions
    , mlCertificates
    , mlPort
    , mlListenerARN

    -- * Destructuring the Response
    , modifyListenerResponse
    , ModifyListenerResponse
    -- * Response Lenses
    , mlrsListeners
    , mlrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyListener' smart constructor.
data ModifyListener = ModifyListener'{_mlSSLPolicy ::
                                      !(Maybe Text),
                                      _mlProtocol :: !(Maybe ProtocolEnum),
                                      _mlDefaultActions :: !(Maybe [Action]),
                                      _mlCertificates :: !(Maybe [Certificate]),
                                      _mlPort :: !(Maybe Nat),
                                      _mlListenerARN :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyListener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlSSLPolicy' - [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported. The following are the possible values:     * @ELBSecurityPolicy-2016-08@      * @ELBSecurityPolicy-TLS-1-0-2015-04@      * @ELBSecurityPolicy-TLS-1-1-2017-01@      * @ELBSecurityPolicy-TLS-1-2-2017-01@      * @ELBSecurityPolicy-TLS-1-2-Ext-2018-06@      * @ELBSecurityPolicy-FS-2018-06@      * @ELBSecurityPolicy-FS-1-1-2019-08@      * @ELBSecurityPolicy-FS-1-2-2019-08@      * @ELBSecurityPolicy-FS-1-2-Res-2019-08@  For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security Policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security Policies> in the /Network Load Balancers Guide/ .
--
-- * 'mlProtocol' - The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols.
--
-- * 'mlDefaultActions' - The actions for the default rule. The rule must include one forward action or one or more fixed-response actions. If the action type is @forward@ , you specify one or more target groups. The protocol of the target group must be HTTP or HTTPS for an Application Load Balancer. The protocol of the target group must be TCP, TLS, UDP, or TCP_UDP for a Network Load Balancer. [HTTPS listeners] If the action type is @authenticate-oidc@ , you authenticate users through an identity provider that is OpenID Connect (OIDC) compliant. [HTTPS listeners] If the action type is @authenticate-cognito@ , you authenticate users through the user pools supported by Amazon Cognito. [Application Load Balancer] If the action type is @redirect@ , you redirect specified client requests from one URL to another. [Application Load Balancer] If the action type is @fixed-response@ , you drop specified client requests and return a custom HTTP response.
--
-- * 'mlCertificates' - [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ . To create a certificate list, use 'AddListenerCertificates' .
--
-- * 'mlPort' - The port for connections from clients to the load balancer.
--
-- * 'mlListenerARN' - The Amazon Resource Name (ARN) of the listener.
modifyListener
    :: Text -- ^ 'mlListenerARN'
    -> ModifyListener
modifyListener pListenerARN_
  = ModifyListener'{_mlSSLPolicy = Nothing,
                    _mlProtocol = Nothing, _mlDefaultActions = Nothing,
                    _mlCertificates = Nothing, _mlPort = Nothing,
                    _mlListenerARN = pListenerARN_}

-- | [HTTPS and TLS listeners] The security policy that defines which protocols and ciphers are supported. The following are the possible values:     * @ELBSecurityPolicy-2016-08@      * @ELBSecurityPolicy-TLS-1-0-2015-04@      * @ELBSecurityPolicy-TLS-1-1-2017-01@      * @ELBSecurityPolicy-TLS-1-2-2017-01@      * @ELBSecurityPolicy-TLS-1-2-Ext-2018-06@      * @ELBSecurityPolicy-FS-2018-06@      * @ELBSecurityPolicy-FS-1-1-2019-08@      * @ELBSecurityPolicy-FS-1-2-2019-08@      * @ELBSecurityPolicy-FS-1-2-Res-2019-08@  For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies Security Policies> in the /Application Load Balancers Guide/ and <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html#describe-ssl-policies Security Policies> in the /Network Load Balancers Guide/ .
mlSSLPolicy :: Lens' ModifyListener (Maybe Text)
mlSSLPolicy = lens _mlSSLPolicy (\ s a -> s{_mlSSLPolicy = a})

-- | The protocol for connections from clients to the load balancer. Application Load Balancers support the HTTP and HTTPS protocols. Network Load Balancers support the TCP, TLS, UDP, and TCP_UDP protocols.
mlProtocol :: Lens' ModifyListener (Maybe ProtocolEnum)
mlProtocol = lens _mlProtocol (\ s a -> s{_mlProtocol = a})

-- | The actions for the default rule. The rule must include one forward action or one or more fixed-response actions. If the action type is @forward@ , you specify one or more target groups. The protocol of the target group must be HTTP or HTTPS for an Application Load Balancer. The protocol of the target group must be TCP, TLS, UDP, or TCP_UDP for a Network Load Balancer. [HTTPS listeners] If the action type is @authenticate-oidc@ , you authenticate users through an identity provider that is OpenID Connect (OIDC) compliant. [HTTPS listeners] If the action type is @authenticate-cognito@ , you authenticate users through the user pools supported by Amazon Cognito. [Application Load Balancer] If the action type is @redirect@ , you redirect specified client requests from one URL to another. [Application Load Balancer] If the action type is @fixed-response@ , you drop specified client requests and return a custom HTTP response.
mlDefaultActions :: Lens' ModifyListener [Action]
mlDefaultActions = lens _mlDefaultActions (\ s a -> s{_mlDefaultActions = a}) . _Default . _Coerce

-- | [HTTPS and TLS listeners] The default certificate for the listener. You must provide exactly one certificate. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ . To create a certificate list, use 'AddListenerCertificates' .
mlCertificates :: Lens' ModifyListener [Certificate]
mlCertificates = lens _mlCertificates (\ s a -> s{_mlCertificates = a}) . _Default . _Coerce

-- | The port for connections from clients to the load balancer.
mlPort :: Lens' ModifyListener (Maybe Natural)
mlPort = lens _mlPort (\ s a -> s{_mlPort = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the listener.
mlListenerARN :: Lens' ModifyListener Text
mlListenerARN = lens _mlListenerARN (\ s a -> s{_mlListenerARN = a})

instance AWSRequest ModifyListener where
        type Rs ModifyListener = ModifyListenerResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "ModifyListenerResult"
              (\ s h x ->
                 ModifyListenerResponse' <$>
                   (x .@? "Listeners" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyListener where

instance NFData ModifyListener where

instance ToHeaders ModifyListener where
        toHeaders = const mempty

instance ToPath ModifyListener where
        toPath = const "/"

instance ToQuery ModifyListener where
        toQuery ModifyListener'{..}
          = mconcat
              ["Action" =: ("ModifyListener" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "SslPolicy" =: _mlSSLPolicy,
               "Protocol" =: _mlProtocol,
               "DefaultActions" =:
                 toQuery (toQueryList "member" <$> _mlDefaultActions),
               "Certificates" =:
                 toQuery (toQueryList "member" <$> _mlCertificates),
               "Port" =: _mlPort, "ListenerArn" =: _mlListenerARN]

-- | /See:/ 'modifyListenerResponse' smart constructor.
data ModifyListenerResponse = ModifyListenerResponse'{_mlrsListeners
                                                      :: !(Maybe [Listener]),
                                                      _mlrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'ModifyListenerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mlrsListeners' - Information about the modified listener.
--
-- * 'mlrsResponseStatus' - -- | The response status code.
modifyListenerResponse
    :: Int -- ^ 'mlrsResponseStatus'
    -> ModifyListenerResponse
modifyListenerResponse pResponseStatus_
  = ModifyListenerResponse'{_mlrsListeners = Nothing,
                            _mlrsResponseStatus = pResponseStatus_}

-- | Information about the modified listener.
mlrsListeners :: Lens' ModifyListenerResponse [Listener]
mlrsListeners = lens _mlrsListeners (\ s a -> s{_mlrsListeners = a}) . _Default . _Coerce

-- | -- | The response status code.
mlrsResponseStatus :: Lens' ModifyListenerResponse Int
mlrsResponseStatus = lens _mlrsResponseStatus (\ s a -> s{_mlrsResponseStatus = a})

instance NFData ModifyListenerResponse where
