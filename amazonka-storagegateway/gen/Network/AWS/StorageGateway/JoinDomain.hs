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
-- Module      : Network.AWS.StorageGateway.JoinDomain
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a file gateway to an Active Directory domain. This operation is only supported for file gateways that support the SMB file protocol.
--
--
module Network.AWS.StorageGateway.JoinDomain
    (
    -- * Creating a Request
      joinDomain
    , JoinDomain
    -- * Request Lenses
    , jdOrganizationalUnit
    , jdDomainControllers
    , jdGatewayARN
    , jdDomainName
    , jdUserName
    , jdPassword

    -- * Destructuring the Response
    , joinDomainResponse
    , JoinDomainResponse
    -- * Response Lenses
    , jdrsGatewayARN
    , jdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | JoinDomainInput
--
--
--
-- /See:/ 'joinDomain' smart constructor.
data JoinDomain = JoinDomain'
  { _jdOrganizationalUnit :: !(Maybe Text)
  , _jdDomainControllers  :: !(Maybe [Text])
  , _jdGatewayARN         :: !Text
  , _jdDomainName         :: !Text
  , _jdUserName           :: !Text
  , _jdPassword           :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'JoinDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdOrganizationalUnit' - The organizational unit (OU) is a container with an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
--
-- * 'jdDomainControllers' - List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
--
-- * 'jdGatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and region.
--
-- * 'jdDomainName' - The name of the domain that you want the gateway to join.
--
-- * 'jdUserName' - Sets the user name of user who has permission to add the gateway to the Active Directory domain.
--
-- * 'jdPassword' - Sets the password of the user who has permission to add the gateway to the Active Directory domain.
joinDomain
    :: Text -- ^ 'jdGatewayARN'
    -> Text -- ^ 'jdDomainName'
    -> Text -- ^ 'jdUserName'
    -> Text -- ^ 'jdPassword'
    -> JoinDomain
joinDomain pGatewayARN_ pDomainName_ pUserName_ pPassword_ =
  JoinDomain'
    { _jdOrganizationalUnit = Nothing
    , _jdDomainControllers = Nothing
    , _jdGatewayARN = pGatewayARN_
    , _jdDomainName = pDomainName_
    , _jdUserName = pUserName_
    , _jdPassword = _Sensitive # pPassword_
    }


-- | The organizational unit (OU) is a container with an Active Directory that can hold users, groups, computers, and other OUs and this parameter specifies the OU that the gateway will join within the AD domain.
jdOrganizationalUnit :: Lens' JoinDomain (Maybe Text)
jdOrganizationalUnit = lens _jdOrganizationalUnit (\ s a -> s{_jdOrganizationalUnit = a})

-- | List of IPv4 addresses, NetBIOS names, or host names of your domain server. If you need to specify the port number include it after the colon (“:”). For example, @mydc.mydomain.com:389@ .
jdDomainControllers :: Lens' JoinDomain [Text]
jdDomainControllers = lens _jdDomainControllers (\ s a -> s{_jdDomainControllers = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the gateway. Use the @ListGateways@ operation to return a list of gateways for your account and region.
jdGatewayARN :: Lens' JoinDomain Text
jdGatewayARN = lens _jdGatewayARN (\ s a -> s{_jdGatewayARN = a})

-- | The name of the domain that you want the gateway to join.
jdDomainName :: Lens' JoinDomain Text
jdDomainName = lens _jdDomainName (\ s a -> s{_jdDomainName = a})

-- | Sets the user name of user who has permission to add the gateway to the Active Directory domain.
jdUserName :: Lens' JoinDomain Text
jdUserName = lens _jdUserName (\ s a -> s{_jdUserName = a})

-- | Sets the password of the user who has permission to add the gateway to the Active Directory domain.
jdPassword :: Lens' JoinDomain Text
jdPassword = lens _jdPassword (\ s a -> s{_jdPassword = a}) . _Sensitive

instance AWSRequest JoinDomain where
        type Rs JoinDomain = JoinDomainResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 JoinDomainResponse' <$>
                   (x .?> "GatewayARN") <*> (pure (fromEnum s)))

instance Hashable JoinDomain where

instance NFData JoinDomain where

instance ToHeaders JoinDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.JoinDomain" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON JoinDomain where
        toJSON JoinDomain'{..}
          = object
              (catMaybes
                 [("OrganizationalUnit" .=) <$> _jdOrganizationalUnit,
                  ("DomainControllers" .=) <$> _jdDomainControllers,
                  Just ("GatewayARN" .= _jdGatewayARN),
                  Just ("DomainName" .= _jdDomainName),
                  Just ("UserName" .= _jdUserName),
                  Just ("Password" .= _jdPassword)])

instance ToPath JoinDomain where
        toPath = const "/"

instance ToQuery JoinDomain where
        toQuery = const mempty

-- | JoinDomainOutput
--
--
--
-- /See:/ 'joinDomainResponse' smart constructor.
data JoinDomainResponse = JoinDomainResponse'
  { _jdrsGatewayARN     :: !(Maybe Text)
  , _jdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JoinDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jdrsGatewayARN' - The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
--
-- * 'jdrsResponseStatus' - -- | The response status code.
joinDomainResponse
    :: Int -- ^ 'jdrsResponseStatus'
    -> JoinDomainResponse
joinDomainResponse pResponseStatus_ =
  JoinDomainResponse'
    {_jdrsGatewayARN = Nothing, _jdrsResponseStatus = pResponseStatus_}


-- | The unique Amazon Resource Name (ARN) of the gateway that joined the domain.
jdrsGatewayARN :: Lens' JoinDomainResponse (Maybe Text)
jdrsGatewayARN = lens _jdrsGatewayARN (\ s a -> s{_jdrsGatewayARN = a})

-- | -- | The response status code.
jdrsResponseStatus :: Lens' JoinDomainResponse Int
jdrsResponseStatus = lens _jdrsResponseStatus (\ s a -> s{_jdrsResponseStatus = a})

instance NFData JoinDomainResponse where
