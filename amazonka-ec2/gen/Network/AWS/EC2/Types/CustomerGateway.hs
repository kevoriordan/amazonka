{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CustomerGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CustomerGateway where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a customer gateway.
--
--
--
-- /See:/ 'customerGateway' smart constructor.
data CustomerGateway = CustomerGateway'{_cgCertificateARN
                                        :: !(Maybe Text),
                                        _cgDeviceName :: !(Maybe Text),
                                        _cgTags :: !(Maybe [Tag]),
                                        _cgBGPASN :: !Text,
                                        _cgCustomerGatewayId :: !Text,
                                        _cgIPAddress :: !Text,
                                        _cgState :: !Text, _cgType :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomerGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgCertificateARN' - The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- * 'cgDeviceName' - The name of customer gateway device.
--
-- * 'cgTags' - Any tags assigned to the customer gateway.
--
-- * 'cgBGPASN' - The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
--
-- * 'cgCustomerGatewayId' - The ID of the customer gateway.
--
-- * 'cgIPAddress' - The Internet-routable IP address of the customer gateway's outside interface.
--
-- * 'cgState' - The current state of the customer gateway (@pending | available | deleting | deleted@ ).
--
-- * 'cgType' - The type of VPN connection the customer gateway supports (@ipsec.1@ ).
customerGateway
    :: Text -- ^ 'cgBGPASN'
    -> Text -- ^ 'cgCustomerGatewayId'
    -> Text -- ^ 'cgIPAddress'
    -> Text -- ^ 'cgState'
    -> Text -- ^ 'cgType'
    -> CustomerGateway
customerGateway pBGPASN_ pCustomerGatewayId_
  pIPAddress_ pState_ pType_
  = CustomerGateway'{_cgCertificateARN = Nothing,
                     _cgDeviceName = Nothing, _cgTags = Nothing,
                     _cgBGPASN = pBGPASN_,
                     _cgCustomerGatewayId = pCustomerGatewayId_,
                     _cgIPAddress = pIPAddress_, _cgState = pState_,
                     _cgType = pType_}

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
cgCertificateARN :: Lens' CustomerGateway (Maybe Text)
cgCertificateARN = lens _cgCertificateARN (\ s a -> s{_cgCertificateARN = a})

-- | The name of customer gateway device.
cgDeviceName :: Lens' CustomerGateway (Maybe Text)
cgDeviceName = lens _cgDeviceName (\ s a -> s{_cgDeviceName = a})

-- | Any tags assigned to the customer gateway.
cgTags :: Lens' CustomerGateway [Tag]
cgTags = lens _cgTags (\ s a -> s{_cgTags = a}) . _Default . _Coerce

-- | The customer gateway's Border Gateway Protocol (BGP) Autonomous System Number (ASN).
cgBGPASN :: Lens' CustomerGateway Text
cgBGPASN = lens _cgBGPASN (\ s a -> s{_cgBGPASN = a})

-- | The ID of the customer gateway.
cgCustomerGatewayId :: Lens' CustomerGateway Text
cgCustomerGatewayId = lens _cgCustomerGatewayId (\ s a -> s{_cgCustomerGatewayId = a})

-- | The Internet-routable IP address of the customer gateway's outside interface.
cgIPAddress :: Lens' CustomerGateway Text
cgIPAddress = lens _cgIPAddress (\ s a -> s{_cgIPAddress = a})

-- | The current state of the customer gateway (@pending | available | deleting | deleted@ ).
cgState :: Lens' CustomerGateway Text
cgState = lens _cgState (\ s a -> s{_cgState = a})

-- | The type of VPN connection the customer gateway supports (@ipsec.1@ ).
cgType :: Lens' CustomerGateway Text
cgType = lens _cgType (\ s a -> s{_cgType = a})

instance FromXML CustomerGateway where
        parseXML x
          = CustomerGateway' <$>
              (x .@? "certificateArn") <*> (x .@? "deviceName") <*>
                (x .@? "tagSet" .!@ mempty >>=
                   may (parseXMLList "item"))
                <*> (x .@ "bgpAsn")
                <*> (x .@ "customerGatewayId")
                <*> (x .@ "ipAddress")
                <*> (x .@ "state")
                <*> (x .@ "type")

instance Hashable CustomerGateway where

instance NFData CustomerGateway where
