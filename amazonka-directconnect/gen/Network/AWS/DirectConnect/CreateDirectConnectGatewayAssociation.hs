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
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a Direct Connect gateway and a virtual private gateway. The virtual private gateway must be attached to a VPC and must not be associated with another Direct Connect gateway.
--
--
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
    (
    -- * Creating a Request
      createDirectConnectGatewayAssociation
    , CreateDirectConnectGatewayAssociation
    -- * Request Lenses
    , cdcgaVirtualGatewayId
    , cdcgaAddAllowedPrefixesToDirectConnectGateway
    , cdcgaGatewayId
    , cdcgaDirectConnectGatewayId

    -- * Destructuring the Response
    , createDirectConnectGatewayAssociationResponse
    , CreateDirectConnectGatewayAssociationResponse
    -- * Response Lenses
    , cdcgarsDirectConnectGatewayAssociation
    , cdcgarsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'{_cdcgaVirtualGatewayId
                                                                                    ::
                                                                                    !(Maybe
                                                                                        Text),
                                                                                    _cdcgaAddAllowedPrefixesToDirectConnectGateway
                                                                                    ::
                                                                                    !(Maybe
                                                                                        [RouteFilterPrefix]),
                                                                                    _cdcgaGatewayId
                                                                                    ::
                                                                                    !(Maybe
                                                                                        Text),
                                                                                    _cdcgaDirectConnectGatewayId
                                                                                    ::
                                                                                    !Text}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'CreateDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgaVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'cdcgaAddAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway This parameter is required when you create an association to a transit gateway. For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- * 'cdcgaGatewayId' - The ID of the virtual private gateway or transit gateway.
--
-- * 'cdcgaDirectConnectGatewayId' - The ID of the Direct Connect gateway.
createDirectConnectGatewayAssociation
    :: Text -- ^ 'cdcgaDirectConnectGatewayId'
    -> CreateDirectConnectGatewayAssociation
createDirectConnectGatewayAssociation
  pDirectConnectGatewayId_
  = CreateDirectConnectGatewayAssociation'{_cdcgaVirtualGatewayId
                                             = Nothing,
                                           _cdcgaAddAllowedPrefixesToDirectConnectGateway
                                             = Nothing,
                                           _cdcgaGatewayId = Nothing,
                                           _cdcgaDirectConnectGatewayId =
                                             pDirectConnectGatewayId_}

-- | The ID of the virtual private gateway.
cdcgaVirtualGatewayId :: Lens' CreateDirectConnectGatewayAssociation (Maybe Text)
cdcgaVirtualGatewayId = lens _cdcgaVirtualGatewayId (\ s a -> s{_cdcgaVirtualGatewayId = a})

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway This parameter is required when you create an association to a transit gateway. For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
cdcgaAddAllowedPrefixesToDirectConnectGateway :: Lens' CreateDirectConnectGatewayAssociation [RouteFilterPrefix]
cdcgaAddAllowedPrefixesToDirectConnectGateway = lens _cdcgaAddAllowedPrefixesToDirectConnectGateway (\ s a -> s{_cdcgaAddAllowedPrefixesToDirectConnectGateway = a}) . _Default . _Coerce

-- | The ID of the virtual private gateway or transit gateway.
cdcgaGatewayId :: Lens' CreateDirectConnectGatewayAssociation (Maybe Text)
cdcgaGatewayId = lens _cdcgaGatewayId (\ s a -> s{_cdcgaGatewayId = a})

-- | The ID of the Direct Connect gateway.
cdcgaDirectConnectGatewayId :: Lens' CreateDirectConnectGatewayAssociation Text
cdcgaDirectConnectGatewayId = lens _cdcgaDirectConnectGatewayId (\ s a -> s{_cdcgaDirectConnectGatewayId = a})

instance AWSRequest
           CreateDirectConnectGatewayAssociation
         where
        type Rs CreateDirectConnectGatewayAssociation =
             CreateDirectConnectGatewayAssociationResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectConnectGatewayAssociationResponse' <$>
                   (x .?> "directConnectGatewayAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable
           CreateDirectConnectGatewayAssociation
         where

instance NFData CreateDirectConnectGatewayAssociation
         where

instance ToHeaders
           CreateDirectConnectGatewayAssociation
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateDirectConnectGatewayAssociation"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDirectConnectGatewayAssociation
         where
        toJSON CreateDirectConnectGatewayAssociation'{..}
          = object
              (catMaybes
                 [("virtualGatewayId" .=) <$> _cdcgaVirtualGatewayId,
                  ("addAllowedPrefixesToDirectConnectGateway" .=) <$>
                    _cdcgaAddAllowedPrefixesToDirectConnectGateway,
                  ("gatewayId" .=) <$> _cdcgaGatewayId,
                  Just
                    ("directConnectGatewayId" .=
                       _cdcgaDirectConnectGatewayId)])

instance ToPath CreateDirectConnectGatewayAssociation
         where
        toPath = const "/"

instance ToQuery
           CreateDirectConnectGatewayAssociation
         where
        toQuery = const mempty

-- | /See:/ 'createDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'{_cdcgarsDirectConnectGatewayAssociation
                                                                                                    ::
                                                                                                    !(Maybe
                                                                                                        DirectConnectGatewayAssociation),
                                                                                                    _cdcgarsResponseStatus
                                                                                                    ::
                                                                                                    !Int}
                                                       deriving (Eq, Read, Show,
                                                                 Data, Typeable,
                                                                 Generic)

-- | Creates a value of 'CreateDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcgarsDirectConnectGatewayAssociation' - The association to be created.
--
-- * 'cdcgarsResponseStatus' - -- | The response status code.
createDirectConnectGatewayAssociationResponse
    :: Int -- ^ 'cdcgarsResponseStatus'
    -> CreateDirectConnectGatewayAssociationResponse
createDirectConnectGatewayAssociationResponse
  pResponseStatus_
  = CreateDirectConnectGatewayAssociationResponse'{_cdcgarsDirectConnectGatewayAssociation
                                                     = Nothing,
                                                   _cdcgarsResponseStatus =
                                                     pResponseStatus_}

-- | The association to be created.
cdcgarsDirectConnectGatewayAssociation :: Lens' CreateDirectConnectGatewayAssociationResponse (Maybe DirectConnectGatewayAssociation)
cdcgarsDirectConnectGatewayAssociation = lens _cdcgarsDirectConnectGatewayAssociation (\ s a -> s{_cdcgarsDirectConnectGatewayAssociation = a})

-- | -- | The response status code.
cdcgarsResponseStatus :: Lens' CreateDirectConnectGatewayAssociationResponse Int
cdcgarsResponseStatus = lens _cdcgarsResponseStatus (\ s a -> s{_cdcgarsResponseStatus = a})

instance NFData
           CreateDirectConnectGatewayAssociationResponse
         where
