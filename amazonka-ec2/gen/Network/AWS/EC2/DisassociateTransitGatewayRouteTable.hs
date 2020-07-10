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
-- Module      : Network.AWS.EC2.DisassociateTransitGatewayRouteTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a resource attachment from a transit gateway route table.
--
--
module Network.AWS.EC2.DisassociateTransitGatewayRouteTable
    (
    -- * Creating a Request
      disassociateTransitGatewayRouteTable
    , DisassociateTransitGatewayRouteTable
    -- * Request Lenses
    , dssscttrnstgtwyrttblDryRun
    , dssscttrnstgtwyrttblTransitGatewayRouteTableId
    , dssscttrnstgtwyrttblTransitGatewayAttachmentId

    -- * Destructuring the Response
    , disassociateTransitGatewayRouteTableResponse
    , DisassociateTransitGatewayRouteTableResponse
    -- * Response Lenses
    , dssscttrnstgtwyrttblrsAssociation
    , dssscttrnstgtwyrttblrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateTransitGatewayRouteTable' smart constructor.
data DisassociateTransitGatewayRouteTable = DisassociateTransitGatewayRouteTable'{_dssscttrnstgtwyrttblDryRun
                                                                                  ::
                                                                                  !(Maybe
                                                                                      Bool),
                                                                                  _dssscttrnstgtwyrttblTransitGatewayRouteTableId
                                                                                  ::
                                                                                  !Text,
                                                                                  _dssscttrnstgtwyrttblTransitGatewayAttachmentId
                                                                                  ::
                                                                                  !Text}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'DisassociateTransitGatewayRouteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssscttrnstgtwyrttblDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dssscttrnstgtwyrttblTransitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- * 'dssscttrnstgtwyrttblTransitGatewayAttachmentId' - The ID of the attachment.
disassociateTransitGatewayRouteTable
    :: Text -- ^ 'dssscttrnstgtwyrttblTransitGatewayRouteTableId'
    -> Text -- ^ 'dssscttrnstgtwyrttblTransitGatewayAttachmentId'
    -> DisassociateTransitGatewayRouteTable
disassociateTransitGatewayRouteTable
  pTransitGatewayRouteTableId_
  pTransitGatewayAttachmentId_
  = DisassociateTransitGatewayRouteTable'{_dssscttrnstgtwyrttblDryRun
                                            = Nothing,
                                          _dssscttrnstgtwyrttblTransitGatewayRouteTableId
                                            = pTransitGatewayRouteTableId_,
                                          _dssscttrnstgtwyrttblTransitGatewayAttachmentId
                                            = pTransitGatewayAttachmentId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dssscttrnstgtwyrttblDryRun :: Lens' DisassociateTransitGatewayRouteTable (Maybe Bool)
dssscttrnstgtwyrttblDryRun = lens _dssscttrnstgtwyrttblDryRun (\ s a -> s{_dssscttrnstgtwyrttblDryRun = a})

-- | The ID of the transit gateway route table.
dssscttrnstgtwyrttblTransitGatewayRouteTableId :: Lens' DisassociateTransitGatewayRouteTable Text
dssscttrnstgtwyrttblTransitGatewayRouteTableId = lens _dssscttrnstgtwyrttblTransitGatewayRouteTableId (\ s a -> s{_dssscttrnstgtwyrttblTransitGatewayRouteTableId = a})

-- | The ID of the attachment.
dssscttrnstgtwyrttblTransitGatewayAttachmentId :: Lens' DisassociateTransitGatewayRouteTable Text
dssscttrnstgtwyrttblTransitGatewayAttachmentId = lens _dssscttrnstgtwyrttblTransitGatewayAttachmentId (\ s a -> s{_dssscttrnstgtwyrttblTransitGatewayAttachmentId = a})

instance AWSRequest
           DisassociateTransitGatewayRouteTable
         where
        type Rs DisassociateTransitGatewayRouteTable =
             DisassociateTransitGatewayRouteTableResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateTransitGatewayRouteTableResponse' <$>
                   (x .@? "association") <*> (pure (fromEnum s)))

instance Hashable
           DisassociateTransitGatewayRouteTable
         where

instance NFData DisassociateTransitGatewayRouteTable
         where

instance ToHeaders
           DisassociateTransitGatewayRouteTable
         where
        toHeaders = const mempty

instance ToPath DisassociateTransitGatewayRouteTable
         where
        toPath = const "/"

instance ToQuery DisassociateTransitGatewayRouteTable
         where
        toQuery DisassociateTransitGatewayRouteTable'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateTransitGatewayRouteTable" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dssscttrnstgtwyrttblDryRun,
               "TransitGatewayRouteTableId" =:
                 _dssscttrnstgtwyrttblTransitGatewayRouteTableId,
               "TransitGatewayAttachmentId" =:
                 _dssscttrnstgtwyrttblTransitGatewayAttachmentId]

-- | /See:/ 'disassociateTransitGatewayRouteTableResponse' smart constructor.
data DisassociateTransitGatewayRouteTableResponse = DisassociateTransitGatewayRouteTableResponse'{_dssscttrnstgtwyrttblrsAssociation
                                                                                                  ::
                                                                                                  !(Maybe
                                                                                                      TransitGatewayAssociation),
                                                                                                  _dssscttrnstgtwyrttblrsResponseStatus
                                                                                                  ::
                                                                                                  !Int}
                                                      deriving (Eq, Read, Show,
                                                                Data, Typeable,
                                                                Generic)

-- | Creates a value of 'DisassociateTransitGatewayRouteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssscttrnstgtwyrttblrsAssociation' - Information about the association.
--
-- * 'dssscttrnstgtwyrttblrsResponseStatus' - -- | The response status code.
disassociateTransitGatewayRouteTableResponse
    :: Int -- ^ 'dssscttrnstgtwyrttblrsResponseStatus'
    -> DisassociateTransitGatewayRouteTableResponse
disassociateTransitGatewayRouteTableResponse
  pResponseStatus_
  = DisassociateTransitGatewayRouteTableResponse'{_dssscttrnstgtwyrttblrsAssociation
                                                    = Nothing,
                                                  _dssscttrnstgtwyrttblrsResponseStatus
                                                    = pResponseStatus_}

-- | Information about the association.
dssscttrnstgtwyrttblrsAssociation :: Lens' DisassociateTransitGatewayRouteTableResponse (Maybe TransitGatewayAssociation)
dssscttrnstgtwyrttblrsAssociation = lens _dssscttrnstgtwyrttblrsAssociation (\ s a -> s{_dssscttrnstgtwyrttblrsAssociation = a})

-- | -- | The response status code.
dssscttrnstgtwyrttblrsResponseStatus :: Lens' DisassociateTransitGatewayRouteTableResponse Int
dssscttrnstgtwyrttblrsResponseStatus = lens _dssscttrnstgtwyrttblrsResponseStatus (\ s a -> s{_dssscttrnstgtwyrttblrsResponseStatus = a})

instance NFData
           DisassociateTransitGatewayRouteTableResponse
         where
