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
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.
--
--
module Network.AWS.EC2.DeleteNatGateway
    (
    -- * Creating a Request
      deleteNatGateway
    , DeleteNatGateway
    -- * Request Lenses
    , dltntgtwyDryRun
    , dltntgtwyNatGatewayId

    -- * Destructuring the Response
    , deleteNatGatewayResponse
    , DeleteNatGatewayResponse
    -- * Response Lenses
    , dltntgtwyrsNatGatewayId
    , dltntgtwyrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'{_dltntgtwyDryRun
                                          :: !(Maybe Bool),
                                          _dltntgtwyNatGatewayId :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteNatGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltntgtwyDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltntgtwyNatGatewayId' - The ID of the NAT gateway.
deleteNatGateway
    :: Text -- ^ 'dltntgtwyNatGatewayId'
    -> DeleteNatGateway
deleteNatGateway pNatGatewayId_
  = DeleteNatGateway'{_dltntgtwyDryRun = Nothing,
                      _dltntgtwyNatGatewayId = pNatGatewayId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltntgtwyDryRun :: Lens' DeleteNatGateway (Maybe Bool)
dltntgtwyDryRun = lens _dltntgtwyDryRun (\ s a -> s{_dltntgtwyDryRun = a})

-- | The ID of the NAT gateway.
dltntgtwyNatGatewayId :: Lens' DeleteNatGateway Text
dltntgtwyNatGatewayId = lens _dltntgtwyNatGatewayId (\ s a -> s{_dltntgtwyNatGatewayId = a})

instance AWSRequest DeleteNatGateway where
        type Rs DeleteNatGateway = DeleteNatGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteNatGatewayResponse' <$>
                   (x .@? "natGatewayId") <*> (pure (fromEnum s)))

instance Hashable DeleteNatGateway where

instance NFData DeleteNatGateway where

instance ToHeaders DeleteNatGateway where
        toHeaders = const mempty

instance ToPath DeleteNatGateway where
        toPath = const "/"

instance ToQuery DeleteNatGateway where
        toQuery DeleteNatGateway'{..}
          = mconcat
              ["Action" =: ("DeleteNatGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dltntgtwyDryRun,
               "NatGatewayId" =: _dltntgtwyNatGatewayId]

-- | /See:/ 'deleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'{_dltntgtwyrsNatGatewayId
                                                          :: !(Maybe Text),
                                                          _dltntgtwyrsResponseStatus
                                                          :: !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DeleteNatGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltntgtwyrsNatGatewayId' - The ID of the NAT gateway.
--
-- * 'dltntgtwyrsResponseStatus' - -- | The response status code.
deleteNatGatewayResponse
    :: Int -- ^ 'dltntgtwyrsResponseStatus'
    -> DeleteNatGatewayResponse
deleteNatGatewayResponse pResponseStatus_
  = DeleteNatGatewayResponse'{_dltntgtwyrsNatGatewayId
                                = Nothing,
                              _dltntgtwyrsResponseStatus = pResponseStatus_}

-- | The ID of the NAT gateway.
dltntgtwyrsNatGatewayId :: Lens' DeleteNatGatewayResponse (Maybe Text)
dltntgtwyrsNatGatewayId = lens _dltntgtwyrsNatGatewayId (\ s a -> s{_dltntgtwyrsNatGatewayId = a})

-- | -- | The response status code.
dltntgtwyrsResponseStatus :: Lens' DeleteNatGatewayResponse Int
dltntgtwyrsResponseStatus = lens _dltntgtwyrsResponseStatus (\ s a -> s{_dltntgtwyrsResponseStatus = a})

instance NFData DeleteNatGatewayResponse where
