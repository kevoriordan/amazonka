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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Direct Connect gateway. You must first delete all virtual interfaces that are attached to the Direct Connect gateway and disassociate all virtual private gateways associated with the Direct Connect gateway.
--
--
module Network.AWS.DirectConnect.DeleteDirectConnectGateway
    (
    -- * Creating a Request
      deleteDirectConnectGateway
    , DeleteDirectConnectGateway
    -- * Request Lenses
    , dltdrctcnnctgtwyDirectConnectGatewayId

    -- * Destructuring the Response
    , deleteDirectConnectGatewayResponse
    , DeleteDirectConnectGatewayResponse
    -- * Response Lenses
    , dltdrctcnnctgtwyrsDirectConnectGateway
    , dltdrctcnnctgtwyrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDirectConnectGateway' smart constructor.
newtype DeleteDirectConnectGateway = DeleteDirectConnectGateway'{_dltdrctcnnctgtwyDirectConnectGatewayId
                                                                 :: Text}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'DeleteDirectConnectGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltdrctcnnctgtwyDirectConnectGatewayId' - The ID of the Direct Connect gateway.
deleteDirectConnectGateway
    :: Text -- ^ 'dltdrctcnnctgtwyDirectConnectGatewayId'
    -> DeleteDirectConnectGateway
deleteDirectConnectGateway pDirectConnectGatewayId_
  = DeleteDirectConnectGateway'{_dltdrctcnnctgtwyDirectConnectGatewayId
                                  = pDirectConnectGatewayId_}

-- | The ID of the Direct Connect gateway.
dltdrctcnnctgtwyDirectConnectGatewayId :: Lens' DeleteDirectConnectGateway Text
dltdrctcnnctgtwyDirectConnectGatewayId = lens _dltdrctcnnctgtwyDirectConnectGatewayId (\ s a -> s{_dltdrctcnnctgtwyDirectConnectGatewayId = a})

instance AWSRequest DeleteDirectConnectGateway where
        type Rs DeleteDirectConnectGateway =
             DeleteDirectConnectGatewayResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDirectConnectGatewayResponse' <$>
                   (x .?> "directConnectGateway") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteDirectConnectGateway where

instance NFData DeleteDirectConnectGateway where

instance ToHeaders DeleteDirectConnectGateway where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteDirectConnectGateway" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDirectConnectGateway where
        toJSON DeleteDirectConnectGateway'{..}
          = object
              (catMaybes
                 [Just
                    ("directConnectGatewayId" .=
                       _dltdrctcnnctgtwyDirectConnectGatewayId)])

instance ToPath DeleteDirectConnectGateway where
        toPath = const "/"

instance ToQuery DeleteDirectConnectGateway where
        toQuery = const mempty

-- | /See:/ 'deleteDirectConnectGatewayResponse' smart constructor.
data DeleteDirectConnectGatewayResponse = DeleteDirectConnectGatewayResponse'{_dltdrctcnnctgtwyrsDirectConnectGateway
                                                                              ::
                                                                              !(Maybe
                                                                                  DirectConnectGateway),
                                                                              _dltdrctcnnctgtwyrsResponseStatus
                                                                              ::
                                                                              !Int}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'DeleteDirectConnectGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltdrctcnnctgtwyrsDirectConnectGateway' - The Direct Connect gateway.
--
-- * 'dltdrctcnnctgtwyrsResponseStatus' - -- | The response status code.
deleteDirectConnectGatewayResponse
    :: Int -- ^ 'dltdrctcnnctgtwyrsResponseStatus'
    -> DeleteDirectConnectGatewayResponse
deleteDirectConnectGatewayResponse pResponseStatus_
  = DeleteDirectConnectGatewayResponse'{_dltdrctcnnctgtwyrsDirectConnectGateway
                                          = Nothing,
                                        _dltdrctcnnctgtwyrsResponseStatus =
                                          pResponseStatus_}

-- | The Direct Connect gateway.
dltdrctcnnctgtwyrsDirectConnectGateway :: Lens' DeleteDirectConnectGatewayResponse (Maybe DirectConnectGateway)
dltdrctcnnctgtwyrsDirectConnectGateway = lens _dltdrctcnnctgtwyrsDirectConnectGateway (\ s a -> s{_dltdrctcnnctgtwyrsDirectConnectGateway = a})

-- | -- | The response status code.
dltdrctcnnctgtwyrsResponseStatus :: Lens' DeleteDirectConnectGatewayResponse Int
dltdrctcnnctgtwyrsResponseStatus = lens _dltdrctcnnctgtwyrsResponseStatus (\ s a -> s{_dltdrctcnnctgtwyrsResponseStatus = a})

instance NFData DeleteDirectConnectGatewayResponse
         where
