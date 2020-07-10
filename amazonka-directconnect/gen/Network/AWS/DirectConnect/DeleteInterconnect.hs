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
-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified interconnect.
--
--
module Network.AWS.DirectConnect.DeleteInterconnect
    (
    -- * Creating a Request
      deleteInterconnect
    , DeleteInterconnect
    -- * Request Lenses
    , dInterconnectId

    -- * Destructuring the Response
    , deleteInterconnectResponse
    , DeleteInterconnectResponse
    -- * Response Lenses
    , drsInterconnectState
    , drsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInterconnect' smart constructor.
newtype DeleteInterconnect = DeleteInterconnect'{_dInterconnectId
                                                 :: Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DeleteInterconnect' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInterconnectId' - The ID of the interconnect.
deleteInterconnect
    :: Text -- ^ 'dInterconnectId'
    -> DeleteInterconnect
deleteInterconnect pInterconnectId_
  = DeleteInterconnect'{_dInterconnectId =
                          pInterconnectId_}

-- | The ID of the interconnect.
dInterconnectId :: Lens' DeleteInterconnect Text
dInterconnectId = lens _dInterconnectId (\ s a -> s{_dInterconnectId = a})

instance AWSRequest DeleteInterconnect where
        type Rs DeleteInterconnect =
             DeleteInterconnectResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DeleteInterconnectResponse' <$>
                   (x .?> "interconnectState") <*> (pure (fromEnum s)))

instance Hashable DeleteInterconnect where

instance NFData DeleteInterconnect where

instance ToHeaders DeleteInterconnect where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DeleteInterconnect" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteInterconnect where
        toJSON DeleteInterconnect'{..}
          = object
              (catMaybes
                 [Just ("interconnectId" .= _dInterconnectId)])

instance ToPath DeleteInterconnect where
        toPath = const "/"

instance ToQuery DeleteInterconnect where
        toQuery = const mempty

-- | /See:/ 'deleteInterconnectResponse' smart constructor.
data DeleteInterconnectResponse = DeleteInterconnectResponse'{_drsInterconnectState
                                                              ::
                                                              !(Maybe
                                                                  InterconnectState),
                                                              _drsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteInterconnectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsInterconnectState' - The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteInterconnectResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteInterconnectResponse
deleteInterconnectResponse pResponseStatus_
  = DeleteInterconnectResponse'{_drsInterconnectState =
                                  Nothing,
                                _drsResponseStatus = pResponseStatus_}

-- | The state of the interconnect. The following are the possible values:     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.     * @pending@ : The interconnect is approved, and is being initialized.     * @available@ : The network link is up, and the interconnect is ready for use.     * @down@ : The network link is down.     * @deleting@ : The interconnect is being deleted.     * @deleted@ : The interconnect is deleted.     * @unknown@ : The state of the interconnect is not available.
drsInterconnectState :: Lens' DeleteInterconnectResponse (Maybe InterconnectState)
drsInterconnectState = lens _drsInterconnectState (\ s a -> s{_drsInterconnectState = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteInterconnectResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteInterconnectResponse where
