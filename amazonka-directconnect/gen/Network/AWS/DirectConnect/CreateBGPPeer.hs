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
-- Module      : Network.AWS.DirectConnect.CreateBGPPeer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a BGP peer on the specified virtual interface.
--
--
-- You must create a BGP peer for the corresponding address family (IPv4/IPv6) in order to access AWS resources that also use that address family.
--
-- If logical redundancy is not supported by the connection, interconnect, or LAG, the BGP peer cannot be in the same address family as an existing BGP peer on the virtual interface.
--
-- When creating a IPv6 BGP peer, omit the Amazon address and customer address. IPv6 addresses are automatically assigned from the Amazon pool of IPv6 addresses; you cannot specify custom IPv6 addresses.
--
-- For a public virtual interface, the Autonomous System Number (ASN) must be private or already whitelisted for the virtual interface.
--
module Network.AWS.DirectConnect.CreateBGPPeer
    (
    -- * Creating a Request
      createBGPPeer
    , CreateBGPPeer
    -- * Request Lenses
    , cbpNewBGPPeer
    , cbpVirtualInterfaceId

    -- * Destructuring the Response
    , createBGPPeerResponse
    , CreateBGPPeerResponse
    -- * Response Lenses
    , cbprsVirtualInterface
    , cbprsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBGPPeer' smart constructor.
data CreateBGPPeer =
  CreateBGPPeer'
    { _cbpNewBGPPeer         :: !(Maybe NewBGPPeer)
    , _cbpVirtualInterfaceId :: !(Maybe Text)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBGPPeer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbpNewBGPPeer' - Information about the BGP peer.
--
-- * 'cbpVirtualInterfaceId' - The ID of the virtual interface.
createBGPPeer
    :: CreateBGPPeer
createBGPPeer =
  CreateBGPPeer' {_cbpNewBGPPeer = Nothing, _cbpVirtualInterfaceId = Nothing}


-- | Information about the BGP peer.
cbpNewBGPPeer :: Lens' CreateBGPPeer (Maybe NewBGPPeer)
cbpNewBGPPeer = lens _cbpNewBGPPeer (\ s a -> s{_cbpNewBGPPeer = a})

-- | The ID of the virtual interface.
cbpVirtualInterfaceId :: Lens' CreateBGPPeer (Maybe Text)
cbpVirtualInterfaceId = lens _cbpVirtualInterfaceId (\ s a -> s{_cbpVirtualInterfaceId = a})

instance AWSRequest CreateBGPPeer where
        type Rs CreateBGPPeer = CreateBGPPeerResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 CreateBGPPeerResponse' <$>
                   (x .?> "virtualInterface") <*> (pure (fromEnum s)))

instance Hashable CreateBGPPeer where

instance NFData CreateBGPPeer where

instance ToHeaders CreateBGPPeer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreateBGPPeer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBGPPeer where
        toJSON CreateBGPPeer'{..}
          = object
              (catMaybes
                 [("newBGPPeer" .=) <$> _cbpNewBGPPeer,
                  ("virtualInterfaceId" .=) <$>
                    _cbpVirtualInterfaceId])

instance ToPath CreateBGPPeer where
        toPath = const "/"

instance ToQuery CreateBGPPeer where
        toQuery = const mempty

-- | /See:/ 'createBGPPeerResponse' smart constructor.
data CreateBGPPeerResponse =
  CreateBGPPeerResponse'
    { _cbprsVirtualInterface :: !(Maybe VirtualInterface)
    , _cbprsResponseStatus   :: !Int
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBGPPeerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbprsVirtualInterface' - The virtual interface.
--
-- * 'cbprsResponseStatus' - -- | The response status code.
createBGPPeerResponse
    :: Int -- ^ 'cbprsResponseStatus'
    -> CreateBGPPeerResponse
createBGPPeerResponse pResponseStatus_ =
  CreateBGPPeerResponse'
    {_cbprsVirtualInterface = Nothing, _cbprsResponseStatus = pResponseStatus_}


-- | The virtual interface.
cbprsVirtualInterface :: Lens' CreateBGPPeerResponse (Maybe VirtualInterface)
cbprsVirtualInterface = lens _cbprsVirtualInterface (\ s a -> s{_cbprsVirtualInterface = a})

-- | -- | The response status code.
cbprsResponseStatus :: Lens' CreateBGPPeerResponse Int
cbprsResponseStatus = lens _cbprsResponseStatus (\ s a -> s{_cbprsResponseStatus = a})

instance NFData CreateBGPPeerResponse where
