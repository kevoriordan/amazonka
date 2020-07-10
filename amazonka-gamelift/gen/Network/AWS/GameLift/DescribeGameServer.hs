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
-- Module      : Network.AWS.GameLift.DescribeGameServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This action is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Retrieves information for a game server resource. Information includes the game server statuses, health check info, and the instance the game server is running on. 
--
-- To retrieve game server information, specify the game server ID. If successful, the requested game server object is returned. 
--
-- __Learn more__ 
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gsg-intro.html GameLift FleetIQ Guide> 
--
-- __Related operations__ 
--
--     * 'RegisterGameServer' 
--
--     * 'ListGameServers' 
--
--     * 'ClaimGameServer' 
--
--     * 'DescribeGameServer' 
--
--     * 'UpdateGameServer' 
--
--     * 'DeregisterGameServer' 
--
--
--
module Network.AWS.GameLift.DescribeGameServer
    (
    -- * Creating a Request
      describeGameServer
    , DescribeGameServer
    -- * Request Lenses
    , dGameServerGroupName
    , dGameServerId

    -- * Destructuring the Response
    , describeGameServerResponse
    , DescribeGameServerResponse
    -- * Response Lenses
    , dscrbgmsrvrrsGameServer
    , dscrbgmsrvrrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGameServer' smart constructor.
data DescribeGameServer = DescribeGameServer'{_dGameServerGroupName
                                              :: !Text,
                                              _dGameServerId :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGameServerGroupName' - An identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'dGameServerId' - The identifier for the game server to be retrieved.
describeGameServer
    :: Text -- ^ 'dGameServerGroupName'
    -> Text -- ^ 'dGameServerId'
    -> DescribeGameServer
describeGameServer pGameServerGroupName_
  pGameServerId_
  = DescribeGameServer'{_dGameServerGroupName =
                          pGameServerGroupName_,
                        _dGameServerId = pGameServerId_}

-- | An identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
dGameServerGroupName :: Lens' DescribeGameServer Text
dGameServerGroupName = lens _dGameServerGroupName (\ s a -> s{_dGameServerGroupName = a})

-- | The identifier for the game server to be retrieved.
dGameServerId :: Lens' DescribeGameServer Text
dGameServerId = lens _dGameServerId (\ s a -> s{_dGameServerId = a})

instance AWSRequest DescribeGameServer where
        type Rs DescribeGameServer =
             DescribeGameServerResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGameServerResponse' <$>
                   (x .?> "GameServer") <*> (pure (fromEnum s)))

instance Hashable DescribeGameServer where

instance NFData DescribeGameServer where

instance ToHeaders DescribeGameServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeGameServer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGameServer where
        toJSON DescribeGameServer'{..}
          = object
              (catMaybes
                 [Just
                    ("GameServerGroupName" .= _dGameServerGroupName),
                  Just ("GameServerId" .= _dGameServerId)])

instance ToPath DescribeGameServer where
        toPath = const "/"

instance ToQuery DescribeGameServer where
        toQuery = const mempty

-- | /See:/ 'describeGameServerResponse' smart constructor.
data DescribeGameServerResponse = DescribeGameServerResponse'{_dscrbgmsrvrrsGameServer
                                                              ::
                                                              !(Maybe
                                                                  GameServer),
                                                              _dscrbgmsrvrrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbgmsrvrrsGameServer' - Object that describes the requested game server resource.
--
-- * 'dscrbgmsrvrrsResponseStatus' - -- | The response status code.
describeGameServerResponse
    :: Int -- ^ 'dscrbgmsrvrrsResponseStatus'
    -> DescribeGameServerResponse
describeGameServerResponse pResponseStatus_
  = DescribeGameServerResponse'{_dscrbgmsrvrrsGameServer
                                  = Nothing,
                                _dscrbgmsrvrrsResponseStatus = pResponseStatus_}

-- | Object that describes the requested game server resource.
dscrbgmsrvrrsGameServer :: Lens' DescribeGameServerResponse (Maybe GameServer)
dscrbgmsrvrrsGameServer = lens _dscrbgmsrvrrsGameServer (\ s a -> s{_dscrbgmsrvrrsGameServer = a})

-- | -- | The response status code.
dscrbgmsrvrrsResponseStatus :: Lens' DescribeGameServerResponse Int
dscrbgmsrvrrsResponseStatus = lens _dscrbgmsrvrrsResponseStatus (\ s a -> s{_dscrbgmsrvrrsResponseStatus = a})

instance NFData DescribeGameServerResponse where
