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
-- Module      : Network.AWS.GameLift.ClaimGameServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This action is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Locates an available game server and temporarily reserves it to host gameplay and players. This action is called by a game client or client service (such as a matchmaker) to request hosting resources for a new game session. In response, GameLift FleetIQ searches for an available game server in the specified game server group, places the game server in "claimed" status for 60 seconds, and returns connection information back to the requester so that players can connect to the game server. 
--
-- There are two ways you can claim a game server. For the first option, you provide a game server group ID only, which prompts GameLift FleetIQ to search for an available game server in the specified group and claim it. With this option, GameLift FleetIQ attempts to consolidate gameplay on as few instances as possible to minimize hosting costs. For the second option, you request a specific game server by its ID. This option results in a less efficient claiming process because it does not take advantage of consolidation and may fail if the requested game server is unavailable. 
--
-- To claim a game server, identify a game server group and (optionally) a game server ID. If your game requires that game data be provided to the game server at the start of a game, such as a game map or player information, you can provide it in your claim request. 
--
-- When a game server is successfully claimed, connection information is returned. A claimed game server's utilization status remains AVAILABLE, while the claim status is set to CLAIMED for up to 60 seconds. This time period allows the game server to be prompted to update its status to UTILIZED (using 'UpdateGameServer' ). If the game server's status is not updated within 60 seconds, the game server reverts to unclaimed status and is available to be claimed by another request.
--
-- If you try to claim a specific game server, this request will fail in the following cases: (1) if the game server utilization status is UTILIZED, (2) if the game server claim status is CLAIMED, or (3) if the instance that the game server is running on is flagged as draining.
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
module Network.AWS.GameLift.ClaimGameServer
    (
    -- * Creating a Request
      claimGameServer
    , ClaimGameServer
    -- * Request Lenses
    , cgsGameServerData
    , cgsGameServerId
    , cgsGameServerGroupName

    -- * Destructuring the Response
    , claimGameServerResponse
    , ClaimGameServerResponse
    -- * Response Lenses
    , crsGameServer
    , crsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'claimGameServer' smart constructor.
data ClaimGameServer = ClaimGameServer'{_cgsGameServerData
                                        :: !(Maybe Text),
                                        _cgsGameServerId :: !(Maybe Text),
                                        _cgsGameServerGroupName :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClaimGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgsGameServerData' - A set of custom game server properties, formatted as a single string value, to be passed to the claimed game server. 
--
-- * 'cgsGameServerId' - A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
--
-- * 'cgsGameServerGroupName' - An identifier for the game server group. When claiming a specific game server, this is the game server group whether the game server is located. When requesting that GameLift FleetIQ locate an available game server, this is the game server group to search on. You can use either the 'GameServerGroup' name or ARN value.
claimGameServer
    :: Text -- ^ 'cgsGameServerGroupName'
    -> ClaimGameServer
claimGameServer pGameServerGroupName_
  = ClaimGameServer'{_cgsGameServerData = Nothing,
                     _cgsGameServerId = Nothing,
                     _cgsGameServerGroupName = pGameServerGroupName_}

-- | A set of custom game server properties, formatted as a single string value, to be passed to the claimed game server. 
cgsGameServerData :: Lens' ClaimGameServer (Maybe Text)
cgsGameServerData = lens _cgsGameServerData (\ s a -> s{_cgsGameServerData = a})

-- | A custom string that uniquely identifies the game server to claim. If this parameter is left empty, GameLift FleetIQ searches for an available game server in the specified game server group.
cgsGameServerId :: Lens' ClaimGameServer (Maybe Text)
cgsGameServerId = lens _cgsGameServerId (\ s a -> s{_cgsGameServerId = a})

-- | An identifier for the game server group. When claiming a specific game server, this is the game server group whether the game server is located. When requesting that GameLift FleetIQ locate an available game server, this is the game server group to search on. You can use either the 'GameServerGroup' name or ARN value.
cgsGameServerGroupName :: Lens' ClaimGameServer Text
cgsGameServerGroupName = lens _cgsGameServerGroupName (\ s a -> s{_cgsGameServerGroupName = a})

instance AWSRequest ClaimGameServer where
        type Rs ClaimGameServer = ClaimGameServerResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ClaimGameServerResponse' <$>
                   (x .?> "GameServer") <*> (pure (fromEnum s)))

instance Hashable ClaimGameServer where

instance NFData ClaimGameServer where

instance ToHeaders ClaimGameServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.ClaimGameServer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ClaimGameServer where
        toJSON ClaimGameServer'{..}
          = object
              (catMaybes
                 [("GameServerData" .=) <$> _cgsGameServerData,
                  ("GameServerId" .=) <$> _cgsGameServerId,
                  Just
                    ("GameServerGroupName" .= _cgsGameServerGroupName)])

instance ToPath ClaimGameServer where
        toPath = const "/"

instance ToQuery ClaimGameServer where
        toQuery = const mempty

-- | /See:/ 'claimGameServerResponse' smart constructor.
data ClaimGameServerResponse = ClaimGameServerResponse'{_crsGameServer
                                                        :: !(Maybe GameServer),
                                                        _crsResponseStatus ::
                                                        !Int}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'ClaimGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsGameServer' - Object that describes the newly claimed game server resource.
--
-- * 'crsResponseStatus' - -- | The response status code.
claimGameServerResponse
    :: Int -- ^ 'crsResponseStatus'
    -> ClaimGameServerResponse
claimGameServerResponse pResponseStatus_
  = ClaimGameServerResponse'{_crsGameServer = Nothing,
                             _crsResponseStatus = pResponseStatus_}

-- | Object that describes the newly claimed game server resource.
crsGameServer :: Lens' ClaimGameServerResponse (Maybe GameServer)
crsGameServer = lens _crsGameServer (\ s a -> s{_crsGameServer = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' ClaimGameServerResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData ClaimGameServerResponse where
