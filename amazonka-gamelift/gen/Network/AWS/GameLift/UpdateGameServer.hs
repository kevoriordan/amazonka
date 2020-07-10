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
-- Module      : Network.AWS.GameLift.UpdateGameServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This action is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Updates information about a registered game server. This action is called by a game server process that is running on an instance in a game server group. There are three reasons to update game server information: (1) to change the utilization status of the game server, (2) to report game server health status, and (3) to change game server metadata. A registered game server should regularly report health and should update utilization status when it is supporting gameplay so that GameLift FleetIQ can accurately track game server availability. You can make all three types of updates in the same request.
--
--     * To update the game server's utilization status, identify the game server and game server group and specify the current utilization status. Use this status to identify when game servers are currently hosting games and when they are available to be claimed. 
--
--     * To report health status, identify the game server and game server group and set health check to HEALTHY. If a game server does not report health status for a certain length of time, the game server is no longer considered healthy and will be eventually de-registered from the game server group to avoid affecting utilization metrics. The best practice is to report health every 60 seconds.
--
--     * To change game server metadata, provide updated game server data and custom sort key values.
--
--
--
-- Once a game server is successfully updated, the relevant statuses and timestamps are updated.
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
module Network.AWS.GameLift.UpdateGameServer
    (
    -- * Creating a Request
      updateGameServer
    , UpdateGameServer
    -- * Request Lenses
    , ugsHealthCheck
    , ugsGameServerData
    , ugsCustomSortKey
    , ugsUtilizationStatus
    , ugsGameServerGroupName
    , ugsGameServerId

    -- * Destructuring the Response
    , updateGameServerResponse
    , UpdateGameServerResponse
    -- * Response Lenses
    , ursGameServer
    , ursResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGameServer' smart constructor.
data UpdateGameServer = UpdateGameServer'{_ugsHealthCheck
                                          :: !(Maybe GameServerHealthCheck),
                                          _ugsGameServerData :: !(Maybe Text),
                                          _ugsCustomSortKey :: !(Maybe Text),
                                          _ugsUtilizationStatus ::
                                          !(Maybe GameServerUtilizationStatus),
                                          _ugsGameServerGroupName :: !Text,
                                          _ugsGameServerId :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsHealthCheck' - Indicates health status of the game server. An update that explicitly includes this parameter updates the game server's /LastHealthCheckTime/ time stamp. 
--
-- * 'ugsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on a game servers using 'DescribeGameServer' or 'ClaimGameServer' . 
--
-- * 'ugsCustomSortKey' - A game server tag that can be used to request sorted lists of game servers using 'ListGameServers' . Custom sort keys are developer-defined based on how you want to organize the retrieved game server information.
--
-- * 'ugsUtilizationStatus' - Indicates whether the game server is available or is currently hosting gameplay.
--
-- * 'ugsGameServerGroupName' - An identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- * 'ugsGameServerId' - The identifier for the game server to be updated.
updateGameServer
    :: Text -- ^ 'ugsGameServerGroupName'
    -> Text -- ^ 'ugsGameServerId'
    -> UpdateGameServer
updateGameServer pGameServerGroupName_ pGameServerId_
  = UpdateGameServer'{_ugsHealthCheck = Nothing,
                      _ugsGameServerData = Nothing,
                      _ugsCustomSortKey = Nothing,
                      _ugsUtilizationStatus = Nothing,
                      _ugsGameServerGroupName = pGameServerGroupName_,
                      _ugsGameServerId = pGameServerId_}

-- | Indicates health status of the game server. An update that explicitly includes this parameter updates the game server's /LastHealthCheckTime/ time stamp. 
ugsHealthCheck :: Lens' UpdateGameServer (Maybe GameServerHealthCheck)
ugsHealthCheck = lens _ugsHealthCheck (\ s a -> s{_ugsHealthCheck = a})

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on a game servers using 'DescribeGameServer' or 'ClaimGameServer' . 
ugsGameServerData :: Lens' UpdateGameServer (Maybe Text)
ugsGameServerData = lens _ugsGameServerData (\ s a -> s{_ugsGameServerData = a})

-- | A game server tag that can be used to request sorted lists of game servers using 'ListGameServers' . Custom sort keys are developer-defined based on how you want to organize the retrieved game server information.
ugsCustomSortKey :: Lens' UpdateGameServer (Maybe Text)
ugsCustomSortKey = lens _ugsCustomSortKey (\ s a -> s{_ugsCustomSortKey = a})

-- | Indicates whether the game server is available or is currently hosting gameplay.
ugsUtilizationStatus :: Lens' UpdateGameServer (Maybe GameServerUtilizationStatus)
ugsUtilizationStatus = lens _ugsUtilizationStatus (\ s a -> s{_ugsUtilizationStatus = a})

-- | An identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
ugsGameServerGroupName :: Lens' UpdateGameServer Text
ugsGameServerGroupName = lens _ugsGameServerGroupName (\ s a -> s{_ugsGameServerGroupName = a})

-- | The identifier for the game server to be updated.
ugsGameServerId :: Lens' UpdateGameServer Text
ugsGameServerId = lens _ugsGameServerId (\ s a -> s{_ugsGameServerId = a})

instance AWSRequest UpdateGameServer where
        type Rs UpdateGameServer = UpdateGameServerResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGameServerResponse' <$>
                   (x .?> "GameServer") <*> (pure (fromEnum s)))

instance Hashable UpdateGameServer where

instance NFData UpdateGameServer where

instance ToHeaders UpdateGameServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateGameServer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGameServer where
        toJSON UpdateGameServer'{..}
          = object
              (catMaybes
                 [("HealthCheck" .=) <$> _ugsHealthCheck,
                  ("GameServerData" .=) <$> _ugsGameServerData,
                  ("CustomSortKey" .=) <$> _ugsCustomSortKey,
                  ("UtilizationStatus" .=) <$> _ugsUtilizationStatus,
                  Just
                    ("GameServerGroupName" .= _ugsGameServerGroupName),
                  Just ("GameServerId" .= _ugsGameServerId)])

instance ToPath UpdateGameServer where
        toPath = const "/"

instance ToQuery UpdateGameServer where
        toQuery = const mempty

-- | /See:/ 'updateGameServerResponse' smart constructor.
data UpdateGameServerResponse = UpdateGameServerResponse'{_ursGameServer
                                                          ::
                                                          !(Maybe GameServer),
                                                          _ursResponseStatus ::
                                                          !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'UpdateGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursGameServer' - Object that describes the newly updated game server resource.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateGameServerResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UpdateGameServerResponse
updateGameServerResponse pResponseStatus_
  = UpdateGameServerResponse'{_ursGameServer = Nothing,
                              _ursResponseStatus = pResponseStatus_}

-- | Object that describes the newly updated game server resource.
ursGameServer :: Lens' UpdateGameServerResponse (Maybe GameServer)
ursGameServer = lens _ursGameServer (\ s a -> s{_ursGameServer = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateGameServerResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UpdateGameServerResponse where
