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
-- Module      : Network.AWS.GameLift.RegisterGameServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This action is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Creates a new game server resource and notifies GameLift FleetIQ that the game server is ready to host gameplay and players. This action is called by a game server process that is running on an instance in a game server group. Registering game servers enables GameLift FleetIQ to track available game servers and enables game clients and services to claim a game server for a new game session. 
--
-- To register a game server, identify the game server group and instance where the game server is running, and provide a unique identifier for the game server. You can also include connection and game server data; when a game client or service requests a game server by calling 'ClaimGameServer' , this information is returned in response.
--
-- Once a game server is successfully registered, it is put in status AVAILABLE. A request to register a game server may fail if the instance it is in the process of shutting down as part of instance rebalancing or scale-down activity. 
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
module Network.AWS.GameLift.RegisterGameServer
    (
    -- * Creating a Request
      registerGameServer
    , RegisterGameServer
    -- * Request Lenses
    , rgsGameServerData
    , rgsCustomSortKey
    , rgsConnectionInfo
    , rgsTags
    , rgsGameServerGroupName
    , rgsGameServerId
    , rgsInstanceId

    -- * Destructuring the Response
    , registerGameServerResponse
    , RegisterGameServerResponse
    -- * Response Lenses
    , rgsrsGameServer
    , rgsrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerGameServer' smart constructor.
data RegisterGameServer = RegisterGameServer'{_rgsGameServerData
                                              :: !(Maybe Text),
                                              _rgsCustomSortKey ::
                                              !(Maybe Text),
                                              _rgsConnectionInfo ::
                                              !(Maybe Text),
                                              _rgsTags :: !(Maybe [Tag]),
                                              _rgsGameServerGroupName :: !Text,
                                              _rgsGameServerId :: !Text,
                                              _rgsInstanceId :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterGameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on a game servers using 'ListGameServers' or 'ClaimGameServer' . 
--
-- * 'rgsCustomSortKey' - A game server tag that can be used to request sorted lists of game servers using 'ListGameServers' . Custom sort keys are developer-defined based on how you want to organize the retrieved game server information.
--
-- * 'rgsConnectionInfo' - Information needed to make inbound client connections to the game server. This might include IP address and port, DNS name, etc.
--
-- * 'rgsTags' - A list of labels to assign to the new game server resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'rgsGameServerGroupName' - An identifier for the game server group where the game server is running. You can use either the 'GameServerGroup' name or ARN value.
--
-- * 'rgsGameServerId' - A custom string that uniquely identifies the new game server. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
--
-- * 'rgsInstanceId' - The unique identifier for the instance where the game server is running. This ID is available in the instance metadata.
registerGameServer
    :: Text -- ^ 'rgsGameServerGroupName'
    -> Text -- ^ 'rgsGameServerId'
    -> Text -- ^ 'rgsInstanceId'
    -> RegisterGameServer
registerGameServer pGameServerGroupName_
  pGameServerId_ pInstanceId_
  = RegisterGameServer'{_rgsGameServerData = Nothing,
                        _rgsCustomSortKey = Nothing,
                        _rgsConnectionInfo = Nothing, _rgsTags = Nothing,
                        _rgsGameServerGroupName = pGameServerGroupName_,
                        _rgsGameServerId = pGameServerId_,
                        _rgsInstanceId = pInstanceId_}

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service when it requests information on a game servers using 'ListGameServers' or 'ClaimGameServer' . 
rgsGameServerData :: Lens' RegisterGameServer (Maybe Text)
rgsGameServerData = lens _rgsGameServerData (\ s a -> s{_rgsGameServerData = a})

-- | A game server tag that can be used to request sorted lists of game servers using 'ListGameServers' . Custom sort keys are developer-defined based on how you want to organize the retrieved game server information.
rgsCustomSortKey :: Lens' RegisterGameServer (Maybe Text)
rgsCustomSortKey = lens _rgsCustomSortKey (\ s a -> s{_rgsCustomSortKey = a})

-- | Information needed to make inbound client connections to the game server. This might include IP address and port, DNS name, etc.
rgsConnectionInfo :: Lens' RegisterGameServer (Maybe Text)
rgsConnectionInfo = lens _rgsConnectionInfo (\ s a -> s{_rgsConnectionInfo = a})

-- | A list of labels to assign to the new game server resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management, and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
rgsTags :: Lens' RegisterGameServer [Tag]
rgsTags = lens _rgsTags (\ s a -> s{_rgsTags = a}) . _Default . _Coerce

-- | An identifier for the game server group where the game server is running. You can use either the 'GameServerGroup' name or ARN value.
rgsGameServerGroupName :: Lens' RegisterGameServer Text
rgsGameServerGroupName = lens _rgsGameServerGroupName (\ s a -> s{_rgsGameServerGroupName = a})

-- | A custom string that uniquely identifies the new game server. Game server IDs are developer-defined and must be unique across all game server groups in your AWS account.
rgsGameServerId :: Lens' RegisterGameServer Text
rgsGameServerId = lens _rgsGameServerId (\ s a -> s{_rgsGameServerId = a})

-- | The unique identifier for the instance where the game server is running. This ID is available in the instance metadata.
rgsInstanceId :: Lens' RegisterGameServer Text
rgsInstanceId = lens _rgsInstanceId (\ s a -> s{_rgsInstanceId = a})

instance AWSRequest RegisterGameServer where
        type Rs RegisterGameServer =
             RegisterGameServerResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 RegisterGameServerResponse' <$>
                   (x .?> "GameServer") <*> (pure (fromEnum s)))

instance Hashable RegisterGameServer where

instance NFData RegisterGameServer where

instance ToHeaders RegisterGameServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.RegisterGameServer" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterGameServer where
        toJSON RegisterGameServer'{..}
          = object
              (catMaybes
                 [("GameServerData" .=) <$> _rgsGameServerData,
                  ("CustomSortKey" .=) <$> _rgsCustomSortKey,
                  ("ConnectionInfo" .=) <$> _rgsConnectionInfo,
                  ("Tags" .=) <$> _rgsTags,
                  Just
                    ("GameServerGroupName" .= _rgsGameServerGroupName),
                  Just ("GameServerId" .= _rgsGameServerId),
                  Just ("InstanceId" .= _rgsInstanceId)])

instance ToPath RegisterGameServer where
        toPath = const "/"

instance ToQuery RegisterGameServer where
        toQuery = const mempty

-- | /See:/ 'registerGameServerResponse' smart constructor.
data RegisterGameServerResponse = RegisterGameServerResponse'{_rgsrsGameServer
                                                              ::
                                                              !(Maybe
                                                                  GameServer),
                                                              _rgsrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'RegisterGameServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgsrsGameServer' - Object that describes the newly created game server resource.
--
-- * 'rgsrsResponseStatus' - -- | The response status code.
registerGameServerResponse
    :: Int -- ^ 'rgsrsResponseStatus'
    -> RegisterGameServerResponse
registerGameServerResponse pResponseStatus_
  = RegisterGameServerResponse'{_rgsrsGameServer =
                                  Nothing,
                                _rgsrsResponseStatus = pResponseStatus_}

-- | Object that describes the newly created game server resource.
rgsrsGameServer :: Lens' RegisterGameServerResponse (Maybe GameServer)
rgsrsGameServer = lens _rgsrsGameServer (\ s a -> s{_rgsrsGameServer = a})

-- | -- | The response status code.
rgsrsResponseStatus :: Lens' RegisterGameServerResponse Int
rgsrsResponseStatus = lens _rgsrsResponseStatus (\ s a -> s{_rgsrsResponseStatus = a})

instance NFData RegisterGameServerResponse where
