{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServer where

import Network.AWS.GameLift.Types.GameServerClaimStatus
import Network.AWS.GameLift.Types.GameServerUtilizationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | __This data type is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Properties describing a game server resource. 
--
-- A game server resource is created by a successful call to 'RegisterGameServer' and deleted by calling 'DeregisterGameServer' . 
--
--
-- /See:/ 'gameServer' smart constructor.
data GameServer = GameServer'{_gsInstanceId ::
                              !(Maybe Text),
                              _gsLastClaimTime :: !(Maybe POSIX),
                              _gsGameServerGroupName :: !(Maybe Text),
                              _gsGameServerData :: !(Maybe Text),
                              _gsClaimStatus :: !(Maybe GameServerClaimStatus),
                              _gsCustomSortKey :: !(Maybe Text),
                              _gsGameServerId :: !(Maybe Text),
                              _gsUtilizationStatus ::
                              !(Maybe GameServerUtilizationStatus),
                              _gsRegistrationTime :: !(Maybe POSIX),
                              _gsLastHealthCheckTime :: !(Maybe POSIX),
                              _gsConnectionInfo :: !(Maybe Text),
                              _gsGameServerGroupARN :: !(Maybe Text)}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GameServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsInstanceId' - The unique identifier for the instance where the game server is located.
--
-- * 'gsLastClaimTime' - Time stamp indicating the last time the game server was claimed with a 'ClaimGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057"). This value is used to calculate when the game server's claim status.
--
-- * 'gsGameServerGroupName' - The name identifier for the game server group where the game server is located.
--
-- * 'gsGameServerData' - A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service in response to requests 'ListGameServers' or 'ClaimGameServer' . This property can be updated using 'UpdateGameServer' .
--
-- * 'gsClaimStatus' - Indicates when an available game server has been reserved but has not yet started hosting a game. Once it is claimed, game server remains in CLAIMED status for a maximum of one minute. During this time, game clients must connect to the game server and start the game, which triggers the game server to update its utilization status. After one minute, the game server claim status reverts to null.
--
-- * 'gsCustomSortKey' - A game server tag that can be used to request sorted lists of game servers when calling 'ListGameServers' . Custom sort keys are developer-defined. This property can be updated using 'UpdateGameServer' .
--
-- * 'gsGameServerId' - A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
--
-- * 'gsUtilizationStatus' - Indicates whether the game server is currently available for new games or is busy. Possible statuses include:     * AVAILABLE - The game server is available to be claimed. A game server that has been claimed remains in this status until it reports game hosting activity.      * IN_USE - The game server is currently hosting a game session with players. 
--
-- * 'gsRegistrationTime' - Time stamp indicating when the game server resource was created with a 'RegisterGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
--
-- * 'gsLastHealthCheckTime' - Time stamp indicating the last time the game server was updated with health status using an 'UpdateGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057"). After game server registration, this property is only changed when a game server update specifies a health check value.
--
-- * 'gsConnectionInfo' - The port and IP address that must be used to establish a client connection to the game server.
--
-- * 'gsGameServerGroupARN' - The ARN identifier for the game server group where the game server is located.
gameServer
    :: GameServer
gameServer
  = GameServer'{_gsInstanceId = Nothing,
                _gsLastClaimTime = Nothing,
                _gsGameServerGroupName = Nothing,
                _gsGameServerData = Nothing,
                _gsClaimStatus = Nothing, _gsCustomSortKey = Nothing,
                _gsGameServerId = Nothing,
                _gsUtilizationStatus = Nothing,
                _gsRegistrationTime = Nothing,
                _gsLastHealthCheckTime = Nothing,
                _gsConnectionInfo = Nothing,
                _gsGameServerGroupARN = Nothing}

-- | The unique identifier for the instance where the game server is located.
gsInstanceId :: Lens' GameServer (Maybe Text)
gsInstanceId = lens _gsInstanceId (\ s a -> s{_gsInstanceId = a})

-- | Time stamp indicating the last time the game server was claimed with a 'ClaimGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057"). This value is used to calculate when the game server's claim status.
gsLastClaimTime :: Lens' GameServer (Maybe UTCTime)
gsLastClaimTime = lens _gsLastClaimTime (\ s a -> s{_gsLastClaimTime = a}) . mapping _Time

-- | The name identifier for the game server group where the game server is located.
gsGameServerGroupName :: Lens' GameServer (Maybe Text)
gsGameServerGroupName = lens _gsGameServerGroupName (\ s a -> s{_gsGameServerGroupName = a})

-- | A set of custom game server properties, formatted as a single string value. This data is passed to a game client or service in response to requests 'ListGameServers' or 'ClaimGameServer' . This property can be updated using 'UpdateGameServer' .
gsGameServerData :: Lens' GameServer (Maybe Text)
gsGameServerData = lens _gsGameServerData (\ s a -> s{_gsGameServerData = a})

-- | Indicates when an available game server has been reserved but has not yet started hosting a game. Once it is claimed, game server remains in CLAIMED status for a maximum of one minute. During this time, game clients must connect to the game server and start the game, which triggers the game server to update its utilization status. After one minute, the game server claim status reverts to null.
gsClaimStatus :: Lens' GameServer (Maybe GameServerClaimStatus)
gsClaimStatus = lens _gsClaimStatus (\ s a -> s{_gsClaimStatus = a})

-- | A game server tag that can be used to request sorted lists of game servers when calling 'ListGameServers' . Custom sort keys are developer-defined. This property can be updated using 'UpdateGameServer' .
gsCustomSortKey :: Lens' GameServer (Maybe Text)
gsCustomSortKey = lens _gsCustomSortKey (\ s a -> s{_gsCustomSortKey = a})

-- | A custom string that uniquely identifies the game server. Game server IDs are developer-defined and are unique across all game server groups in an AWS account.
gsGameServerId :: Lens' GameServer (Maybe Text)
gsGameServerId = lens _gsGameServerId (\ s a -> s{_gsGameServerId = a})

-- | Indicates whether the game server is currently available for new games or is busy. Possible statuses include:     * AVAILABLE - The game server is available to be claimed. A game server that has been claimed remains in this status until it reports game hosting activity.      * IN_USE - The game server is currently hosting a game session with players. 
gsUtilizationStatus :: Lens' GameServer (Maybe GameServerUtilizationStatus)
gsUtilizationStatus = lens _gsUtilizationStatus (\ s a -> s{_gsUtilizationStatus = a})

-- | Time stamp indicating when the game server resource was created with a 'RegisterGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057").
gsRegistrationTime :: Lens' GameServer (Maybe UTCTime)
gsRegistrationTime = lens _gsRegistrationTime (\ s a -> s{_gsRegistrationTime = a}) . mapping _Time

-- | Time stamp indicating the last time the game server was updated with health status using an 'UpdateGameServer' request. Format is a number expressed in Unix time as milliseconds (for example "1469498468.057"). After game server registration, this property is only changed when a game server update specifies a health check value.
gsLastHealthCheckTime :: Lens' GameServer (Maybe UTCTime)
gsLastHealthCheckTime = lens _gsLastHealthCheckTime (\ s a -> s{_gsLastHealthCheckTime = a}) . mapping _Time

-- | The port and IP address that must be used to establish a client connection to the game server.
gsConnectionInfo :: Lens' GameServer (Maybe Text)
gsConnectionInfo = lens _gsConnectionInfo (\ s a -> s{_gsConnectionInfo = a})

-- | The ARN identifier for the game server group where the game server is located.
gsGameServerGroupARN :: Lens' GameServer (Maybe Text)
gsGameServerGroupARN = lens _gsGameServerGroupARN (\ s a -> s{_gsGameServerGroupARN = a})

instance FromJSON GameServer where
        parseJSON
          = withObject "GameServer"
              (\ x ->
                 GameServer' <$>
                   (x .:? "InstanceId") <*> (x .:? "LastClaimTime") <*>
                     (x .:? "GameServerGroupName")
                     <*> (x .:? "GameServerData")
                     <*> (x .:? "ClaimStatus")
                     <*> (x .:? "CustomSortKey")
                     <*> (x .:? "GameServerId")
                     <*> (x .:? "UtilizationStatus")
                     <*> (x .:? "RegistrationTime")
                     <*> (x .:? "LastHealthCheckTime")
                     <*> (x .:? "ConnectionInfo")
                     <*> (x .:? "GameServerGroupArn"))

instance Hashable GameServer where

instance NFData GameServer where
