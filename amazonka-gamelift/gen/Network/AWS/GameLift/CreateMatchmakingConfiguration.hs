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
-- Module      : Network.AWS.GameLift.CreateMatchmakingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a new matchmaking configuration for use with FlexMatch. A matchmaking configuration sets out guidelines for matching players and getting the matches into games. You can set up multiple matchmaking configurations to handle the scenarios needed for your game. Each matchmaking ticket ('StartMatchmaking' or 'StartMatchBackfill' ) specifies a configuration for the match and provides player attributes to support the configuration being used. 
--
--
-- To create a matchmaking configuration, at a minimum you must specify the following: configuration name; a rule set that governs how to evaluate players and find acceptable matches; a game session queue to use when placing a new game session for the match; and the maximum time allowed for a matchmaking attempt.
--
-- There are two ways to track the progress of matchmaking tickets: (1) polling ticket status with 'DescribeMatchmaking' ; or (2) receiving notifications with Amazon Simple Notification Service (SNS). To use notifications, you first need to set up an SNS topic to receive the notifications, and provide the topic ARN in the matchmaking configuration. Since notifications promise only "best effort" delivery, we recommend calling @DescribeMatchmaking@ if no notifications are received within 30 seconds.
--
-- __Learn more__ 
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/match-configuration.html Design a FlexMatch Matchmaker> 
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/match-notification.html Setting up Notifications for Matchmaking> 
--
-- __Related operations__ 
--
--     * 'CreateMatchmakingConfiguration' 
--
--     * 'DescribeMatchmakingConfigurations' 
--
--     * 'UpdateMatchmakingConfiguration' 
--
--     * 'DeleteMatchmakingConfiguration' 
--
--     * 'CreateMatchmakingRuleSet' 
--
--     * 'DescribeMatchmakingRuleSets' 
--
--     * 'ValidateMatchmakingRuleSet' 
--
--     * 'DeleteMatchmakingRuleSet' 
--
--
--
module Network.AWS.GameLift.CreateMatchmakingConfiguration
    (
    -- * Creating a Request
      createMatchmakingConfiguration
    , CreateMatchmakingConfiguration
    -- * Request Lenses
    , cmcBackfillMode
    , cmcGameProperties
    , cmcAcceptanceTimeoutSeconds
    , cmcNotificationTarget
    , cmcCustomEventData
    , cmcGameSessionData
    , cmcDescription
    , cmcTags
    , cmcAdditionalPlayerCount
    , cmcName
    , cmcGameSessionQueueARNs
    , cmcRequestTimeoutSeconds
    , cmcAcceptanceRequired
    , cmcRuleSetName

    -- * Destructuring the Response
    , createMatchmakingConfigurationResponse
    , CreateMatchmakingConfigurationResponse
    -- * Response Lenses
    , cmcrsConfiguration
    , cmcrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'createMatchmakingConfiguration' smart constructor.
data CreateMatchmakingConfiguration = CreateMatchmakingConfiguration'{_cmcBackfillMode
                                                                      ::
                                                                      !(Maybe
                                                                          BackfillMode),
                                                                      _cmcGameProperties
                                                                      ::
                                                                      !(Maybe
                                                                          [GameProperty]),
                                                                      _cmcAcceptanceTimeoutSeconds
                                                                      ::
                                                                      !(Maybe
                                                                          Nat),
                                                                      _cmcNotificationTarget
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _cmcCustomEventData
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _cmcGameSessionData
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _cmcDescription
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _cmcTags
                                                                      ::
                                                                      !(Maybe
                                                                          [Tag]),
                                                                      _cmcAdditionalPlayerCount
                                                                      ::
                                                                      !(Maybe
                                                                          Nat),
                                                                      _cmcName
                                                                      :: !Text,
                                                                      _cmcGameSessionQueueARNs
                                                                      ::
                                                                      ![Text],
                                                                      _cmcRequestTimeoutSeconds
                                                                      :: !Nat,
                                                                      _cmcAcceptanceRequired
                                                                      :: !Bool,
                                                                      _cmcRuleSetName
                                                                      :: !Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcBackfillMode' - The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/developerguide/match-backfill.html Backfill Existing Games with FlexMatch> . 
--
-- * 'cmcGameProperties' - A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. 
--
-- * 'cmcAcceptanceTimeoutSeconds' - The length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
--
-- * 'cmcNotificationTarget' - An SNS topic ARN that is set up to receive matchmaking notifications.
--
-- * 'cmcCustomEventData' - Information to be added to all events related to this matchmaking configuration. 
--
-- * 'cmcGameSessionData' - A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
--
-- * 'cmcDescription' - A human-readable description of the matchmaking configuration. 
--
-- * 'cmcTags' - A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- * 'cmcAdditionalPlayerCount' - The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match.
--
-- * 'cmcName' - A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
--
-- * 'cmcGameSessionQueueARNs' - Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any Region.
--
-- * 'cmcRequestTimeoutSeconds' - The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
--
-- * 'cmcAcceptanceRequired' - A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ .
--
-- * 'cmcRuleSetName' - A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
createMatchmakingConfiguration
    :: Text -- ^ 'cmcName'
    -> Natural -- ^ 'cmcRequestTimeoutSeconds'
    -> Bool -- ^ 'cmcAcceptanceRequired'
    -> Text -- ^ 'cmcRuleSetName'
    -> CreateMatchmakingConfiguration
createMatchmakingConfiguration pName_
  pRequestTimeoutSeconds_ pAcceptanceRequired_
  pRuleSetName_
  = CreateMatchmakingConfiguration'{_cmcBackfillMode =
                                      Nothing,
                                    _cmcGameProperties = Nothing,
                                    _cmcAcceptanceTimeoutSeconds = Nothing,
                                    _cmcNotificationTarget = Nothing,
                                    _cmcCustomEventData = Nothing,
                                    _cmcGameSessionData = Nothing,
                                    _cmcDescription = Nothing,
                                    _cmcTags = Nothing,
                                    _cmcAdditionalPlayerCount = Nothing,
                                    _cmcName = pName_,
                                    _cmcGameSessionQueueARNs = mempty,
                                    _cmcRequestTimeoutSeconds =
                                      _Nat # pRequestTimeoutSeconds_,
                                    _cmcAcceptanceRequired =
                                      pAcceptanceRequired_,
                                    _cmcRuleSetName = pRuleSetName_}

-- | The method used to backfill game sessions that are created with this matchmaking configuration. Specify @MANUAL@ when your game manages backfill requests manually or does not use the match backfill feature. Specify @AUTOMATIC@ to have GameLift create a 'StartMatchBackfill' request whenever a game session has one or more open slots. Learn more about manual and automatic backfill in <https://docs.aws.amazon.com/gamelift/latest/developerguide/match-backfill.html Backfill Existing Games with FlexMatch> . 
cmcBackfillMode :: Lens' CreateMatchmakingConfiguration (Maybe BackfillMode)
cmcBackfillMode = lens _cmcBackfillMode (\ s a -> s{_cmcBackfillMode = a})

-- | A set of custom properties for a game session, formatted as key-value pairs. These properties are passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match. 
cmcGameProperties :: Lens' CreateMatchmakingConfiguration [GameProperty]
cmcGameProperties = lens _cmcGameProperties (\ s a -> s{_cmcGameProperties = a}) . _Default . _Coerce

-- | The length of time (in seconds) to wait for players to accept a proposed match. If any player rejects the match or fails to accept before the timeout, the ticket continues to look for an acceptable match.
cmcAcceptanceTimeoutSeconds :: Lens' CreateMatchmakingConfiguration (Maybe Natural)
cmcAcceptanceTimeoutSeconds = lens _cmcAcceptanceTimeoutSeconds (\ s a -> s{_cmcAcceptanceTimeoutSeconds = a}) . mapping _Nat

-- | An SNS topic ARN that is set up to receive matchmaking notifications.
cmcNotificationTarget :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcNotificationTarget = lens _cmcNotificationTarget (\ s a -> s{_cmcNotificationTarget = a})

-- | Information to be added to all events related to this matchmaking configuration. 
cmcCustomEventData :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcCustomEventData = lens _cmcCustomEventData (\ s a -> s{_cmcCustomEventData = a})

-- | A set of custom game session properties, formatted as a single string value. This data is passed to a game server process in the 'GameSession' object with a request to start a new game session (see <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session> ). This information is added to the new 'GameSession' object that is created for a successful match.
cmcGameSessionData :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcGameSessionData = lens _cmcGameSessionData (\ s a -> s{_cmcGameSessionData = a})

-- | A human-readable description of the matchmaking configuration. 
cmcDescription :: Lens' CreateMatchmakingConfiguration (Maybe Text)
cmcDescription = lens _cmcDescription (\ s a -> s{_cmcDescription = a})

-- | A list of labels to assign to the new matchmaking configuration resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
cmcTags :: Lens' CreateMatchmakingConfiguration [Tag]
cmcTags = lens _cmcTags (\ s a -> s{_cmcTags = a}) . _Default . _Coerce

-- | The number of player slots in a match to keep open for future players. For example, assume that the configuration's rule set specifies a match for a single 12-person team. If the additional player count is set to 2, only 10 players are initially selected for the match.
cmcAdditionalPlayerCount :: Lens' CreateMatchmakingConfiguration (Maybe Natural)
cmcAdditionalPlayerCount = lens _cmcAdditionalPlayerCount (\ s a -> s{_cmcAdditionalPlayerCount = a}) . mapping _Nat

-- | A unique identifier for a matchmaking configuration. This name is used to identify the configuration associated with a matchmaking request or ticket.
cmcName :: Lens' CreateMatchmakingConfiguration Text
cmcName = lens _cmcName (\ s a -> s{_cmcName = a})

-- | Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) that is assigned to a GameLift game session queue resource and uniquely identifies it. ARNs are unique across all Regions. These queues are used when placing game sessions for matches that are created with this matchmaking configuration. Queues can be located in any Region.
cmcGameSessionQueueARNs :: Lens' CreateMatchmakingConfiguration [Text]
cmcGameSessionQueueARNs = lens _cmcGameSessionQueueARNs (\ s a -> s{_cmcGameSessionQueueARNs = a}) . _Coerce

-- | The maximum duration, in seconds, that a matchmaking ticket can remain in process before timing out. Requests that fail due to timing out can be resubmitted as needed.
cmcRequestTimeoutSeconds :: Lens' CreateMatchmakingConfiguration Natural
cmcRequestTimeoutSeconds = lens _cmcRequestTimeoutSeconds (\ s a -> s{_cmcRequestTimeoutSeconds = a}) . _Nat

-- | A flag that determines whether a match that was created with this configuration must be accepted by the matched players. To require acceptance, set to @TRUE@ .
cmcAcceptanceRequired :: Lens' CreateMatchmakingConfiguration Bool
cmcAcceptanceRequired = lens _cmcAcceptanceRequired (\ s a -> s{_cmcAcceptanceRequired = a})

-- | A unique identifier for a matchmaking rule set to use with this configuration. You can use either the rule set name or ARN value. A matchmaking configuration can only use rule sets that are defined in the same Region.
cmcRuleSetName :: Lens' CreateMatchmakingConfiguration Text
cmcRuleSetName = lens _cmcRuleSetName (\ s a -> s{_cmcRuleSetName = a})

instance AWSRequest CreateMatchmakingConfiguration
         where
        type Rs CreateMatchmakingConfiguration =
             CreateMatchmakingConfigurationResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 CreateMatchmakingConfigurationResponse' <$>
                   (x .?> "Configuration") <*> (pure (fromEnum s)))

instance Hashable CreateMatchmakingConfiguration
         where

instance NFData CreateMatchmakingConfiguration where

instance ToHeaders CreateMatchmakingConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.CreateMatchmakingConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMatchmakingConfiguration where
        toJSON CreateMatchmakingConfiguration'{..}
          = object
              (catMaybes
                 [("BackfillMode" .=) <$> _cmcBackfillMode,
                  ("GameProperties" .=) <$> _cmcGameProperties,
                  ("AcceptanceTimeoutSeconds" .=) <$>
                    _cmcAcceptanceTimeoutSeconds,
                  ("NotificationTarget" .=) <$> _cmcNotificationTarget,
                  ("CustomEventData" .=) <$> _cmcCustomEventData,
                  ("GameSessionData" .=) <$> _cmcGameSessionData,
                  ("Description" .=) <$> _cmcDescription,
                  ("Tags" .=) <$> _cmcTags,
                  ("AdditionalPlayerCount" .=) <$>
                    _cmcAdditionalPlayerCount,
                  Just ("Name" .= _cmcName),
                  Just
                    ("GameSessionQueueArns" .= _cmcGameSessionQueueARNs),
                  Just
                    ("RequestTimeoutSeconds" .=
                       _cmcRequestTimeoutSeconds),
                  Just
                    ("AcceptanceRequired" .= _cmcAcceptanceRequired),
                  Just ("RuleSetName" .= _cmcRuleSetName)])

instance ToPath CreateMatchmakingConfiguration where
        toPath = const "/"

instance ToQuery CreateMatchmakingConfiguration where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'createMatchmakingConfigurationResponse' smart constructor.
data CreateMatchmakingConfigurationResponse = CreateMatchmakingConfigurationResponse'{_cmcrsConfiguration
                                                                                      ::
                                                                                      !(Maybe
                                                                                          MatchmakingConfiguration),
                                                                                      _cmcrsResponseStatus
                                                                                      ::
                                                                                      !Int}
                                                deriving (Eq, Read, Show, Data,
                                                          Typeable, Generic)

-- | Creates a value of 'CreateMatchmakingConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcrsConfiguration' - Object that describes the newly created matchmaking configuration.
--
-- * 'cmcrsResponseStatus' - -- | The response status code.
createMatchmakingConfigurationResponse
    :: Int -- ^ 'cmcrsResponseStatus'
    -> CreateMatchmakingConfigurationResponse
createMatchmakingConfigurationResponse
  pResponseStatus_
  = CreateMatchmakingConfigurationResponse'{_cmcrsConfiguration
                                              = Nothing,
                                            _cmcrsResponseStatus =
                                              pResponseStatus_}

-- | Object that describes the newly created matchmaking configuration.
cmcrsConfiguration :: Lens' CreateMatchmakingConfigurationResponse (Maybe MatchmakingConfiguration)
cmcrsConfiguration = lens _cmcrsConfiguration (\ s a -> s{_cmcrsConfiguration = a})

-- | -- | The response status code.
cmcrsResponseStatus :: Lens' CreateMatchmakingConfigurationResponse Int
cmcrsResponseStatus = lens _cmcrsResponseStatus (\ s a -> s{_cmcrsResponseStatus = a})

instance NFData
           CreateMatchmakingConfigurationResponse
         where
