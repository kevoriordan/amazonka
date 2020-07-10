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
-- Module      : Network.AWS.GameLift.UpdateGameServerGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This action is part of Amazon GameLift FleetIQ with game server groups, which is in preview release and is subject to change.__ 
--
--
-- Updates GameLift FleetIQ-specific properties for a game server group. These properties include instance rebalancing and game server protection. Many Auto Scaling group properties are updated directly. These include autoscaling policies, minimum/maximum/desired instance counts, and launch template. 
--
-- To update the game server group, specify the game server group ID and provide the updated values.
--
-- Updated properties are validated to ensure that GameLift FleetIQ can continue to perform its core instance rebalancing activity. When you change Auto Scaling group properties directly and the changes cause errors with GameLift FleetIQ activities, an alert is sent.
--
-- __Learn more__ 
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gsg-intro.html GameLift FleetIQ Guide> 
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gsg-asgroups.html Updating a GameLift FleetIQ-Linked Auto Scaling Group> 
--
-- __Related operations__ 
--
--     * 'CreateGameServerGroup' 
--
--     * 'ListGameServerGroups' 
--
--     * 'DescribeGameServerGroup' 
--
--     * 'UpdateGameServerGroup' 
--
--     * 'DeleteGameServerGroup' 
--
--     * 'ResumeGameServerGroup' 
--
--     * 'SuspendGameServerGroup' 
--
--
--
module Network.AWS.GameLift.UpdateGameServerGroup
    (
    -- * Creating a Request
      updateGameServerGroup
    , UpdateGameServerGroup
    -- * Request Lenses
    , ugsgInstanceDefinitions
    , ugsgBalancingStrategy
    , ugsgGameServerProtectionPolicy
    , ugsgRoleARN
    , ugsgGameServerGroupName

    -- * Destructuring the Response
    , updateGameServerGroupResponse
    , UpdateGameServerGroupResponse
    -- * Response Lenses
    , ugsgrsGameServerGroup
    , ugsgrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGameServerGroup' smart constructor.
data UpdateGameServerGroup = UpdateGameServerGroup'{_ugsgInstanceDefinitions
                                                    ::
                                                    !(Maybe
                                                        (List1
                                                           InstanceDefinition)),
                                                    _ugsgBalancingStrategy ::
                                                    !(Maybe BalancingStrategy),
                                                    _ugsgGameServerProtectionPolicy
                                                    ::
                                                    !(Maybe
                                                        GameServerProtectionPolicy),
                                                    _ugsgRoleARN ::
                                                    !(Maybe Text),
                                                    _ugsgGameServerGroupName ::
                                                    !Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'UpdateGameServerGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsgInstanceDefinitions' - An updated list of EC2 instance types to use when creating instances in the group. The instance definition must specify instance types that are supported by GameLift FleetIQ, and must include at least two instance types. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ ..
--
-- * 'ugsgBalancingStrategy' - The fallback balancing method to use for the game server group when Spot instances in a Region become unavailable or are not viable for game hosting. Once triggered, this method remains active until Spot instances can once again be used. Method options include:     * SPOT_ONLY -- If Spot instances are unavailable, the game server group provides no hosting capacity. No new instances are started, and the existing nonviable Spot instances are terminated (once current gameplay ends) and not replaced.     * SPOT_PREFERRED -- If Spot instances are unavailable, the game server group continues to provide hosting capacity by using On-Demand instances. Existing nonviable Spot instances are terminated (once current gameplay ends) and replaced with new On-Demand instances. 
--
-- * 'ugsgGameServerProtectionPolicy' - A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running may by terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running. An exception to this is Spot Instances, which may be terminated by AWS regardless of protection status. This property is set to NO_PROTECTION by default.
--
-- * 'ugsgRoleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups. The submitted role is validated to ensure that it contains the necessary permissions for game server groups.
--
-- * 'ugsgGameServerGroupName' - The unique identifier of the game server group to update. Use either the 'GameServerGroup' name or ARN value.
updateGameServerGroup
    :: Text -- ^ 'ugsgGameServerGroupName'
    -> UpdateGameServerGroup
updateGameServerGroup pGameServerGroupName_
  = UpdateGameServerGroup'{_ugsgInstanceDefinitions =
                             Nothing,
                           _ugsgBalancingStrategy = Nothing,
                           _ugsgGameServerProtectionPolicy = Nothing,
                           _ugsgRoleARN = Nothing,
                           _ugsgGameServerGroupName = pGameServerGroupName_}

-- | An updated list of EC2 instance types to use when creating instances in the group. The instance definition must specify instance types that are supported by GameLift FleetIQ, and must include at least two instance types. This updated list replaces the entire current list of instance definitions for the game server group. For more information on instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html EC2 Instance Types> in the /Amazon EC2 User Guide/ ..
ugsgInstanceDefinitions :: Lens' UpdateGameServerGroup (Maybe (NonEmpty InstanceDefinition))
ugsgInstanceDefinitions = lens _ugsgInstanceDefinitions (\ s a -> s{_ugsgInstanceDefinitions = a}) . mapping _List1

-- | The fallback balancing method to use for the game server group when Spot instances in a Region become unavailable or are not viable for game hosting. Once triggered, this method remains active until Spot instances can once again be used. Method options include:     * SPOT_ONLY -- If Spot instances are unavailable, the game server group provides no hosting capacity. No new instances are started, and the existing nonviable Spot instances are terminated (once current gameplay ends) and not replaced.     * SPOT_PREFERRED -- If Spot instances are unavailable, the game server group continues to provide hosting capacity by using On-Demand instances. Existing nonviable Spot instances are terminated (once current gameplay ends) and replaced with new On-Demand instances. 
ugsgBalancingStrategy :: Lens' UpdateGameServerGroup (Maybe BalancingStrategy)
ugsgBalancingStrategy = lens _ugsgBalancingStrategy (\ s a -> s{_ugsgBalancingStrategy = a})

-- | A flag that indicates whether instances in the game server group are protected from early termination. Unprotected instances that have active game servers running may by terminated during a scale-down event, causing players to be dropped from the game. Protected instances cannot be terminated while there are active game servers running. An exception to this is Spot Instances, which may be terminated by AWS regardless of protection status. This property is set to NO_PROTECTION by default.
ugsgGameServerProtectionPolicy :: Lens' UpdateGameServerGroup (Maybe GameServerProtectionPolicy)
ugsgGameServerProtectionPolicy = lens _ugsgGameServerProtectionPolicy (\ s a -> s{_ugsgGameServerProtectionPolicy = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access your EC2 Auto Scaling groups. The submitted role is validated to ensure that it contains the necessary permissions for game server groups.
ugsgRoleARN :: Lens' UpdateGameServerGroup (Maybe Text)
ugsgRoleARN = lens _ugsgRoleARN (\ s a -> s{_ugsgRoleARN = a})

-- | The unique identifier of the game server group to update. Use either the 'GameServerGroup' name or ARN value.
ugsgGameServerGroupName :: Lens' UpdateGameServerGroup Text
ugsgGameServerGroupName = lens _ugsgGameServerGroupName (\ s a -> s{_ugsgGameServerGroupName = a})

instance AWSRequest UpdateGameServerGroup where
        type Rs UpdateGameServerGroup =
             UpdateGameServerGroupResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGameServerGroupResponse' <$>
                   (x .?> "GameServerGroup") <*> (pure (fromEnum s)))

instance Hashable UpdateGameServerGroup where

instance NFData UpdateGameServerGroup where

instance ToHeaders UpdateGameServerGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateGameServerGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateGameServerGroup where
        toJSON UpdateGameServerGroup'{..}
          = object
              (catMaybes
                 [("InstanceDefinitions" .=) <$>
                    _ugsgInstanceDefinitions,
                  ("BalancingStrategy" .=) <$> _ugsgBalancingStrategy,
                  ("GameServerProtectionPolicy" .=) <$>
                    _ugsgGameServerProtectionPolicy,
                  ("RoleArn" .=) <$> _ugsgRoleARN,
                  Just
                    ("GameServerGroupName" .= _ugsgGameServerGroupName)])

instance ToPath UpdateGameServerGroup where
        toPath = const "/"

instance ToQuery UpdateGameServerGroup where
        toQuery = const mempty

-- | /See:/ 'updateGameServerGroupResponse' smart constructor.
data UpdateGameServerGroupResponse = UpdateGameServerGroupResponse'{_ugsgrsGameServerGroup
                                                                    ::
                                                                    !(Maybe
                                                                        GameServerGroup),
                                                                    _ugsgrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'UpdateGameServerGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugsgrsGameServerGroup' - An object that describes the game server group resource with updated properties. 
--
-- * 'ugsgrsResponseStatus' - -- | The response status code.
updateGameServerGroupResponse
    :: Int -- ^ 'ugsgrsResponseStatus'
    -> UpdateGameServerGroupResponse
updateGameServerGroupResponse pResponseStatus_
  = UpdateGameServerGroupResponse'{_ugsgrsGameServerGroup
                                     = Nothing,
                                   _ugsgrsResponseStatus = pResponseStatus_}

-- | An object that describes the game server group resource with updated properties. 
ugsgrsGameServerGroup :: Lens' UpdateGameServerGroupResponse (Maybe GameServerGroup)
ugsgrsGameServerGroup = lens _ugsgrsGameServerGroup (\ s a -> s{_ugsgrsGameServerGroup = a})

-- | -- | The response status code.
ugsgrsResponseStatus :: Lens' UpdateGameServerGroupResponse Int
ugsgrsResponseStatus = lens _ugsgrsResponseStatus (\ s a -> s{_ugsgrsResponseStatus = a})

instance NFData UpdateGameServerGroupResponse where
