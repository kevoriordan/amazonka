{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Deployment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Deployment where

import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of an Amazon ECS service deployment. This is used only when a service uses the @ECS@ deployment controller type.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'{_dRunningCount ::
                              !(Maybe Int),
                              _dStatus :: !(Maybe Text),
                              _dCreatedAt :: !(Maybe POSIX),
                              _dPlatformVersion :: !(Maybe Text),
                              _dDesiredCount :: !(Maybe Int),
                              _dPendingCount :: !(Maybe Int),
                              _dId :: !(Maybe Text),
                              _dLaunchType :: !(Maybe LaunchType),
                              _dUpdatedAt :: !(Maybe POSIX),
                              _dTaskDefinition :: !(Maybe Text),
                              _dNetworkConfiguration ::
                              !(Maybe NetworkConfiguration),
                              _dCapacityProviderStrategy ::
                              !(Maybe [CapacityProviderStrategyItem])}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRunningCount' - The number of tasks in the deployment that are in the @RUNNING@ status.
--
-- * 'dStatus' - The status of the deployment. The following describes each state:     * PRIMARY    * The most recent deployment of a service.     * ACTIVE    * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.     * INACTIVE    * A deployment that has been completely replaced.
--
-- * 'dCreatedAt' - The Unix timestamp for when the service deployment was created.
--
-- * 'dPlatformVersion' - The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'dDesiredCount' - The most recent desired count of tasks that was specified for the service to deploy or maintain.
--
-- * 'dPendingCount' - The number of tasks in the deployment that are in the @PENDING@ status.
--
-- * 'dId' - The ID of the deployment.
--
-- * 'dLaunchType' - The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'dUpdatedAt' - The Unix timestamp for when the service deployment was last updated.
--
-- * 'dTaskDefinition' - The most recent task definition that was specified for the tasks in the service to use.
--
-- * 'dNetworkConfiguration' - The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
--
-- * 'dCapacityProviderStrategy' - The capacity provider strategy that the deployment is using.
deployment
    :: Deployment
deployment
  = Deployment'{_dRunningCount = Nothing,
                _dStatus = Nothing, _dCreatedAt = Nothing,
                _dPlatformVersion = Nothing,
                _dDesiredCount = Nothing, _dPendingCount = Nothing,
                _dId = Nothing, _dLaunchType = Nothing,
                _dUpdatedAt = Nothing, _dTaskDefinition = Nothing,
                _dNetworkConfiguration = Nothing,
                _dCapacityProviderStrategy = Nothing}

-- | The number of tasks in the deployment that are in the @RUNNING@ status.
dRunningCount :: Lens' Deployment (Maybe Int)
dRunningCount = lens _dRunningCount (\ s a -> s{_dRunningCount = a})

-- | The status of the deployment. The following describes each state:     * PRIMARY    * The most recent deployment of a service.     * ACTIVE    * A service deployment that still has running tasks, but are in the process of being replaced with a new @PRIMARY@ deployment.     * INACTIVE    * A deployment that has been completely replaced.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The Unix timestamp for when the service deployment was created.
dCreatedAt :: Lens' Deployment (Maybe UTCTime)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a}) . mapping _Time

-- | The platform version on which your tasks in the service are running. A platform version is only specified for tasks using the Fargate launch type. If one is not specified, the @LATEST@ platform version is used by default. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html AWS Fargate Platform Versions> in the /Amazon Elastic Container Service Developer Guide/ .
dPlatformVersion :: Lens' Deployment (Maybe Text)
dPlatformVersion = lens _dPlatformVersion (\ s a -> s{_dPlatformVersion = a})

-- | The most recent desired count of tasks that was specified for the service to deploy or maintain.
dDesiredCount :: Lens' Deployment (Maybe Int)
dDesiredCount = lens _dDesiredCount (\ s a -> s{_dDesiredCount = a})

-- | The number of tasks in the deployment that are in the @PENDING@ status.
dPendingCount :: Lens' Deployment (Maybe Int)
dPendingCount = lens _dPendingCount (\ s a -> s{_dPendingCount = a})

-- | The ID of the deployment.
dId :: Lens' Deployment (Maybe Text)
dId = lens _dId (\ s a -> s{_dId = a})

-- | The launch type the tasks in the service are using. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS Launch Types> in the /Amazon Elastic Container Service Developer Guide/ .
dLaunchType :: Lens' Deployment (Maybe LaunchType)
dLaunchType = lens _dLaunchType (\ s a -> s{_dLaunchType = a})

-- | The Unix timestamp for when the service deployment was last updated.
dUpdatedAt :: Lens' Deployment (Maybe UTCTime)
dUpdatedAt = lens _dUpdatedAt (\ s a -> s{_dUpdatedAt = a}) . mapping _Time

-- | The most recent task definition that was specified for the tasks in the service to use.
dTaskDefinition :: Lens' Deployment (Maybe Text)
dTaskDefinition = lens _dTaskDefinition (\ s a -> s{_dTaskDefinition = a})

-- | The VPC subnet and security group configuration for tasks that receive their own elastic network interface by using the @awsvpc@ networking mode.
dNetworkConfiguration :: Lens' Deployment (Maybe NetworkConfiguration)
dNetworkConfiguration = lens _dNetworkConfiguration (\ s a -> s{_dNetworkConfiguration = a})

-- | The capacity provider strategy that the deployment is using.
dCapacityProviderStrategy :: Lens' Deployment [CapacityProviderStrategyItem]
dCapacityProviderStrategy = lens _dCapacityProviderStrategy (\ s a -> s{_dCapacityProviderStrategy = a}) . _Default . _Coerce

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "runningCount") <*> (x .:? "status") <*>
                     (x .:? "createdAt")
                     <*> (x .:? "platformVersion")
                     <*> (x .:? "desiredCount")
                     <*> (x .:? "pendingCount")
                     <*> (x .:? "id")
                     <*> (x .:? "launchType")
                     <*> (x .:? "updatedAt")
                     <*> (x .:? "taskDefinition")
                     <*> (x .:? "networkConfiguration")
                     <*> (x .:? "capacityProviderStrategy" .!= mempty))

instance Hashable Deployment where

instance NFData Deployment where
