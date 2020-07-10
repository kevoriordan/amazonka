{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LogConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.LogConfiguration where

import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.Secret
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The log configuration specification for the container.
--
--
-- This parameter maps to @LogConfig@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--log-driver@ option to <https://docs.docker.com/engine/reference/commandline/run/ @docker run@ > . By default, containers use the same logging driver that the Docker daemon uses; however the container may use a different logging driver than the Docker daemon by specifying a log driver with this parameter in the container definition. To use a different logging driver for a container, the log system must be configured properly on the container instance (or on a different log server for remote logging options). For more information on the options for different supported log drivers, see <https://docs.docker.com/engine/admin/logging/overview/ Configure logging drivers> in the Docker documentation.
--
-- The following should be noted when specifying a log configuration for your containers:
--
--     * Amazon ECS currently supports a subset of the logging drivers available to the Docker daemon (shown in the valid values below). Additional log drivers may be available in future releases of the Amazon ECS container agent.
--
--     * This parameter requires version 1.18 of the Docker Remote API or greater on your container instance.
--
--     * For tasks using the EC2 launch type, the Amazon ECS container agent running on a container instance must register the logging drivers available on that instance with the @ECS_AVAILABLE_LOGGING_DRIVERS@ environment variable before containers placed on that instance can use these log configuration options. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html Amazon ECS Container Agent Configuration> in the /Amazon Elastic Container Service Developer Guide/ .
--
--     * For tasks using the Fargate launch type, because you do not have access to the underlying infrastructure your tasks are hosted on, any additional software needed will have to be installed outside of the task. For example, the Fluentd output aggregators or a remote host running Logstash to send Gelf logs to.
--
--
--
--
-- /See:/ 'logConfiguration' smart constructor.
data LogConfiguration = LogConfiguration'{_lcOptions
                                          :: !(Maybe (Map Text Text)),
                                          _lcSecretOptions :: !(Maybe [Secret]),
                                          _lcLogDriver :: !LogDriver}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcOptions' - The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
--
-- * 'lcSecretOptions' - The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'lcLogDriver' - The log driver to use for the container. The valid values listed earlier are log drivers that the Amazon ECS container agent can communicate with by default. For tasks using the Fargate launch type, the supported log drivers are @awslogs@ , @splunk@ , and @awsfirelens@ . For tasks using the EC2 launch type, the supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ ,@syslog@ , @splunk@ , and @awsfirelens@ . For more information about using the @awslogs@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs Log Driver> in the /Amazon Elastic Container Service Developer Guide/ . For more information about using the @awsfirelens@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
logConfiguration
    :: LogDriver -- ^ 'lcLogDriver'
    -> LogConfiguration
logConfiguration pLogDriver_
  = LogConfiguration'{_lcOptions = Nothing,
                      _lcSecretOptions = Nothing,
                      _lcLogDriver = pLogDriver_}

-- | The configuration options to send to the log driver. This parameter requires version 1.19 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@ 
lcOptions :: Lens' LogConfiguration (HashMap Text Text)
lcOptions = lens _lcOptions (\ s a -> s{_lcOptions = a}) . _Default . _Map

-- | The secrets to pass to the log configuration. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data> in the /Amazon Elastic Container Service Developer Guide/ .
lcSecretOptions :: Lens' LogConfiguration [Secret]
lcSecretOptions = lens _lcSecretOptions (\ s a -> s{_lcSecretOptions = a}) . _Default . _Coerce

-- | The log driver to use for the container. The valid values listed earlier are log drivers that the Amazon ECS container agent can communicate with by default. For tasks using the Fargate launch type, the supported log drivers are @awslogs@ , @splunk@ , and @awsfirelens@ . For tasks using the EC2 launch type, the supported log drivers are @awslogs@ , @fluentd@ , @gelf@ , @json-file@ , @journald@ , @logentries@ ,@syslog@ , @splunk@ , and @awsfirelens@ . For more information about using the @awslogs@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_awslogs.html Using the awslogs Log Driver> in the /Amazon Elastic Container Service Developer Guide/ . For more information about using the @awsfirelens@ log driver, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom Log Routing> in the /Amazon Elastic Container Service Developer Guide/ .
lcLogDriver :: Lens' LogConfiguration LogDriver
lcLogDriver = lens _lcLogDriver (\ s a -> s{_lcLogDriver = a})

instance FromJSON LogConfiguration where
        parseJSON
          = withObject "LogConfiguration"
              (\ x ->
                 LogConfiguration' <$>
                   (x .:? "options" .!= mempty) <*>
                     (x .:? "secretOptions" .!= mempty)
                     <*> (x .: "logDriver"))

instance Hashable LogConfiguration where

instance NFData LogConfiguration where

instance ToJSON LogConfiguration where
        toJSON LogConfiguration'{..}
          = object
              (catMaybes
                 [("options" .=) <$> _lcOptions,
                  ("secretOptions" .=) <$> _lcSecretOptions,
                  Just ("logDriver" .= _lcLogDriver)])
