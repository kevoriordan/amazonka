{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Product (
    module Network.AWS.OpsWorks.Types.AgentVersion,
    module Network.AWS.OpsWorks.Types.App,
    module Network.AWS.OpsWorks.Types.AutoScalingThresholds,
    module Network.AWS.OpsWorks.Types.BlockDeviceMapping,
    module Network.AWS.OpsWorks.Types.ChefConfiguration,
    module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration,
    module Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream,
    module Network.AWS.OpsWorks.Types.Command,
    module Network.AWS.OpsWorks.Types.DataSource,
    module Network.AWS.OpsWorks.Types.Deployment,
    module Network.AWS.OpsWorks.Types.DeploymentCommand,
    module Network.AWS.OpsWorks.Types.EBSBlockDevice,
    module Network.AWS.OpsWorks.Types.EcsCluster,
    module Network.AWS.OpsWorks.Types.ElasticIP,
    module Network.AWS.OpsWorks.Types.ElasticLoadBalancer,
    module Network.AWS.OpsWorks.Types.EnvironmentVariable,
    module Network.AWS.OpsWorks.Types.Instance,
    module Network.AWS.OpsWorks.Types.InstanceIdentity,
    module Network.AWS.OpsWorks.Types.InstancesCount,
    module Network.AWS.OpsWorks.Types.Layer,
    module Network.AWS.OpsWorks.Types.LifecycleEventConfiguration,
    module Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration,
    module Network.AWS.OpsWorks.Types.OperatingSystem,
    module Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager,
    module Network.AWS.OpsWorks.Types.Permission,
    module Network.AWS.OpsWorks.Types.RAIdArray,
    module Network.AWS.OpsWorks.Types.RDSDBInstance,
    module Network.AWS.OpsWorks.Types.Recipes,
    module Network.AWS.OpsWorks.Types.ReportedOS,
    module Network.AWS.OpsWorks.Types.SSLConfiguration,
    module Network.AWS.OpsWorks.Types.SelfUserProfile,
    module Network.AWS.OpsWorks.Types.ServiceError',
    module Network.AWS.OpsWorks.Types.ShutdownEventConfiguration,
    module Network.AWS.OpsWorks.Types.Source,
    module Network.AWS.OpsWorks.Types.Stack,
    module Network.AWS.OpsWorks.Types.StackConfigurationManager,
    module Network.AWS.OpsWorks.Types.StackSummary,
    module Network.AWS.OpsWorks.Types.TemporaryCredential,
    module Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration,
    module Network.AWS.OpsWorks.Types.UserProfile,
    module Network.AWS.OpsWorks.Types.Volume,
    module Network.AWS.OpsWorks.Types.VolumeConfiguration,
    module Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
  ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.AgentVersion
import Network.AWS.OpsWorks.Types.App
import Network.AWS.OpsWorks.Types.AutoScalingThresholds
import Network.AWS.OpsWorks.Types.BlockDeviceMapping
import Network.AWS.OpsWorks.Types.ChefConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import Network.AWS.OpsWorks.Types.Command
import Network.AWS.OpsWorks.Types.DataSource
import Network.AWS.OpsWorks.Types.Deployment
import Network.AWS.OpsWorks.Types.DeploymentCommand
import Network.AWS.OpsWorks.Types.EBSBlockDevice
import Network.AWS.OpsWorks.Types.EcsCluster
import Network.AWS.OpsWorks.Types.ElasticIP
import Network.AWS.OpsWorks.Types.ElasticLoadBalancer
import Network.AWS.OpsWorks.Types.EnvironmentVariable
import Network.AWS.OpsWorks.Types.Instance
import Network.AWS.OpsWorks.Types.InstanceIdentity
import Network.AWS.OpsWorks.Types.InstancesCount
import Network.AWS.OpsWorks.Types.Layer
import Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
import Network.AWS.OpsWorks.Types.LoadBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.OperatingSystem
import Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
import Network.AWS.OpsWorks.Types.Permission
import Network.AWS.OpsWorks.Types.RAIdArray
import Network.AWS.OpsWorks.Types.RDSDBInstance
import Network.AWS.OpsWorks.Types.Recipes
import Network.AWS.OpsWorks.Types.ReportedOS
import Network.AWS.OpsWorks.Types.SSLConfiguration
import Network.AWS.OpsWorks.Types.SelfUserProfile
import Network.AWS.OpsWorks.Types.ServiceError'
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import Network.AWS.OpsWorks.Types.Source
import Network.AWS.OpsWorks.Types.Stack
import Network.AWS.OpsWorks.Types.StackConfigurationManager
import Network.AWS.OpsWorks.Types.StackSummary
import Network.AWS.OpsWorks.Types.TemporaryCredential
import Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
import Network.AWS.OpsWorks.Types.UserProfile
import Network.AWS.OpsWorks.Types.Volume
import Network.AWS.OpsWorks.Types.VolumeConfiguration
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import Network.AWS.Prelude
