{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Product (
    module Network.AWS.ECS.Types.AWSVPCConfiguration,
    module Network.AWS.ECS.Types.Attachment,
    module Network.AWS.ECS.Types.AttachmentStateChange,
    module Network.AWS.ECS.Types.Attribute,
    module Network.AWS.ECS.Types.AutoScalingGroupProvider,
    module Network.AWS.ECS.Types.CapacityProvider,
    module Network.AWS.ECS.Types.CapacityProviderStrategyItem,
    module Network.AWS.ECS.Types.Cluster,
    module Network.AWS.ECS.Types.ClusterSetting,
    module Network.AWS.ECS.Types.Container,
    module Network.AWS.ECS.Types.ContainerDefinition,
    module Network.AWS.ECS.Types.ContainerDependency,
    module Network.AWS.ECS.Types.ContainerInstance,
    module Network.AWS.ECS.Types.ContainerOverride,
    module Network.AWS.ECS.Types.ContainerService,
    module Network.AWS.ECS.Types.ContainerStateChange,
    module Network.AWS.ECS.Types.Deployment,
    module Network.AWS.ECS.Types.DeploymentConfiguration,
    module Network.AWS.ECS.Types.DeploymentController,
    module Network.AWS.ECS.Types.Device,
    module Network.AWS.ECS.Types.DockerVolumeConfiguration,
    module Network.AWS.ECS.Types.EFSAuthorizationConfig,
    module Network.AWS.ECS.Types.EFSVolumeConfiguration,
    module Network.AWS.ECS.Types.Failure,
    module Network.AWS.ECS.Types.FirelensConfiguration,
    module Network.AWS.ECS.Types.HealthCheck,
    module Network.AWS.ECS.Types.HostEntry,
    module Network.AWS.ECS.Types.HostVolumeProperties,
    module Network.AWS.ECS.Types.InferenceAccelerator,
    module Network.AWS.ECS.Types.InferenceAcceleratorOverride,
    module Network.AWS.ECS.Types.KernelCapabilities,
    module Network.AWS.ECS.Types.KeyValuePair,
    module Network.AWS.ECS.Types.LinuxParameters,
    module Network.AWS.ECS.Types.LoadBalancer,
    module Network.AWS.ECS.Types.LogConfiguration,
    module Network.AWS.ECS.Types.ManagedScaling,
    module Network.AWS.ECS.Types.MountPoint,
    module Network.AWS.ECS.Types.NetworkBinding,
    module Network.AWS.ECS.Types.NetworkConfiguration,
    module Network.AWS.ECS.Types.NetworkInterface,
    module Network.AWS.ECS.Types.PlacementConstraint,
    module Network.AWS.ECS.Types.PlacementStrategy,
    module Network.AWS.ECS.Types.PlatformDevice,
    module Network.AWS.ECS.Types.PortMapping,
    module Network.AWS.ECS.Types.ProxyConfiguration,
    module Network.AWS.ECS.Types.RepositoryCredentials,
    module Network.AWS.ECS.Types.Resource,
    module Network.AWS.ECS.Types.ResourceRequirement,
    module Network.AWS.ECS.Types.Scale,
    module Network.AWS.ECS.Types.Secret,
    module Network.AWS.ECS.Types.ServiceEvent,
    module Network.AWS.ECS.Types.ServiceRegistry,
    module Network.AWS.ECS.Types.Setting,
    module Network.AWS.ECS.Types.SystemControl,
    module Network.AWS.ECS.Types.Tag,
    module Network.AWS.ECS.Types.Task,
    module Network.AWS.ECS.Types.TaskDefinition,
    module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint,
    module Network.AWS.ECS.Types.TaskOverride,
    module Network.AWS.ECS.Types.TaskSet,
    module Network.AWS.ECS.Types.Tmpfs,
    module Network.AWS.ECS.Types.Ulimit,
    module Network.AWS.ECS.Types.VersionInfo,
    module Network.AWS.ECS.Types.Volume,
    module Network.AWS.ECS.Types.VolumeFrom
  ) where

import Network.AWS.ECS.Types.AWSVPCConfiguration
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.AttachmentStateChange
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.CapacityProvider
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.Cluster
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.ContainerInstance
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.ContainerService
import Network.AWS.ECS.Types.ContainerStateChange
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.Failure
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.NetworkInterface
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PlatformDevice
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Setting
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.Task
import Network.AWS.ECS.Types.TaskDefinition
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.ECS.Types.Volume
import Network.AWS.ECS.Types.VolumeFrom
import Network.AWS.Lens
import Network.AWS.Prelude
