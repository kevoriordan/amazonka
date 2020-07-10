{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Product (
    module Network.AWS.EMR.Types.Application,
    module Network.AWS.EMR.Types.AutoScalingPolicy,
    module Network.AWS.EMR.Types.AutoScalingPolicyDescription,
    module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason,
    module Network.AWS.EMR.Types.AutoScalingPolicyStatus,
    module Network.AWS.EMR.Types.BlockPublicAccessConfiguration,
    module Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata,
    module Network.AWS.EMR.Types.BootstrapActionConfig,
    module Network.AWS.EMR.Types.CancelStepsInfo,
    module Network.AWS.EMR.Types.CloudWatchAlarmDefinition,
    module Network.AWS.EMR.Types.Cluster,
    module Network.AWS.EMR.Types.ClusterStateChangeReason,
    module Network.AWS.EMR.Types.ClusterStatus,
    module Network.AWS.EMR.Types.ClusterSummary,
    module Network.AWS.EMR.Types.ClusterTimeline,
    module Network.AWS.EMR.Types.Command,
    module Network.AWS.EMR.Types.ComputeLimits,
    module Network.AWS.EMR.Types.Configuration,
    module Network.AWS.EMR.Types.EBSBlockDevice,
    module Network.AWS.EMR.Types.EBSBlockDeviceConfig,
    module Network.AWS.EMR.Types.EBSConfiguration,
    module Network.AWS.EMR.Types.EBSVolume,
    module Network.AWS.EMR.Types.EC2InstanceAttributes,
    module Network.AWS.EMR.Types.FailureDetails,
    module Network.AWS.EMR.Types.HadoopJARStepConfig,
    module Network.AWS.EMR.Types.HadoopStepConfig,
    module Network.AWS.EMR.Types.Instance,
    module Network.AWS.EMR.Types.InstanceFleet,
    module Network.AWS.EMR.Types.InstanceFleetConfig,
    module Network.AWS.EMR.Types.InstanceFleetModifyConfig,
    module Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications,
    module Network.AWS.EMR.Types.InstanceFleetStateChangeReason,
    module Network.AWS.EMR.Types.InstanceFleetStatus,
    module Network.AWS.EMR.Types.InstanceFleetTimeline,
    module Network.AWS.EMR.Types.InstanceGroup,
    module Network.AWS.EMR.Types.InstanceGroupConfig,
    module Network.AWS.EMR.Types.InstanceGroupModifyConfig,
    module Network.AWS.EMR.Types.InstanceGroupStateChangeReason,
    module Network.AWS.EMR.Types.InstanceGroupStatus,
    module Network.AWS.EMR.Types.InstanceGroupTimeline,
    module Network.AWS.EMR.Types.InstanceResizePolicy,
    module Network.AWS.EMR.Types.InstanceStateChangeReason,
    module Network.AWS.EMR.Types.InstanceStatus,
    module Network.AWS.EMR.Types.InstanceTimeline,
    module Network.AWS.EMR.Types.InstanceTypeConfig,
    module Network.AWS.EMR.Types.InstanceTypeSpecification,
    module Network.AWS.EMR.Types.JobFlowInstancesConfig,
    module Network.AWS.EMR.Types.KerberosAttributes,
    module Network.AWS.EMR.Types.KeyValue,
    module Network.AWS.EMR.Types.ManagedScalingPolicy,
    module Network.AWS.EMR.Types.MetricDimension,
    module Network.AWS.EMR.Types.PlacementType,
    module Network.AWS.EMR.Types.PortRange,
    module Network.AWS.EMR.Types.ScalingAction,
    module Network.AWS.EMR.Types.ScalingConstraints,
    module Network.AWS.EMR.Types.ScalingRule,
    module Network.AWS.EMR.Types.ScalingTrigger,
    module Network.AWS.EMR.Types.ScriptBootstrapActionConfig,
    module Network.AWS.EMR.Types.SecurityConfigurationSummary,
    module Network.AWS.EMR.Types.ShrinkPolicy,
    module Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration,
    module Network.AWS.EMR.Types.SpotProvisioningSpecification,
    module Network.AWS.EMR.Types.Step,
    module Network.AWS.EMR.Types.StepConfig,
    module Network.AWS.EMR.Types.StepStateChangeReason,
    module Network.AWS.EMR.Types.StepStatus,
    module Network.AWS.EMR.Types.StepSummary,
    module Network.AWS.EMR.Types.StepTimeline,
    module Network.AWS.EMR.Types.SupportedProductConfig,
    module Network.AWS.EMR.Types.Tag,
    module Network.AWS.EMR.Types.VolumeSpecification
  ) where

import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.AutoScalingPolicy
import Network.AWS.EMR.Types.AutoScalingPolicyDescription
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.BlockPublicAccessConfiguration
import Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
import Network.AWS.EMR.Types.BootstrapActionConfig
import Network.AWS.EMR.Types.CancelStepsInfo
import Network.AWS.EMR.Types.CloudWatchAlarmDefinition
import Network.AWS.EMR.Types.Cluster
import Network.AWS.EMR.Types.ClusterStateChangeReason
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.ClusterSummary
import Network.AWS.EMR.Types.ClusterTimeline
import Network.AWS.EMR.Types.Command
import Network.AWS.EMR.Types.ComputeLimits
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EBSBlockDevice
import Network.AWS.EMR.Types.EBSBlockDeviceConfig
import Network.AWS.EMR.Types.EBSConfiguration
import Network.AWS.EMR.Types.EBSVolume
import Network.AWS.EMR.Types.EC2InstanceAttributes
import Network.AWS.EMR.Types.FailureDetails
import Network.AWS.EMR.Types.HadoopJARStepConfig
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.Instance
import Network.AWS.EMR.Types.InstanceFleet
import Network.AWS.EMR.Types.InstanceFleetConfig
import Network.AWS.EMR.Types.InstanceFleetModifyConfig
import Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
import Network.AWS.EMR.Types.InstanceFleetStateChangeReason
import Network.AWS.EMR.Types.InstanceFleetStatus
import Network.AWS.EMR.Types.InstanceFleetTimeline
import Network.AWS.EMR.Types.InstanceGroup
import Network.AWS.EMR.Types.InstanceGroupConfig
import Network.AWS.EMR.Types.InstanceGroupModifyConfig
import Network.AWS.EMR.Types.InstanceGroupStateChangeReason
import Network.AWS.EMR.Types.InstanceGroupStatus
import Network.AWS.EMR.Types.InstanceGroupTimeline
import Network.AWS.EMR.Types.InstanceResizePolicy
import Network.AWS.EMR.Types.InstanceStateChangeReason
import Network.AWS.EMR.Types.InstanceStatus
import Network.AWS.EMR.Types.InstanceTimeline
import Network.AWS.EMR.Types.InstanceTypeConfig
import Network.AWS.EMR.Types.InstanceTypeSpecification
import Network.AWS.EMR.Types.JobFlowInstancesConfig
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.KeyValue
import Network.AWS.EMR.Types.ManagedScalingPolicy
import Network.AWS.EMR.Types.MetricDimension
import Network.AWS.EMR.Types.PlacementType
import Network.AWS.EMR.Types.PortRange
import Network.AWS.EMR.Types.ScalingAction
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import Network.AWS.EMR.Types.ScalingTrigger
import Network.AWS.EMR.Types.ScriptBootstrapActionConfig
import Network.AWS.EMR.Types.SecurityConfigurationSummary
import Network.AWS.EMR.Types.ShrinkPolicy
import Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
import Network.AWS.EMR.Types.SpotProvisioningSpecification
import Network.AWS.EMR.Types.Step
import Network.AWS.EMR.Types.StepConfig
import Network.AWS.EMR.Types.StepStateChangeReason
import Network.AWS.EMR.Types.StepStatus
import Network.AWS.EMR.Types.StepSummary
import Network.AWS.EMR.Types.StepTimeline
import Network.AWS.EMR.Types.SupportedProductConfig
import Network.AWS.EMR.Types.Tag
import Network.AWS.EMR.Types.VolumeSpecification
import Network.AWS.Lens
import Network.AWS.Prelude
