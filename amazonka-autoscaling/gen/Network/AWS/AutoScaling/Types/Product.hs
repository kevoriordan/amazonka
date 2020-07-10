{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Product (
    module Network.AWS.AutoScaling.Types.Activity,
    module Network.AWS.AutoScaling.Types.AdjustmentType,
    module Network.AWS.AutoScaling.Types.Alarm,
    module Network.AWS.AutoScaling.Types.AutoScalingGroup,
    module Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails,
    module Network.AWS.AutoScaling.Types.BlockDeviceMapping,
    module Network.AWS.AutoScaling.Types.CustomizedMetricSpecification,
    module Network.AWS.AutoScaling.Types.EBS,
    module Network.AWS.AutoScaling.Types.EnabledMetric,
    module Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest,
    module Network.AWS.AutoScaling.Types.Filter,
    module Network.AWS.AutoScaling.Types.Instance,
    module Network.AWS.AutoScaling.Types.InstanceMonitoring,
    module Network.AWS.AutoScaling.Types.InstancesDistribution,
    module Network.AWS.AutoScaling.Types.LaunchConfiguration,
    module Network.AWS.AutoScaling.Types.LaunchTemplate,
    module Network.AWS.AutoScaling.Types.LaunchTemplateOverrides,
    module Network.AWS.AutoScaling.Types.LaunchTemplateSpecification,
    module Network.AWS.AutoScaling.Types.LifecycleHook,
    module Network.AWS.AutoScaling.Types.LifecycleHookSpecification,
    module Network.AWS.AutoScaling.Types.LoadBalancerState,
    module Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState,
    module Network.AWS.AutoScaling.Types.MetricCollectionType,
    module Network.AWS.AutoScaling.Types.MetricDimension,
    module Network.AWS.AutoScaling.Types.MetricGranularityType,
    module Network.AWS.AutoScaling.Types.MixedInstancesPolicy,
    module Network.AWS.AutoScaling.Types.NotificationConfiguration,
    module Network.AWS.AutoScaling.Types.PredefinedMetricSpecification,
    module Network.AWS.AutoScaling.Types.ProcessType,
    module Network.AWS.AutoScaling.Types.ScalingPolicy,
    module Network.AWS.AutoScaling.Types.ScalingProcessQuery,
    module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction,
    module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest,
    module Network.AWS.AutoScaling.Types.StepAdjustment,
    module Network.AWS.AutoScaling.Types.SuspendedProcess,
    module Network.AWS.AutoScaling.Types.Tag,
    module Network.AWS.AutoScaling.Types.TagDescription,
    module Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
  ) where

import Network.AWS.AutoScaling.Types.Activity
import Network.AWS.AutoScaling.Types.AdjustmentType
import Network.AWS.AutoScaling.Types.Alarm
import Network.AWS.AutoScaling.Types.AutoScalingGroup
import Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
import Network.AWS.AutoScaling.Types.BlockDeviceMapping
import Network.AWS.AutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.AutoScaling.Types.EBS
import Network.AWS.AutoScaling.Types.EnabledMetric
import Network.AWS.AutoScaling.Types.FailedScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.Filter
import Network.AWS.AutoScaling.Types.Instance
import Network.AWS.AutoScaling.Types.InstanceMonitoring
import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchConfiguration
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
import Network.AWS.AutoScaling.Types.LaunchTemplateSpecification
import Network.AWS.AutoScaling.Types.LifecycleHook
import Network.AWS.AutoScaling.Types.LifecycleHookSpecification
import Network.AWS.AutoScaling.Types.LoadBalancerState
import Network.AWS.AutoScaling.Types.LoadBalancerTargetGroupState
import Network.AWS.AutoScaling.Types.MetricCollectionType
import Network.AWS.AutoScaling.Types.MetricDimension
import Network.AWS.AutoScaling.Types.MetricGranularityType
import Network.AWS.AutoScaling.Types.MixedInstancesPolicy
import Network.AWS.AutoScaling.Types.NotificationConfiguration
import Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.AutoScaling.Types.ProcessType
import Network.AWS.AutoScaling.Types.ScalingPolicy
import Network.AWS.AutoScaling.Types.ScalingProcessQuery
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
import Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
import Network.AWS.AutoScaling.Types.StepAdjustment
import Network.AWS.AutoScaling.Types.SuspendedProcess
import Network.AWS.AutoScaling.Types.Tag
import Network.AWS.AutoScaling.Types.TagDescription
import Network.AWS.AutoScaling.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
