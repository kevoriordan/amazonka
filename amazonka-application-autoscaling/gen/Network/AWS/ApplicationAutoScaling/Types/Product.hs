{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Product (
    module Network.AWS.ApplicationAutoScaling.Types.Alarm,
    module Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification,
    module Network.AWS.ApplicationAutoScaling.Types.MetricDimension,
    module Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification,
    module Network.AWS.ApplicationAutoScaling.Types.ScalableTarget,
    module Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction,
    module Network.AWS.ApplicationAutoScaling.Types.ScalingActivity,
    module Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy,
    module Network.AWS.ApplicationAutoScaling.Types.ScheduledAction,
    module Network.AWS.ApplicationAutoScaling.Types.StepAdjustment,
    module Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration,
    module Network.AWS.ApplicationAutoScaling.Types.SuspendedState,
    module Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
  ) where

import Network.AWS.ApplicationAutoScaling.Types.Alarm
import Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.MetricDimension
import Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.ScalableTarget
import Network.AWS.ApplicationAutoScaling.Types.ScalableTargetAction
import Network.AWS.ApplicationAutoScaling.Types.ScalingActivity
import Network.AWS.ApplicationAutoScaling.Types.ScalingPolicy
import Network.AWS.ApplicationAutoScaling.Types.ScheduledAction
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
import Network.AWS.ApplicationAutoScaling.Types.SuspendedState
import Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
