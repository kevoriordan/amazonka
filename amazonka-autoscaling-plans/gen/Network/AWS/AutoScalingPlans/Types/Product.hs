{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.Product (
    module Network.AWS.AutoScalingPlans.Types.ApplicationSource,
    module Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification,
    module Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification,
    module Network.AWS.AutoScalingPlans.Types.Datapoint,
    module Network.AWS.AutoScalingPlans.Types.MetricDimension,
    module Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification,
    module Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification,
    module Network.AWS.AutoScalingPlans.Types.ScalingInstruction,
    module Network.AWS.AutoScalingPlans.Types.ScalingPlan,
    module Network.AWS.AutoScalingPlans.Types.ScalingPlanResource,
    module Network.AWS.AutoScalingPlans.Types.ScalingPolicy,
    module Network.AWS.AutoScalingPlans.Types.TagFilter,
    module Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
  ) where

import Network.AWS.AutoScalingPlans.Types.ApplicationSource
import Network.AWS.AutoScalingPlans.Types.CustomizedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.CustomizedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.Datapoint
import Network.AWS.AutoScalingPlans.Types.MetricDimension
import Network.AWS.AutoScalingPlans.Types.PredefinedLoadMetricSpecification
import Network.AWS.AutoScalingPlans.Types.PredefinedScalingMetricSpecification
import Network.AWS.AutoScalingPlans.Types.ScalingInstruction
import Network.AWS.AutoScalingPlans.Types.ScalingPlan
import Network.AWS.AutoScalingPlans.Types.ScalingPlanResource
import Network.AWS.AutoScalingPlans.Types.ScalingPolicy
import Network.AWS.AutoScalingPlans.Types.TagFilter
import Network.AWS.AutoScalingPlans.Types.TargetTrackingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
