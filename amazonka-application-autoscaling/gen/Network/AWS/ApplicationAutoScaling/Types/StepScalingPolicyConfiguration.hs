{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.StepScalingPolicyConfiguration where

import Network.AWS.ApplicationAutoScaling.Types.AdjustmentType
import Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
import Network.AWS.ApplicationAutoScaling.Types.StepAdjustment
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a step scaling policy configuration to use with Application Auto Scaling.
--
--
--
-- /See:/ 'stepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration = StepScalingPolicyConfiguration'{_sspcStepAdjustments
                                                                      ::
                                                                      !(Maybe
                                                                          [StepAdjustment]),
                                                                      _sspcAdjustmentType
                                                                      ::
                                                                      !(Maybe
                                                                          AdjustmentType),
                                                                      _sspcCooldown
                                                                      ::
                                                                      !(Maybe
                                                                          Int),
                                                                      _sspcMetricAggregationType
                                                                      ::
                                                                      !(Maybe
                                                                          MetricAggregationType),
                                                                      _sspcMinAdjustmentMagnitude
                                                                      ::
                                                                      !(Maybe
                                                                          Int)}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'StepScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcAdjustmentType' - Specifies whether the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is an absolute number or a percentage of the current capacity.  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcCooldown' - The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale-out policies, while the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a cooldown period of 5 minutes starts. During the cooldown period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale-out action are considered part of that capacity and only 1 additional task is added to the desired count. For scale-in policies, the cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'sspcMetricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- * 'sspcMinAdjustmentMagnitude' - The minimum value to scale by when scaling by percentages. For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks. Valid only if the adjustment type is @PercentChangeInCapacity@ . 
stepScalingPolicyConfiguration
    :: StepScalingPolicyConfiguration
stepScalingPolicyConfiguration
  = StepScalingPolicyConfiguration'{_sspcStepAdjustments
                                      = Nothing,
                                    _sspcAdjustmentType = Nothing,
                                    _sspcCooldown = Nothing,
                                    _sspcMetricAggregationType = Nothing,
                                    _sspcMinAdjustmentMagnitude = Nothing}

-- | A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
sspcStepAdjustments :: Lens' StepScalingPolicyConfiguration [StepAdjustment]
sspcStepAdjustments = lens _sspcStepAdjustments (\ s a -> s{_sspcStepAdjustments = a}) . _Default . _Coerce

-- | Specifies whether the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is an absolute number or a percentage of the current capacity.  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
sspcAdjustmentType :: Lens' StepScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\ s a -> s{_sspcAdjustmentType = a})

-- | The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale-out policies, while the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a cooldown period of 5 minutes starts. During the cooldown period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale-out action are considered part of that capacity and only 1 additional task is added to the desired count. For scale-in policies, the cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
sspcCooldown :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcCooldown = lens _sspcCooldown (\ s a -> s{_sspcCooldown = a})

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
sspcMetricAggregationType :: Lens' StepScalingPolicyConfiguration (Maybe MetricAggregationType)
sspcMetricAggregationType = lens _sspcMetricAggregationType (\ s a -> s{_sspcMetricAggregationType = a})

-- | The minimum value to scale by when scaling by percentages. For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks. Valid only if the adjustment type is @PercentChangeInCapacity@ . 
sspcMinAdjustmentMagnitude :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcMinAdjustmentMagnitude = lens _sspcMinAdjustmentMagnitude (\ s a -> s{_sspcMinAdjustmentMagnitude = a})

instance FromJSON StepScalingPolicyConfiguration
         where
        parseJSON
          = withObject "StepScalingPolicyConfiguration"
              (\ x ->
                 StepScalingPolicyConfiguration' <$>
                   (x .:? "StepAdjustments" .!= mempty) <*>
                     (x .:? "AdjustmentType")
                     <*> (x .:? "Cooldown")
                     <*> (x .:? "MetricAggregationType")
                     <*> (x .:? "MinAdjustmentMagnitude"))

instance Hashable StepScalingPolicyConfiguration
         where

instance NFData StepScalingPolicyConfiguration where

instance ToJSON StepScalingPolicyConfiguration where
        toJSON StepScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("StepAdjustments" .=) <$> _sspcStepAdjustments,
                  ("AdjustmentType" .=) <$> _sspcAdjustmentType,
                  ("Cooldown" .=) <$> _sspcCooldown,
                  ("MetricAggregationType" .=) <$>
                    _sspcMetricAggregationType,
                  ("MinAdjustmentMagnitude" .=) <$>
                    _sspcMinAdjustmentMagnitude])
