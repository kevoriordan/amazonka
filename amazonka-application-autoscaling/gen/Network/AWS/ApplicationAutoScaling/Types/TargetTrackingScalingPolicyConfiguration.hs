{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.TargetTrackingScalingPolicyConfiguration where

import Network.AWS.ApplicationAutoScaling.Types.CustomizedMetricSpecification
import Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a target tracking scaling policy configuration to use with Application Auto Scaling.
--
--
--
-- /See:/ 'targetTrackingScalingPolicyConfiguration' smart constructor.
data TargetTrackingScalingPolicyConfiguration = TargetTrackingScalingPolicyConfiguration'{_ttspcPredefinedMetricSpecification
                                                                                          ::
                                                                                          !(Maybe
                                                                                              PredefinedMetricSpecification),
                                                                                          _ttspcScaleInCooldown
                                                                                          ::
                                                                                          !(Maybe
                                                                                              Int),
                                                                                          _ttspcCustomizedMetricSpecification
                                                                                          ::
                                                                                          !(Maybe
                                                                                              CustomizedMetricSpecification),
                                                                                          _ttspcDisableScaleIn
                                                                                          ::
                                                                                          !(Maybe
                                                                                              Bool),
                                                                                          _ttspcScaleOutCooldown
                                                                                          ::
                                                                                          !(Maybe
                                                                                              Int),
                                                                                          _ttspcTargetValue
                                                                                          ::
                                                                                          !Double}
                                                  deriving (Eq, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

-- | Creates a value of 'TargetTrackingScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttspcPredefinedMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttspcScaleInCooldown' - The amount of time, in seconds, after a scale-in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'ttspcCustomizedMetricSpecification' - A customized metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttspcDisableScaleIn' - Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
--
-- * 'ttspcScaleOutCooldown' - The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'ttspcTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
targetTrackingScalingPolicyConfiguration
    :: Double -- ^ 'ttspcTargetValue'
    -> TargetTrackingScalingPolicyConfiguration
targetTrackingScalingPolicyConfiguration
  pTargetValue_
  = TargetTrackingScalingPolicyConfiguration'{_ttspcPredefinedMetricSpecification
                                                = Nothing,
                                              _ttspcScaleInCooldown = Nothing,
                                              _ttspcCustomizedMetricSpecification
                                                = Nothing,
                                              _ttspcDisableScaleIn = Nothing,
                                              _ttspcScaleOutCooldown = Nothing,
                                              _ttspcTargetValue = pTargetValue_}

-- | A predefined metric. You can specify either a predefined metric or a customized metric.
ttspcPredefinedMetricSpecification :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe PredefinedMetricSpecification)
ttspcPredefinedMetricSpecification = lens _ttspcPredefinedMetricSpecification (\ s a -> s{_ttspcPredefinedMetricSpecification = a})

-- | The amount of time, in seconds, after a scale-in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
ttspcScaleInCooldown :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Int)
ttspcScaleInCooldown = lens _ttspcScaleInCooldown (\ s a -> s{_ttspcScaleInCooldown = a})

-- | A customized metric. You can specify either a predefined metric or a customized metric.
ttspcCustomizedMetricSpecification :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe CustomizedMetricSpecification)
ttspcCustomizedMetricSpecification = lens _ttspcCustomizedMetricSpecification (\ s a -> s{_ttspcCustomizedMetricSpecification = a})

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
ttspcDisableScaleIn :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Bool)
ttspcDisableScaleIn = lens _ttspcDisableScaleIn (\ s a -> s{_ttspcDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
ttspcScaleOutCooldown :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Int)
ttspcScaleOutCooldown = lens _ttspcScaleOutCooldown (\ s a -> s{_ttspcScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
ttspcTargetValue :: Lens' TargetTrackingScalingPolicyConfiguration Double
ttspcTargetValue = lens _ttspcTargetValue (\ s a -> s{_ttspcTargetValue = a})

instance FromJSON
           TargetTrackingScalingPolicyConfiguration
         where
        parseJSON
          = withObject
              "TargetTrackingScalingPolicyConfiguration"
              (\ x ->
                 TargetTrackingScalingPolicyConfiguration' <$>
                   (x .:? "PredefinedMetricSpecification") <*>
                     (x .:? "ScaleInCooldown")
                     <*> (x .:? "CustomizedMetricSpecification")
                     <*> (x .:? "DisableScaleIn")
                     <*> (x .:? "ScaleOutCooldown")
                     <*> (x .: "TargetValue"))

instance Hashable
           TargetTrackingScalingPolicyConfiguration
         where

instance NFData
           TargetTrackingScalingPolicyConfiguration
         where

instance ToJSON
           TargetTrackingScalingPolicyConfiguration
         where
        toJSON TargetTrackingScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("PredefinedMetricSpecification" .=) <$>
                    _ttspcPredefinedMetricSpecification,
                  ("ScaleInCooldown" .=) <$> _ttspcScaleInCooldown,
                  ("CustomizedMetricSpecification" .=) <$>
                    _ttspcCustomizedMetricSpecification,
                  ("DisableScaleIn" .=) <$> _ttspcDisableScaleIn,
                  ("ScaleOutCooldown" .=) <$> _ttspcScaleOutCooldown,
                  Just ("TargetValue" .= _ttspcTargetValue)])
