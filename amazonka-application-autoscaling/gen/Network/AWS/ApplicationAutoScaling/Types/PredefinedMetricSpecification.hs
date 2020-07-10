{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.PredefinedMetricSpecification where

import Network.AWS.ApplicationAutoScaling.Types.MetricType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a predefined metric for a target tracking scaling policy to use with Application Auto Scaling.
--
--
-- Only the AWS services that you're using send metrics to Amazon CloudWatch. To determine whether a desired metric already exists by looking up its namespace and dimension using the CloudWatch metrics dashboard in the console, follow the procedure in <https://docs.aws.amazon.com/autoscaling/application/userguide/monitoring-cloudwatch.html Building Dashboards with CloudWatch> in the /Application Auto Scaling User Guide/ .
--
--
-- /See:/ 'predefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification = PredefinedMetricSpecification'{_pmsResourceLabel
                                                                    ::
                                                                    !(Maybe
                                                                        Text),
                                                                    _pmsPredefinedMetricType
                                                                    ::
                                                                    !MetricType}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'PredefinedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Spot Fleet request or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'pmsPredefinedMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Spot Fleet requests and ECS services.
predefinedMetricSpecification
    :: MetricType -- ^ 'pmsPredefinedMetricType'
    -> PredefinedMetricSpecification
predefinedMetricSpecification pPredefinedMetricType_
  = PredefinedMetricSpecification'{_pmsResourceLabel =
                                     Nothing,
                                   _pmsPredefinedMetricType =
                                     pPredefinedMetricType_}

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Spot Fleet request or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
pmsResourceLabel :: Lens' PredefinedMetricSpecification (Maybe Text)
pmsResourceLabel = lens _pmsResourceLabel (\ s a -> s{_pmsResourceLabel = a})

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Spot Fleet requests and ECS services.
pmsPredefinedMetricType :: Lens' PredefinedMetricSpecification MetricType
pmsPredefinedMetricType = lens _pmsPredefinedMetricType (\ s a -> s{_pmsPredefinedMetricType = a})

instance FromJSON PredefinedMetricSpecification where
        parseJSON
          = withObject "PredefinedMetricSpecification"
              (\ x ->
                 PredefinedMetricSpecification' <$>
                   (x .:? "ResourceLabel") <*>
                     (x .: "PredefinedMetricType"))

instance Hashable PredefinedMetricSpecification where

instance NFData PredefinedMetricSpecification where

instance ToJSON PredefinedMetricSpecification where
        toJSON PredefinedMetricSpecification'{..}
          = object
              (catMaybes
                 [("ResourceLabel" .=) <$> _pmsResourceLabel,
                  Just
                    ("PredefinedMetricType" .=
                       _pmsPredefinedMetricType)])
