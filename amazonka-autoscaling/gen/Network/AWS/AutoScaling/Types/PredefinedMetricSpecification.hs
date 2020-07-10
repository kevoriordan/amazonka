{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.PredefinedMetricSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.PredefinedMetricSpecification where

import Network.AWS.AutoScaling.Types.MetricType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a predefined metric for a target tracking scaling policy to use with Amazon EC2 Auto Scaling.
--
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
-- * 'pmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Auto Scaling group. The format is @app//load-balancer-name/ //load-balancer-id/ /targetgroup//target-group-name/ //target-group-id/ @ , where      * @app//load-balancer-name/ //load-balancer-id/ @ is the final portion of the load balancer ARN, and     * @targetgroup//target-group-name/ //target-group-id/ @ is the final portion of the target group ARN.
--
-- * 'pmsPredefinedMetricType' - The metric type. The following predefined metrics are available:     * @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto Scaling group.     * @ASGAverageNetworkIn@ - Average number of bytes received on all network interfaces by the Auto Scaling group.     * @ASGAverageNetworkOut@ - Average number of bytes sent out on all network interfaces by the Auto Scaling group.     * @ALBRequestCountPerTarget@ - Number of requests completed per target in an Application Load Balancer target group.
predefinedMetricSpecification
    :: MetricType -- ^ 'pmsPredefinedMetricType'
    -> PredefinedMetricSpecification
predefinedMetricSpecification pPredefinedMetricType_
  = PredefinedMetricSpecification'{_pmsResourceLabel =
                                     Nothing,
                                   _pmsPredefinedMetricType =
                                     pPredefinedMetricType_}

-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Auto Scaling group. The format is @app//load-balancer-name/ //load-balancer-id/ /targetgroup//target-group-name/ //target-group-id/ @ , where      * @app//load-balancer-name/ //load-balancer-id/ @ is the final portion of the load balancer ARN, and     * @targetgroup//target-group-name/ //target-group-id/ @ is the final portion of the target group ARN.
pmsResourceLabel :: Lens' PredefinedMetricSpecification (Maybe Text)
pmsResourceLabel = lens _pmsResourceLabel (\ s a -> s{_pmsResourceLabel = a})

-- | The metric type. The following predefined metrics are available:     * @ASGAverageCPUUtilization@ - Average CPU utilization of the Auto Scaling group.     * @ASGAverageNetworkIn@ - Average number of bytes received on all network interfaces by the Auto Scaling group.     * @ASGAverageNetworkOut@ - Average number of bytes sent out on all network interfaces by the Auto Scaling group.     * @ALBRequestCountPerTarget@ - Number of requests completed per target in an Application Load Balancer target group.
pmsPredefinedMetricType :: Lens' PredefinedMetricSpecification MetricType
pmsPredefinedMetricType = lens _pmsPredefinedMetricType (\ s a -> s{_pmsPredefinedMetricType = a})

instance FromXML PredefinedMetricSpecification where
        parseXML x
          = PredefinedMetricSpecification' <$>
              (x .@? "ResourceLabel") <*>
                (x .@ "PredefinedMetricType")

instance Hashable PredefinedMetricSpecification where

instance NFData PredefinedMetricSpecification where

instance ToQuery PredefinedMetricSpecification where
        toQuery PredefinedMetricSpecification'{..}
          = mconcat
              ["ResourceLabel" =: _pmsResourceLabel,
               "PredefinedMetricType" =: _pmsPredefinedMetricType]
