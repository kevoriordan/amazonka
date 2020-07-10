{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstancesDistribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.InstancesDistribution where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an instances distribution for an Auto Scaling group with 'MixedInstancesPolicy' .
--
--
-- The instances distribution specifies the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacity.
--
-- When you update @SpotAllocationStrategy@ , @SpotInstancePools@ , or @SpotMaxPrice@ , this update action does not deploy any changes across the running Amazon EC2 instances in the group. Your existing Spot Instances continue to run as long as the maximum price for those instances is higher than the current Spot price. When scale out occurs, Amazon EC2 Auto Scaling launches instances based on the new settings. When scale in occurs, Amazon EC2 Auto Scaling terminates instances according to the group's termination policies.
--
--
-- /See:/ 'instancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'{_idSpotAllocationStrategy
                                                    :: !(Maybe Text),
                                                    _idSpotInstancePools ::
                                                    !(Maybe Int),
                                                    _idSpotMaxPrice ::
                                                    !(Maybe Text),
                                                    _idOnDemandBaseCapacity ::
                                                    !(Maybe Int),
                                                    _idOnDemandAllocationStrategy
                                                    :: !(Maybe Text),
                                                    _idOnDemandPercentageAboveBaseCapacity
                                                    :: !(Maybe Int)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'InstancesDistribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idSpotAllocationStrategy' - Indicates how to allocate instances across Spot Instance pools. If the allocation strategy is @lowest-price@ , the Auto Scaling group launches instances using the Spot pools with the lowest price, and evenly allocates your instances across the number of Spot pools that you specify. If the allocation strategy is @capacity-optimized@ , the Auto Scaling group launches instances using Spot pools that are optimally chosen based on the available Spot capacity. The default Spot allocation strategy for calls that you make through the API, the AWS CLI, or the AWS SDKs is @lowest-price@ . The default Spot allocation strategy for the AWS Management Console is @capacity-optimized@ . Valid values: @lowest-price@ | @capacity-optimized@ 
--
-- * 'idSpotInstancePools' - The number of Spot Instance pools across which to allocate your Spot Instances. The Spot pools are determined from the different instance types in the Overrides array of 'LaunchTemplate' . Default if not set is 2. Used only when the Spot allocation strategy is @lowest-price@ . Valid Range: Minimum value of 1. Maximum value of 20.
--
-- * 'idSpotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot Instance. If you leave the value of this parameter blank (which is the default), the maximum Spot price is set at the On-Demand price. To remove a value that you previously set, include the parameter but leave the value blank.
--
-- * 'idOnDemandBaseCapacity' - The minimum amount of the Auto Scaling group's capacity that must be fulfilled by On-Demand Instances. This base portion is provisioned first as your group scales. Default if not set is 0. If you leave it set to 0, On-Demand Instances are launched as a percentage of the Auto Scaling group's desired capacity, per the @OnDemandPercentageAboveBaseCapacity@ setting.
--
-- * 'idOnDemandAllocationStrategy' - Indicates how to allocate instance types to fulfill On-Demand capacity. The only valid value is @prioritized@ , which is also the default value. This strategy uses the order of instance type overrides for the 'LaunchTemplate' to define the launch priority of each instance type. The first instance type in the array is prioritized higher than the last. If all your On-Demand capacity cannot be fulfilled using your highest priority instance, then the Auto Scaling groups launches the remaining capacity using the second priority instance type, and so on.
--
-- * 'idOnDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for your additional capacity beyond @OnDemandBaseCapacity@ . Default if not set is 100. If you leave it set to 100, the percentages are 100% for On-Demand Instances and 0% for Spot Instances. Valid Range: Minimum value of 0. Maximum value of 100.
instancesDistribution
    :: InstancesDistribution
instancesDistribution
  = InstancesDistribution'{_idSpotAllocationStrategy =
                             Nothing,
                           _idSpotInstancePools = Nothing,
                           _idSpotMaxPrice = Nothing,
                           _idOnDemandBaseCapacity = Nothing,
                           _idOnDemandAllocationStrategy = Nothing,
                           _idOnDemandPercentageAboveBaseCapacity = Nothing}

-- | Indicates how to allocate instances across Spot Instance pools. If the allocation strategy is @lowest-price@ , the Auto Scaling group launches instances using the Spot pools with the lowest price, and evenly allocates your instances across the number of Spot pools that you specify. If the allocation strategy is @capacity-optimized@ , the Auto Scaling group launches instances using Spot pools that are optimally chosen based on the available Spot capacity. The default Spot allocation strategy for calls that you make through the API, the AWS CLI, or the AWS SDKs is @lowest-price@ . The default Spot allocation strategy for the AWS Management Console is @capacity-optimized@ . Valid values: @lowest-price@ | @capacity-optimized@ 
idSpotAllocationStrategy :: Lens' InstancesDistribution (Maybe Text)
idSpotAllocationStrategy = lens _idSpotAllocationStrategy (\ s a -> s{_idSpotAllocationStrategy = a})

-- | The number of Spot Instance pools across which to allocate your Spot Instances. The Spot pools are determined from the different instance types in the Overrides array of 'LaunchTemplate' . Default if not set is 2. Used only when the Spot allocation strategy is @lowest-price@ . Valid Range: Minimum value of 1. Maximum value of 20.
idSpotInstancePools :: Lens' InstancesDistribution (Maybe Int)
idSpotInstancePools = lens _idSpotInstancePools (\ s a -> s{_idSpotInstancePools = a})

-- | The maximum price per unit hour that you are willing to pay for a Spot Instance. If you leave the value of this parameter blank (which is the default), the maximum Spot price is set at the On-Demand price. To remove a value that you previously set, include the parameter but leave the value blank.
idSpotMaxPrice :: Lens' InstancesDistribution (Maybe Text)
idSpotMaxPrice = lens _idSpotMaxPrice (\ s a -> s{_idSpotMaxPrice = a})

-- | The minimum amount of the Auto Scaling group's capacity that must be fulfilled by On-Demand Instances. This base portion is provisioned first as your group scales. Default if not set is 0. If you leave it set to 0, On-Demand Instances are launched as a percentage of the Auto Scaling group's desired capacity, per the @OnDemandPercentageAboveBaseCapacity@ setting.
idOnDemandBaseCapacity :: Lens' InstancesDistribution (Maybe Int)
idOnDemandBaseCapacity = lens _idOnDemandBaseCapacity (\ s a -> s{_idOnDemandBaseCapacity = a})

-- | Indicates how to allocate instance types to fulfill On-Demand capacity. The only valid value is @prioritized@ , which is also the default value. This strategy uses the order of instance type overrides for the 'LaunchTemplate' to define the launch priority of each instance type. The first instance type in the array is prioritized higher than the last. If all your On-Demand capacity cannot be fulfilled using your highest priority instance, then the Auto Scaling groups launches the remaining capacity using the second priority instance type, and so on.
idOnDemandAllocationStrategy :: Lens' InstancesDistribution (Maybe Text)
idOnDemandAllocationStrategy = lens _idOnDemandAllocationStrategy (\ s a -> s{_idOnDemandAllocationStrategy = a})

-- | Controls the percentages of On-Demand Instances and Spot Instances for your additional capacity beyond @OnDemandBaseCapacity@ . Default if not set is 100. If you leave it set to 100, the percentages are 100% for On-Demand Instances and 0% for Spot Instances. Valid Range: Minimum value of 0. Maximum value of 100.
idOnDemandPercentageAboveBaseCapacity :: Lens' InstancesDistribution (Maybe Int)
idOnDemandPercentageAboveBaseCapacity = lens _idOnDemandPercentageAboveBaseCapacity (\ s a -> s{_idOnDemandPercentageAboveBaseCapacity = a})

instance FromXML InstancesDistribution where
        parseXML x
          = InstancesDistribution' <$>
              (x .@? "SpotAllocationStrategy") <*>
                (x .@? "SpotInstancePools")
                <*> (x .@? "SpotMaxPrice")
                <*> (x .@? "OnDemandBaseCapacity")
                <*> (x .@? "OnDemandAllocationStrategy")
                <*> (x .@? "OnDemandPercentageAboveBaseCapacity")

instance Hashable InstancesDistribution where

instance NFData InstancesDistribution where

instance ToQuery InstancesDistribution where
        toQuery InstancesDistribution'{..}
          = mconcat
              ["SpotAllocationStrategy" =:
                 _idSpotAllocationStrategy,
               "SpotInstancePools" =: _idSpotInstancePools,
               "SpotMaxPrice" =: _idSpotMaxPrice,
               "OnDemandBaseCapacity" =: _idOnDemandBaseCapacity,
               "OnDemandAllocationStrategy" =:
                 _idOnDemandAllocationStrategy,
               "OnDemandPercentageAboveBaseCapacity" =:
                 _idOnDemandPercentageAboveBaseCapacity]
