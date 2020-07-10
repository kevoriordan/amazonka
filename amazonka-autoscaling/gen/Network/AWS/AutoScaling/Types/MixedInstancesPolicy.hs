{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MixedInstancesPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MixedInstancesPolicy where

import Network.AWS.AutoScaling.Types.InstancesDistribution
import Network.AWS.AutoScaling.Types.LaunchTemplate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a mixed instances policy for an Auto Scaling group. With mixed instances, your Auto Scaling group can provision a combination of On-Demand Instances and Spot Instances across multiple instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling Groups with Multiple Instance Types and Purchase Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- You can create a mixed instances policy for a new Auto Scaling group, or you can create it for an existing group by updating the group to specify @MixedInstancesPolicy@ as the top-level parameter instead of a launch configuration or template. For more information, see 'CreateAutoScalingGroup' and 'UpdateAutoScalingGroup' .
--
--
-- /See:/ 'mixedInstancesPolicy' smart constructor.
data MixedInstancesPolicy = MixedInstancesPolicy'{_mipLaunchTemplate
                                                  :: !(Maybe LaunchTemplate),
                                                  _mipInstancesDistribution ::
                                                  !(Maybe
                                                      InstancesDistribution)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MixedInstancesPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mipLaunchTemplate' - The launch template and instance types (overrides). This parameter must be specified when creating a mixed instances policy.
--
-- * 'mipInstancesDistribution' - The instances distribution to use. If you leave this parameter unspecified, the value for each parameter in @InstancesDistribution@ uses a default value.
mixedInstancesPolicy
    :: MixedInstancesPolicy
mixedInstancesPolicy
  = MixedInstancesPolicy'{_mipLaunchTemplate = Nothing,
                          _mipInstancesDistribution = Nothing}

-- | The launch template and instance types (overrides). This parameter must be specified when creating a mixed instances policy.
mipLaunchTemplate :: Lens' MixedInstancesPolicy (Maybe LaunchTemplate)
mipLaunchTemplate = lens _mipLaunchTemplate (\ s a -> s{_mipLaunchTemplate = a})

-- | The instances distribution to use. If you leave this parameter unspecified, the value for each parameter in @InstancesDistribution@ uses a default value.
mipInstancesDistribution :: Lens' MixedInstancesPolicy (Maybe InstancesDistribution)
mipInstancesDistribution = lens _mipInstancesDistribution (\ s a -> s{_mipInstancesDistribution = a})

instance FromXML MixedInstancesPolicy where
        parseXML x
          = MixedInstancesPolicy' <$>
              (x .@? "LaunchTemplate") <*>
                (x .@? "InstancesDistribution")

instance Hashable MixedInstancesPolicy where

instance NFData MixedInstancesPolicy where

instance ToQuery MixedInstancesPolicy where
        toQuery MixedInstancesPolicy'{..}
          = mconcat
              ["LaunchTemplate" =: _mipLaunchTemplate,
               "InstancesDistribution" =: _mipInstancesDistribution]
