{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LaunchTemplateOverrides
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LaunchTemplateOverrides where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an override for a launch template.
--
--
--
-- /See:/ 'launchTemplateOverrides' smart constructor.
data LaunchTemplateOverrides = LaunchTemplateOverrides'{_ltoWeightedCapacity
                                                        :: !(Maybe Text),
                                                        _ltoInstanceType ::
                                                        !(Maybe Text)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'LaunchTemplateOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltoWeightedCapacity' - The number of capacity units, which gives the instance type a proportional weight to other instance types. For example, larger instance types are generally weighted more than smaller instance types. These are the same units that you chose to set the desired capacity in terms of instances, or a performance attribute such as vCPUs, memory, or I/O. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 1. Maximum value of 999.
--
-- * 'ltoInstanceType' - The instance type. For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon Elastic Compute Cloud User Guide./ 
launchTemplateOverrides
    :: LaunchTemplateOverrides
launchTemplateOverrides
  = LaunchTemplateOverrides'{_ltoWeightedCapacity =
                               Nothing,
                             _ltoInstanceType = Nothing}

-- | The number of capacity units, which gives the instance type a proportional weight to other instance types. For example, larger instance types are generally weighted more than smaller instance types. These are the same units that you chose to set the desired capacity in terms of instances, or a performance attribute such as vCPUs, memory, or I/O. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-weighting.html Instance Weighting for Amazon EC2 Auto Scaling> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 1. Maximum value of 999.
ltoWeightedCapacity :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoWeightedCapacity = lens _ltoWeightedCapacity (\ s a -> s{_ltoWeightedCapacity = a})

-- | The instance type. For information about available instance types, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types> in the /Amazon Elastic Compute Cloud User Guide./ 
ltoInstanceType :: Lens' LaunchTemplateOverrides (Maybe Text)
ltoInstanceType = lens _ltoInstanceType (\ s a -> s{_ltoInstanceType = a})

instance FromXML LaunchTemplateOverrides where
        parseXML x
          = LaunchTemplateOverrides' <$>
              (x .@? "WeightedCapacity") <*> (x .@? "InstanceType")

instance Hashable LaunchTemplateOverrides where

instance NFData LaunchTemplateOverrides where

instance ToQuery LaunchTemplateOverrides where
        toQuery LaunchTemplateOverrides'{..}
          = mconcat
              ["WeightedCapacity" =: _ltoWeightedCapacity,
               "InstanceType" =: _ltoInstanceType]
