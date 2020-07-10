{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SpotProvisioningSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.SpotProvisioningSpecification where

import Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch specification for Spot instances in the instance fleet, which determines the defined duration and provisioning timeout behavior.
--
--
--
-- /See:/ 'spotProvisioningSpecification' smart constructor.
data SpotProvisioningSpecification = SpotProvisioningSpecification'{_spsBlockDurationMinutes
                                                                    ::
                                                                    !(Maybe
                                                                        Nat),
                                                                    _spsTimeoutDurationMinutes
                                                                    :: !Nat,
                                                                    _spsTimeoutAction
                                                                    ::
                                                                    !SpotProvisioningTimeoutAction}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'SpotProvisioningSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spsBlockDurationMinutes' - The defined duration for Spot instances (also known as Spot blocks) in minutes. When specified, the Spot instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot instance for termination and provides a Spot instance termination notice, which gives the instance a two-minute warning before it terminates. 
--
-- * 'spsTimeoutDurationMinutes' - The spot provisioning timeout period in minutes. If Spot instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
--
-- * 'spsTimeoutAction' - The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
spotProvisioningSpecification
    :: Natural -- ^ 'spsTimeoutDurationMinutes'
    -> SpotProvisioningTimeoutAction -- ^ 'spsTimeoutAction'
    -> SpotProvisioningSpecification
spotProvisioningSpecification
  pTimeoutDurationMinutes_ pTimeoutAction_
  = SpotProvisioningSpecification'{_spsBlockDurationMinutes
                                     = Nothing,
                                   _spsTimeoutDurationMinutes =
                                     _Nat # pTimeoutDurationMinutes_,
                                   _spsTimeoutAction = pTimeoutAction_}

-- | The defined duration for Spot instances (also known as Spot blocks) in minutes. When specified, the Spot instance does not terminate before the defined duration expires, and defined duration pricing for Spot instances applies. Valid values are 60, 120, 180, 240, 300, or 360. The duration period starts as soon as a Spot instance receives its instance ID. At the end of the duration, Amazon EC2 marks the Spot instance for termination and provides a Spot instance termination notice, which gives the instance a two-minute warning before it terminates. 
spsBlockDurationMinutes :: Lens' SpotProvisioningSpecification (Maybe Natural)
spsBlockDurationMinutes = lens _spsBlockDurationMinutes (\ s a -> s{_spsBlockDurationMinutes = a}) . mapping _Nat

-- | The spot provisioning timeout period in minutes. If Spot instances are not provisioned within this time period, the @TimeOutAction@ is taken. Minimum value is 5 and maximum value is 1440. The timeout applies only during initial provisioning, when the cluster is first created.
spsTimeoutDurationMinutes :: Lens' SpotProvisioningSpecification Natural
spsTimeoutDurationMinutes = lens _spsTimeoutDurationMinutes (\ s a -> s{_spsTimeoutDurationMinutes = a}) . _Nat

-- | The action to take when @TargetSpotCapacity@ has not been fulfilled when the @TimeoutDurationMinutes@ has expired; that is, when all Spot instances could not be provisioned within the Spot provisioning timeout. Valid values are @TERMINATE_CLUSTER@ and @SWITCH_TO_ON_DEMAND@ . SWITCH_TO_ON_DEMAND specifies that if no Spot instances are available, On-Demand Instances should be provisioned to fulfill any remaining Spot capacity.
spsTimeoutAction :: Lens' SpotProvisioningSpecification SpotProvisioningTimeoutAction
spsTimeoutAction = lens _spsTimeoutAction (\ s a -> s{_spsTimeoutAction = a})

instance FromJSON SpotProvisioningSpecification where
        parseJSON
          = withObject "SpotProvisioningSpecification"
              (\ x ->
                 SpotProvisioningSpecification' <$>
                   (x .:? "BlockDurationMinutes") <*>
                     (x .: "TimeoutDurationMinutes")
                     <*> (x .: "TimeoutAction"))

instance Hashable SpotProvisioningSpecification where

instance NFData SpotProvisioningSpecification where

instance ToJSON SpotProvisioningSpecification where
        toJSON SpotProvisioningSpecification'{..}
          = object
              (catMaybes
                 [("BlockDurationMinutes" .=) <$>
                    _spsBlockDurationMinutes,
                  Just
                    ("TimeoutDurationMinutes" .=
                       _spsTimeoutDurationMinutes),
                  Just ("TimeoutAction" .= _spsTimeoutAction)])
