{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotMarketOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotMarketOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'spotMarketOptions' smart constructor.
data SpotMarketOptions = SpotMarketOptions'{_smoBlockDurationMinutes
                                            :: !(Maybe Int),
                                            _smoInstanceInterruptionBehavior ::
                                            !(Maybe
                                                InstanceInterruptionBehavior),
                                            _smoValidUntil :: !(Maybe ISO8601),
                                            _smoSpotInstanceType ::
                                            !(Maybe SpotInstanceType),
                                            _smoMaxPrice :: !(Maybe Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smoBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'smoInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'smoValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
--
-- * 'smoSpotInstanceType' - The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
--
-- * 'smoMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
spotMarketOptions
    :: SpotMarketOptions
spotMarketOptions
  = SpotMarketOptions'{_smoBlockDurationMinutes =
                         Nothing,
                       _smoInstanceInterruptionBehavior = Nothing,
                       _smoValidUntil = Nothing,
                       _smoSpotInstanceType = Nothing,
                       _smoMaxPrice = Nothing}

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
smoBlockDurationMinutes :: Lens' SpotMarketOptions (Maybe Int)
smoBlockDurationMinutes = lens _smoBlockDurationMinutes (\ s a -> s{_smoBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
smoInstanceInterruptionBehavior :: Lens' SpotMarketOptions (Maybe InstanceInterruptionBehavior)
smoInstanceInterruptionBehavior = lens _smoInstanceInterruptionBehavior (\ s a -> s{_smoInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
smoValidUntil :: Lens' SpotMarketOptions (Maybe UTCTime)
smoValidUntil = lens _smoValidUntil (\ s a -> s{_smoValidUntil = a}) . mapping _Time

-- | The Spot Instance request type. For <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances> , persistent Spot Instance requests are only supported when __InstanceInterruptionBehavior__ is set to either @hibernate@ or @stop@ .
smoSpotInstanceType :: Lens' SpotMarketOptions (Maybe SpotInstanceType)
smoSpotInstanceType = lens _smoSpotInstanceType (\ s a -> s{_smoSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances. The default is the On-Demand price.
smoMaxPrice :: Lens' SpotMarketOptions (Maybe Text)
smoMaxPrice = lens _smoMaxPrice (\ s a -> s{_smoMaxPrice = a})

instance Hashable SpotMarketOptions where

instance NFData SpotMarketOptions where

instance ToQuery SpotMarketOptions where
        toQuery SpotMarketOptions'{..}
          = mconcat
              ["BlockDurationMinutes" =: _smoBlockDurationMinutes,
               "InstanceInterruptionBehavior" =:
                 _smoInstanceInterruptionBehavior,
               "ValidUntil" =: _smoValidUntil,
               "SpotInstanceType" =: _smoSpotInstanceType,
               "MaxPrice" =: _smoMaxPrice]
