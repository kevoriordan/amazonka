{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ComputeLimits where

import Network.AWS.EMR.Types.ComputeLimitsUnitType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster can not be above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
--
--
-- /See:/ 'computeLimits' smart constructor.
data ComputeLimits = ComputeLimits'{_clMaximumOnDemandCapacityUnits
                                    :: !(Maybe Int),
                                    _clUnitType :: !ComputeLimitsUnitType,
                                    _clMinimumCapacityUnits :: !Int,
                                    _clMaximumCapacityUnits :: !Int}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clMaximumOnDemandCapacityUnits' - The upper boundary of on-demand EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. The on-demand units are not allowed to scale beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- * 'clUnitType' - The unit type used for specifying a managed scaling policy. 
--
-- * 'clMinimumCapacityUnits' - The lower boundary of EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
--
-- * 'clMaximumCapacityUnits' - The upper boundary of EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
computeLimits
    :: ComputeLimitsUnitType -- ^ 'clUnitType'
    -> Int -- ^ 'clMinimumCapacityUnits'
    -> Int -- ^ 'clMaximumCapacityUnits'
    -> ComputeLimits
computeLimits pUnitType_ pMinimumCapacityUnits_
  pMaximumCapacityUnits_
  = ComputeLimits'{_clMaximumOnDemandCapacityUnits =
                     Nothing,
                   _clUnitType = pUnitType_,
                   _clMinimumCapacityUnits = pMinimumCapacityUnits_,
                   _clMaximumCapacityUnits = pMaximumCapacityUnits_}

-- | The upper boundary of on-demand EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. The on-demand units are not allowed to scale beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
clMaximumOnDemandCapacityUnits :: Lens' ComputeLimits (Maybe Int)
clMaximumOnDemandCapacityUnits = lens _clMaximumOnDemandCapacityUnits (\ s a -> s{_clMaximumOnDemandCapacityUnits = a})

-- | The unit type used for specifying a managed scaling policy. 
clUnitType :: Lens' ComputeLimits ComputeLimitsUnitType
clUnitType = lens _clUnitType (\ s a -> s{_clUnitType = a})

-- | The lower boundary of EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
clMinimumCapacityUnits :: Lens' ComputeLimits Int
clMinimumCapacityUnits = lens _clMinimumCapacityUnits (\ s a -> s{_clMinimumCapacityUnits = a})

-- | The upper boundary of EC2 units. It is measured through VCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration. 
clMaximumCapacityUnits :: Lens' ComputeLimits Int
clMaximumCapacityUnits = lens _clMaximumCapacityUnits (\ s a -> s{_clMaximumCapacityUnits = a})

instance FromJSON ComputeLimits where
        parseJSON
          = withObject "ComputeLimits"
              (\ x ->
                 ComputeLimits' <$>
                   (x .:? "MaximumOnDemandCapacityUnits") <*>
                     (x .: "UnitType")
                     <*> (x .: "MinimumCapacityUnits")
                     <*> (x .: "MaximumCapacityUnits"))

instance Hashable ComputeLimits where

instance NFData ComputeLimits where

instance ToJSON ComputeLimits where
        toJSON ComputeLimits'{..}
          = object
              (catMaybes
                 [("MaximumOnDemandCapacityUnits" .=) <$>
                    _clMaximumOnDemandCapacityUnits,
                  Just ("UnitType" .= _clUnitType),
                  Just
                    ("MinimumCapacityUnits" .= _clMinimumCapacityUnits),
                  Just
                    ("MaximumCapacityUnits" .= _clMaximumCapacityUnits)])
