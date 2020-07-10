{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications where

import Network.AWS.EMR.Types.SpotProvisioningSpecification
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
--
--
--
-- /See:/ 'instanceFleetProvisioningSpecifications' smart constructor.
newtype InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'{_ifpsSpotSpecification
                                                                                           ::
                                                                                           SpotProvisioningSpecification}
                                                    deriving (Eq, Read, Show,
                                                              Data, Typeable,
                                                              Generic)

-- | Creates a value of 'InstanceFleetProvisioningSpecifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ifpsSpotSpecification' - The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
instanceFleetProvisioningSpecifications
    :: SpotProvisioningSpecification -- ^ 'ifpsSpotSpecification'
    -> InstanceFleetProvisioningSpecifications
instanceFleetProvisioningSpecifications
  pSpotSpecification_
  = InstanceFleetProvisioningSpecifications'{_ifpsSpotSpecification
                                               = pSpotSpecification_}

-- | The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.
ifpsSpotSpecification :: Lens' InstanceFleetProvisioningSpecifications SpotProvisioningSpecification
ifpsSpotSpecification = lens _ifpsSpotSpecification (\ s a -> s{_ifpsSpotSpecification = a})

instance FromJSON
           InstanceFleetProvisioningSpecifications
         where
        parseJSON
          = withObject
              "InstanceFleetProvisioningSpecifications"
              (\ x ->
                 InstanceFleetProvisioningSpecifications' <$>
                   (x .: "SpotSpecification"))

instance Hashable
           InstanceFleetProvisioningSpecifications
         where

instance NFData
           InstanceFleetProvisioningSpecifications
         where

instance ToJSON
           InstanceFleetProvisioningSpecifications
         where
        toJSON InstanceFleetProvisioningSpecifications'{..}
          = object
              (catMaybes
                 [Just
                    ("SpotSpecification" .= _ifpsSpotSpecification)])
