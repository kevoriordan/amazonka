{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationTargetResponse where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a target Capacity Reservation.
--
--
--
-- /See:/ 'capacityReservationTargetResponse' smart constructor.
newtype CapacityReservationTargetResponse = CapacityReservationTargetResponse'{_cCapacityReservationId
                                                                               ::
                                                                               Maybe
                                                                                 Text}
                                              deriving (Eq, Read, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'CapacityReservationTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCapacityReservationId' - The ID of the Capacity Reservation.
capacityReservationTargetResponse
    :: CapacityReservationTargetResponse
capacityReservationTargetResponse
  = CapacityReservationTargetResponse'{_cCapacityReservationId
                                         = Nothing}

-- | The ID of the Capacity Reservation.
cCapacityReservationId :: Lens' CapacityReservationTargetResponse (Maybe Text)
cCapacityReservationId = lens _cCapacityReservationId (\ s a -> s{_cCapacityReservationId = a})

instance FromXML CapacityReservationTargetResponse
         where
        parseXML x
          = CapacityReservationTargetResponse' <$>
              (x .@? "capacityReservationId")

instance Hashable CapacityReservationTargetResponse
         where

instance NFData CapacityReservationTargetResponse
         where
