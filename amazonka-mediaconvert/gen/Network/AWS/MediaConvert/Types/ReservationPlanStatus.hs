{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ReservationPlanStatus (
  ReservationPlanStatus (
    ..
    , Active
    , Expired
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
data ReservationPlanStatus = ReservationPlanStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Active :: ReservationPlanStatus
pattern Active = ReservationPlanStatus' "ACTIVE"

pattern Expired :: ReservationPlanStatus
pattern Expired = ReservationPlanStatus' "EXPIRED"

{-# COMPLETE
  Active,
  Expired,
  ReservationPlanStatus' #-}

instance FromText ReservationPlanStatus where
    parser = (ReservationPlanStatus' . mk) <$> takeText

instance ToText ReservationPlanStatus where
    toText (ReservationPlanStatus' ci) = original ci

-- | Represents an enum of /known/ $ReservationPlanStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationPlanStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Expired
        _ -> (error . showText) $ "Unknown index for ReservationPlanStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Expired -> 1
        ReservationPlanStatus' name -> (error . showText) $ "Unknown ReservationPlanStatus: " <> original name

-- | Represents the bounds of /known/ $ReservationPlanStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationPlanStatus where
    minBound = Active
    maxBound = Expired

instance Hashable     ReservationPlanStatus
instance NFData       ReservationPlanStatus
instance ToByteString ReservationPlanStatus
instance ToQuery      ReservationPlanStatus
instance ToHeader     ReservationPlanStatus

instance FromJSON ReservationPlanStatus where
    parseJSON = parseJSONText "ReservationPlanStatus"
