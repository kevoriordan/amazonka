{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OfferingDurationUnits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.OfferingDurationUnits (
  OfferingDurationUnits (
    ..
    , Months
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Units for duration, e.g. 'MONTHS'
data OfferingDurationUnits = OfferingDurationUnits' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Months :: OfferingDurationUnits
pattern Months = OfferingDurationUnits' "MONTHS"

{-# COMPLETE
  Months,
  OfferingDurationUnits' #-}

instance FromText OfferingDurationUnits where
    parser = (OfferingDurationUnits' . mk) <$> takeText

instance ToText OfferingDurationUnits where
    toText (OfferingDurationUnits' ci) = original ci

-- | Represents an enum of /known/ $OfferingDurationUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OfferingDurationUnits where
    toEnum i = case i of
        0 -> Months
        _ -> (error . showText) $ "Unknown index for OfferingDurationUnits: " <> toText i
    fromEnum x = case x of
        Months -> 0
        OfferingDurationUnits' name -> (error . showText) $ "Unknown OfferingDurationUnits: " <> original name

-- | Represents the bounds of /known/ $OfferingDurationUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OfferingDurationUnits where
    minBound = Months
    maxBound = Months

instance Hashable     OfferingDurationUnits
instance NFData       OfferingDurationUnits
instance ToByteString OfferingDurationUnits
instance ToQuery      OfferingDurationUnits
instance ToHeader     OfferingDurationUnits

instance FromJSON OfferingDurationUnits where
    parseJSON = parseJSONText "OfferingDurationUnits"
