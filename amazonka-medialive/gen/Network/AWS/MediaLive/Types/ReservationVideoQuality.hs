{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationVideoQuality
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationVideoQuality (
  ReservationVideoQuality (
    ..
    , Enhanced
    , Premium
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Video quality, e.g. 'STANDARD' (Outputs only)
data ReservationVideoQuality = ReservationVideoQuality' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Enhanced :: ReservationVideoQuality
pattern Enhanced = ReservationVideoQuality' "ENHANCED"

pattern Premium :: ReservationVideoQuality
pattern Premium = ReservationVideoQuality' "PREMIUM"

pattern Standard :: ReservationVideoQuality
pattern Standard = ReservationVideoQuality' "STANDARD"

{-# COMPLETE
  Enhanced,
  Premium,
  Standard,
  ReservationVideoQuality' #-}

instance FromText ReservationVideoQuality where
    parser = (ReservationVideoQuality' . mk) <$> takeText

instance ToText ReservationVideoQuality where
    toText (ReservationVideoQuality' ci) = original ci

-- | Represents an enum of /known/ $ReservationVideoQuality.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationVideoQuality where
    toEnum i = case i of
        0 -> Enhanced
        1 -> Premium
        2 -> Standard
        _ -> (error . showText) $ "Unknown index for ReservationVideoQuality: " <> toText i
    fromEnum x = case x of
        Enhanced -> 0
        Premium -> 1
        Standard -> 2
        ReservationVideoQuality' name -> (error . showText) $ "Unknown ReservationVideoQuality: " <> original name

-- | Represents the bounds of /known/ $ReservationVideoQuality.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationVideoQuality where
    minBound = Enhanced
    maxBound = Standard

instance Hashable     ReservationVideoQuality
instance NFData       ReservationVideoQuality
instance ToByteString ReservationVideoQuality
instance ToQuery      ReservationVideoQuality
instance ToHeader     ReservationVideoQuality

instance FromJSON ReservationVideoQuality where
    parseJSON = parseJSONText "ReservationVideoQuality"
