{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterStrength
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TemporalFilterStrength (
  TemporalFilterStrength (
    ..
    , Auto
    , Strength1
    , Strength10
    , Strength11
    , Strength12
    , Strength13
    , Strength14
    , Strength15
    , Strength16
    , Strength2
    , Strength3
    , Strength4
    , Strength5
    , Strength6
    , Strength7
    , Strength8
    , Strength9
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Temporal Filter Strength
data TemporalFilterStrength = TemporalFilterStrength' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Auto :: TemporalFilterStrength
pattern Auto = TemporalFilterStrength' "AUTO"

pattern Strength1 :: TemporalFilterStrength
pattern Strength1 = TemporalFilterStrength' "STRENGTH_1"

pattern Strength10 :: TemporalFilterStrength
pattern Strength10 = TemporalFilterStrength' "STRENGTH_10"

pattern Strength11 :: TemporalFilterStrength
pattern Strength11 = TemporalFilterStrength' "STRENGTH_11"

pattern Strength12 :: TemporalFilterStrength
pattern Strength12 = TemporalFilterStrength' "STRENGTH_12"

pattern Strength13 :: TemporalFilterStrength
pattern Strength13 = TemporalFilterStrength' "STRENGTH_13"

pattern Strength14 :: TemporalFilterStrength
pattern Strength14 = TemporalFilterStrength' "STRENGTH_14"

pattern Strength15 :: TemporalFilterStrength
pattern Strength15 = TemporalFilterStrength' "STRENGTH_15"

pattern Strength16 :: TemporalFilterStrength
pattern Strength16 = TemporalFilterStrength' "STRENGTH_16"

pattern Strength2 :: TemporalFilterStrength
pattern Strength2 = TemporalFilterStrength' "STRENGTH_2"

pattern Strength3 :: TemporalFilterStrength
pattern Strength3 = TemporalFilterStrength' "STRENGTH_3"

pattern Strength4 :: TemporalFilterStrength
pattern Strength4 = TemporalFilterStrength' "STRENGTH_4"

pattern Strength5 :: TemporalFilterStrength
pattern Strength5 = TemporalFilterStrength' "STRENGTH_5"

pattern Strength6 :: TemporalFilterStrength
pattern Strength6 = TemporalFilterStrength' "STRENGTH_6"

pattern Strength7 :: TemporalFilterStrength
pattern Strength7 = TemporalFilterStrength' "STRENGTH_7"

pattern Strength8 :: TemporalFilterStrength
pattern Strength8 = TemporalFilterStrength' "STRENGTH_8"

pattern Strength9 :: TemporalFilterStrength
pattern Strength9 = TemporalFilterStrength' "STRENGTH_9"

{-# COMPLETE
  Auto,
  Strength1,
  Strength10,
  Strength11,
  Strength12,
  Strength13,
  Strength14,
  Strength15,
  Strength16,
  Strength2,
  Strength3,
  Strength4,
  Strength5,
  Strength6,
  Strength7,
  Strength8,
  Strength9,
  TemporalFilterStrength' #-}

instance FromText TemporalFilterStrength where
    parser = (TemporalFilterStrength' . mk) <$> takeText

instance ToText TemporalFilterStrength where
    toText (TemporalFilterStrength' ci) = original ci

-- | Represents an enum of /known/ $TemporalFilterStrength.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TemporalFilterStrength where
    toEnum i = case i of
        0 -> Auto
        1 -> Strength1
        2 -> Strength10
        3 -> Strength11
        4 -> Strength12
        5 -> Strength13
        6 -> Strength14
        7 -> Strength15
        8 -> Strength16
        9 -> Strength2
        10 -> Strength3
        11 -> Strength4
        12 -> Strength5
        13 -> Strength6
        14 -> Strength7
        15 -> Strength8
        16 -> Strength9
        _ -> (error . showText) $ "Unknown index for TemporalFilterStrength: " <> toText i
    fromEnum x = case x of
        Auto -> 0
        Strength1 -> 1
        Strength10 -> 2
        Strength11 -> 3
        Strength12 -> 4
        Strength13 -> 5
        Strength14 -> 6
        Strength15 -> 7
        Strength16 -> 8
        Strength2 -> 9
        Strength3 -> 10
        Strength4 -> 11
        Strength5 -> 12
        Strength6 -> 13
        Strength7 -> 14
        Strength8 -> 15
        Strength9 -> 16
        TemporalFilterStrength' name -> (error . showText) $ "Unknown TemporalFilterStrength: " <> original name

-- | Represents the bounds of /known/ $TemporalFilterStrength.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TemporalFilterStrength where
    minBound = Auto
    maxBound = Strength9

instance Hashable     TemporalFilterStrength
instance NFData       TemporalFilterStrength
instance ToByteString TemporalFilterStrength
instance ToQuery      TemporalFilterStrength
instance ToHeader     TemporalFilterStrength

instance ToJSON TemporalFilterStrength where
    toJSON = toJSONText

instance FromJSON TemporalFilterStrength where
    parseJSON = parseJSONText "TemporalFilterStrength"
