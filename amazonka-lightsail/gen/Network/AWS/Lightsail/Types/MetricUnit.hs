{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.MetricUnit (
  MetricUnit (
    ..
    , Bits
    , BitsSecond
    , Bytes
    , BytesSecond
    , Count
    , CountSecond
    , Gigabits
    , GigabitsSecond
    , Gigabytes
    , GigabytesSecond
    , Kilobits
    , KilobitsSecond
    , Kilobytes
    , KilobytesSecond
    , Megabits
    , MegabitsSecond
    , Megabytes
    , MegabytesSecond
    , Microseconds
    , Milliseconds
    , None
    , Percent
    , Seconds
    , Terabits
    , TerabitsSecond
    , Terabytes
    , TerabytesSecond
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricUnit = MetricUnit' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Bits :: MetricUnit
pattern Bits = MetricUnit' "Bits"

pattern BitsSecond :: MetricUnit
pattern BitsSecond = MetricUnit' "Bits/Second"

pattern Bytes :: MetricUnit
pattern Bytes = MetricUnit' "Bytes"

pattern BytesSecond :: MetricUnit
pattern BytesSecond = MetricUnit' "Bytes/Second"

pattern Count :: MetricUnit
pattern Count = MetricUnit' "Count"

pattern CountSecond :: MetricUnit
pattern CountSecond = MetricUnit' "Count/Second"

pattern Gigabits :: MetricUnit
pattern Gigabits = MetricUnit' "Gigabits"

pattern GigabitsSecond :: MetricUnit
pattern GigabitsSecond = MetricUnit' "Gigabits/Second"

pattern Gigabytes :: MetricUnit
pattern Gigabytes = MetricUnit' "Gigabytes"

pattern GigabytesSecond :: MetricUnit
pattern GigabytesSecond = MetricUnit' "Gigabytes/Second"

pattern Kilobits :: MetricUnit
pattern Kilobits = MetricUnit' "Kilobits"

pattern KilobitsSecond :: MetricUnit
pattern KilobitsSecond = MetricUnit' "Kilobits/Second"

pattern Kilobytes :: MetricUnit
pattern Kilobytes = MetricUnit' "Kilobytes"

pattern KilobytesSecond :: MetricUnit
pattern KilobytesSecond = MetricUnit' "Kilobytes/Second"

pattern Megabits :: MetricUnit
pattern Megabits = MetricUnit' "Megabits"

pattern MegabitsSecond :: MetricUnit
pattern MegabitsSecond = MetricUnit' "Megabits/Second"

pattern Megabytes :: MetricUnit
pattern Megabytes = MetricUnit' "Megabytes"

pattern MegabytesSecond :: MetricUnit
pattern MegabytesSecond = MetricUnit' "Megabytes/Second"

pattern Microseconds :: MetricUnit
pattern Microseconds = MetricUnit' "Microseconds"

pattern Milliseconds :: MetricUnit
pattern Milliseconds = MetricUnit' "Milliseconds"

pattern None :: MetricUnit
pattern None = MetricUnit' "None"

pattern Percent :: MetricUnit
pattern Percent = MetricUnit' "Percent"

pattern Seconds :: MetricUnit
pattern Seconds = MetricUnit' "Seconds"

pattern Terabits :: MetricUnit
pattern Terabits = MetricUnit' "Terabits"

pattern TerabitsSecond :: MetricUnit
pattern TerabitsSecond = MetricUnit' "Terabits/Second"

pattern Terabytes :: MetricUnit
pattern Terabytes = MetricUnit' "Terabytes"

pattern TerabytesSecond :: MetricUnit
pattern TerabytesSecond = MetricUnit' "Terabytes/Second"

{-# COMPLETE
  Bits,
  BitsSecond,
  Bytes,
  BytesSecond,
  Count,
  CountSecond,
  Gigabits,
  GigabitsSecond,
  Gigabytes,
  GigabytesSecond,
  Kilobits,
  KilobitsSecond,
  Kilobytes,
  KilobytesSecond,
  Megabits,
  MegabitsSecond,
  Megabytes,
  MegabytesSecond,
  Microseconds,
  Milliseconds,
  None,
  Percent,
  Seconds,
  Terabits,
  TerabitsSecond,
  Terabytes,
  TerabytesSecond,
  MetricUnit' #-}

instance FromText MetricUnit where
    parser = (MetricUnit' . mk) <$> takeText

instance ToText MetricUnit where
    toText (MetricUnit' ci) = original ci

-- | Represents an enum of /known/ $MetricUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricUnit where
    toEnum i = case i of
        0 -> Bits
        1 -> BitsSecond
        2 -> Bytes
        3 -> BytesSecond
        4 -> Count
        5 -> CountSecond
        6 -> Gigabits
        7 -> GigabitsSecond
        8 -> Gigabytes
        9 -> GigabytesSecond
        10 -> Kilobits
        11 -> KilobitsSecond
        12 -> Kilobytes
        13 -> KilobytesSecond
        14 -> Megabits
        15 -> MegabitsSecond
        16 -> Megabytes
        17 -> MegabytesSecond
        18 -> Microseconds
        19 -> Milliseconds
        20 -> None
        21 -> Percent
        22 -> Seconds
        23 -> Terabits
        24 -> TerabitsSecond
        25 -> Terabytes
        26 -> TerabytesSecond
        _ -> (error . showText) $ "Unknown index for MetricUnit: " <> toText i
    fromEnum x = case x of
        Bits -> 0
        BitsSecond -> 1
        Bytes -> 2
        BytesSecond -> 3
        Count -> 4
        CountSecond -> 5
        Gigabits -> 6
        GigabitsSecond -> 7
        Gigabytes -> 8
        GigabytesSecond -> 9
        Kilobits -> 10
        KilobitsSecond -> 11
        Kilobytes -> 12
        KilobytesSecond -> 13
        Megabits -> 14
        MegabitsSecond -> 15
        Megabytes -> 16
        MegabytesSecond -> 17
        Microseconds -> 18
        Milliseconds -> 19
        None -> 20
        Percent -> 21
        Seconds -> 22
        Terabits -> 23
        TerabitsSecond -> 24
        Terabytes -> 25
        TerabytesSecond -> 26
        MetricUnit' name -> (error . showText) $ "Unknown MetricUnit: " <> original name

-- | Represents the bounds of /known/ $MetricUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricUnit where
    minBound = Bits
    maxBound = TerabytesSecond

instance Hashable     MetricUnit
instance NFData       MetricUnit
instance ToByteString MetricUnit
instance ToQuery      MetricUnit
instance ToHeader     MetricUnit

instance ToJSON MetricUnit where
    toJSON = toJSONText

instance FromJSON MetricUnit where
    parseJSON = parseJSONText "MetricUnit"
