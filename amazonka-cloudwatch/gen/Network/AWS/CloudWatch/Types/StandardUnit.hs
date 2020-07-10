{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StandardUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.StandardUnit (
  StandardUnit (
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

data StandardUnit = StandardUnit' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Bits :: StandardUnit
pattern Bits = StandardUnit' "Bits"

pattern BitsSecond :: StandardUnit
pattern BitsSecond = StandardUnit' "Bits/Second"

pattern Bytes :: StandardUnit
pattern Bytes = StandardUnit' "Bytes"

pattern BytesSecond :: StandardUnit
pattern BytesSecond = StandardUnit' "Bytes/Second"

pattern Count :: StandardUnit
pattern Count = StandardUnit' "Count"

pattern CountSecond :: StandardUnit
pattern CountSecond = StandardUnit' "Count/Second"

pattern Gigabits :: StandardUnit
pattern Gigabits = StandardUnit' "Gigabits"

pattern GigabitsSecond :: StandardUnit
pattern GigabitsSecond = StandardUnit' "Gigabits/Second"

pattern Gigabytes :: StandardUnit
pattern Gigabytes = StandardUnit' "Gigabytes"

pattern GigabytesSecond :: StandardUnit
pattern GigabytesSecond = StandardUnit' "Gigabytes/Second"

pattern Kilobits :: StandardUnit
pattern Kilobits = StandardUnit' "Kilobits"

pattern KilobitsSecond :: StandardUnit
pattern KilobitsSecond = StandardUnit' "Kilobits/Second"

pattern Kilobytes :: StandardUnit
pattern Kilobytes = StandardUnit' "Kilobytes"

pattern KilobytesSecond :: StandardUnit
pattern KilobytesSecond = StandardUnit' "Kilobytes/Second"

pattern Megabits :: StandardUnit
pattern Megabits = StandardUnit' "Megabits"

pattern MegabitsSecond :: StandardUnit
pattern MegabitsSecond = StandardUnit' "Megabits/Second"

pattern Megabytes :: StandardUnit
pattern Megabytes = StandardUnit' "Megabytes"

pattern MegabytesSecond :: StandardUnit
pattern MegabytesSecond = StandardUnit' "Megabytes/Second"

pattern Microseconds :: StandardUnit
pattern Microseconds = StandardUnit' "Microseconds"

pattern Milliseconds :: StandardUnit
pattern Milliseconds = StandardUnit' "Milliseconds"

pattern None :: StandardUnit
pattern None = StandardUnit' "None"

pattern Percent :: StandardUnit
pattern Percent = StandardUnit' "Percent"

pattern Seconds :: StandardUnit
pattern Seconds = StandardUnit' "Seconds"

pattern Terabits :: StandardUnit
pattern Terabits = StandardUnit' "Terabits"

pattern TerabitsSecond :: StandardUnit
pattern TerabitsSecond = StandardUnit' "Terabits/Second"

pattern Terabytes :: StandardUnit
pattern Terabytes = StandardUnit' "Terabytes"

pattern TerabytesSecond :: StandardUnit
pattern TerabytesSecond = StandardUnit' "Terabytes/Second"

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
  StandardUnit' #-}

instance FromText StandardUnit where
    parser = (StandardUnit' . mk) <$> takeText

instance ToText StandardUnit where
    toText (StandardUnit' ci) = original ci

-- | Represents an enum of /known/ $StandardUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StandardUnit where
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
        _ -> (error . showText) $ "Unknown index for StandardUnit: " <> toText i
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
        StandardUnit' name -> (error . showText) $ "Unknown StandardUnit: " <> original name

-- | Represents the bounds of /known/ $StandardUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StandardUnit where
    minBound = Bits
    maxBound = TerabytesSecond

instance Hashable     StandardUnit
instance NFData       StandardUnit
instance ToByteString StandardUnit
instance ToQuery      StandardUnit
instance ToHeader     StandardUnit

instance FromXML StandardUnit where
    parseXML = parseXMLText "StandardUnit"
