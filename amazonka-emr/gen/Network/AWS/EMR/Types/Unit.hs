{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Unit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Unit (
  Unit (
    ..
    , Bits
    , BitsPerSecond
    , Bytes
    , BytesPerSecond
    , Count
    , CountPerSecond
    , GigaBits
    , GigaBitsPerSecond
    , GigaBytes
    , GigaBytesPerSecond
    , KiloBits
    , KiloBitsPerSecond
    , KiloBytes
    , KiloBytesPerSecond
    , MegaBits
    , MegaBitsPerSecond
    , MegaBytes
    , MegaBytesPerSecond
    , MicroSeconds
    , MilliSeconds
    , None
    , Percent
    , Seconds
    , TeraBits
    , TeraBitsPerSecond
    , TeraBytes
    , TeraBytesPerSecond
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Unit = Unit' (CI Text)
              deriving (Eq, Ord, Read, Show, Data, Typeable,
                        Generic)

pattern Bits :: Unit
pattern Bits = Unit' "BITS"

pattern BitsPerSecond :: Unit
pattern BitsPerSecond = Unit' "BITS_PER_SECOND"

pattern Bytes :: Unit
pattern Bytes = Unit' "BYTES"

pattern BytesPerSecond :: Unit
pattern BytesPerSecond = Unit' "BYTES_PER_SECOND"

pattern Count :: Unit
pattern Count = Unit' "COUNT"

pattern CountPerSecond :: Unit
pattern CountPerSecond = Unit' "COUNT_PER_SECOND"

pattern GigaBits :: Unit
pattern GigaBits = Unit' "GIGA_BITS"

pattern GigaBitsPerSecond :: Unit
pattern GigaBitsPerSecond = Unit' "GIGA_BITS_PER_SECOND"

pattern GigaBytes :: Unit
pattern GigaBytes = Unit' "GIGA_BYTES"

pattern GigaBytesPerSecond :: Unit
pattern GigaBytesPerSecond = Unit' "GIGA_BYTES_PER_SECOND"

pattern KiloBits :: Unit
pattern KiloBits = Unit' "KILO_BITS"

pattern KiloBitsPerSecond :: Unit
pattern KiloBitsPerSecond = Unit' "KILO_BITS_PER_SECOND"

pattern KiloBytes :: Unit
pattern KiloBytes = Unit' "KILO_BYTES"

pattern KiloBytesPerSecond :: Unit
pattern KiloBytesPerSecond = Unit' "KILO_BYTES_PER_SECOND"

pattern MegaBits :: Unit
pattern MegaBits = Unit' "MEGA_BITS"

pattern MegaBitsPerSecond :: Unit
pattern MegaBitsPerSecond = Unit' "MEGA_BITS_PER_SECOND"

pattern MegaBytes :: Unit
pattern MegaBytes = Unit' "MEGA_BYTES"

pattern MegaBytesPerSecond :: Unit
pattern MegaBytesPerSecond = Unit' "MEGA_BYTES_PER_SECOND"

pattern MicroSeconds :: Unit
pattern MicroSeconds = Unit' "MICRO_SECONDS"

pattern MilliSeconds :: Unit
pattern MilliSeconds = Unit' "MILLI_SECONDS"

pattern None :: Unit
pattern None = Unit' "NONE"

pattern Percent :: Unit
pattern Percent = Unit' "PERCENT"

pattern Seconds :: Unit
pattern Seconds = Unit' "SECONDS"

pattern TeraBits :: Unit
pattern TeraBits = Unit' "TERA_BITS"

pattern TeraBitsPerSecond :: Unit
pattern TeraBitsPerSecond = Unit' "TERA_BITS_PER_SECOND"

pattern TeraBytes :: Unit
pattern TeraBytes = Unit' "TERA_BYTES"

pattern TeraBytesPerSecond :: Unit
pattern TeraBytesPerSecond = Unit' "TERA_BYTES_PER_SECOND"

{-# COMPLETE
  Bits,
  BitsPerSecond,
  Bytes,
  BytesPerSecond,
  Count,
  CountPerSecond,
  GigaBits,
  GigaBitsPerSecond,
  GigaBytes,
  GigaBytesPerSecond,
  KiloBits,
  KiloBitsPerSecond,
  KiloBytes,
  KiloBytesPerSecond,
  MegaBits,
  MegaBitsPerSecond,
  MegaBytes,
  MegaBytesPerSecond,
  MicroSeconds,
  MilliSeconds,
  None,
  Percent,
  Seconds,
  TeraBits,
  TeraBitsPerSecond,
  TeraBytes,
  TeraBytesPerSecond,
  Unit' #-}

instance FromText Unit where
    parser = (Unit' . mk) <$> takeText

instance ToText Unit where
    toText (Unit' ci) = original ci

-- | Represents an enum of /known/ $Unit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Unit where
    toEnum i = case i of
        0 -> Bits
        1 -> BitsPerSecond
        2 -> Bytes
        3 -> BytesPerSecond
        4 -> Count
        5 -> CountPerSecond
        6 -> GigaBits
        7 -> GigaBitsPerSecond
        8 -> GigaBytes
        9 -> GigaBytesPerSecond
        10 -> KiloBits
        11 -> KiloBitsPerSecond
        12 -> KiloBytes
        13 -> KiloBytesPerSecond
        14 -> MegaBits
        15 -> MegaBitsPerSecond
        16 -> MegaBytes
        17 -> MegaBytesPerSecond
        18 -> MicroSeconds
        19 -> MilliSeconds
        20 -> None
        21 -> Percent
        22 -> Seconds
        23 -> TeraBits
        24 -> TeraBitsPerSecond
        25 -> TeraBytes
        26 -> TeraBytesPerSecond
        _ -> (error . showText) $ "Unknown index for Unit: " <> toText i
    fromEnum x = case x of
        Bits -> 0
        BitsPerSecond -> 1
        Bytes -> 2
        BytesPerSecond -> 3
        Count -> 4
        CountPerSecond -> 5
        GigaBits -> 6
        GigaBitsPerSecond -> 7
        GigaBytes -> 8
        GigaBytesPerSecond -> 9
        KiloBits -> 10
        KiloBitsPerSecond -> 11
        KiloBytes -> 12
        KiloBytesPerSecond -> 13
        MegaBits -> 14
        MegaBitsPerSecond -> 15
        MegaBytes -> 16
        MegaBytesPerSecond -> 17
        MicroSeconds -> 18
        MilliSeconds -> 19
        None -> 20
        Percent -> 21
        Seconds -> 22
        TeraBits -> 23
        TeraBitsPerSecond -> 24
        TeraBytes -> 25
        TeraBytesPerSecond -> 26
        Unit' name -> (error . showText) $ "Unknown Unit: " <> original name

-- | Represents the bounds of /known/ $Unit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Unit where
    minBound = Bits
    maxBound = TeraBytesPerSecond

instance Hashable     Unit
instance NFData       Unit
instance ToByteString Unit
instance ToQuery      Unit
instance ToHeader     Unit

instance ToJSON Unit where
    toJSON = toJSONText

instance FromJSON Unit where
    parseJSON = parseJSONText "Unit"
