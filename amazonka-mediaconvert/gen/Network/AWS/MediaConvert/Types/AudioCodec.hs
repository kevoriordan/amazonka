{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioCodec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioCodec (
  AudioCodec (
    ..
    , ACAC3
    , ACAac
    , ACAiff
    , ACEAC3
    , ACEAC3Atmos
    , ACMP2
    , ACMP3
    , ACPassthrough
    , ACWav
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Type of Audio codec.
data AudioCodec = AudioCodec' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ACAC3 :: AudioCodec
pattern ACAC3 = AudioCodec' "AC3"

pattern ACAac :: AudioCodec
pattern ACAac = AudioCodec' "AAC"

pattern ACAiff :: AudioCodec
pattern ACAiff = AudioCodec' "AIFF"

pattern ACEAC3 :: AudioCodec
pattern ACEAC3 = AudioCodec' "EAC3"

pattern ACEAC3Atmos :: AudioCodec
pattern ACEAC3Atmos = AudioCodec' "EAC3_ATMOS"

pattern ACMP2 :: AudioCodec
pattern ACMP2 = AudioCodec' "MP2"

pattern ACMP3 :: AudioCodec
pattern ACMP3 = AudioCodec' "MP3"

pattern ACPassthrough :: AudioCodec
pattern ACPassthrough = AudioCodec' "PASSTHROUGH"

pattern ACWav :: AudioCodec
pattern ACWav = AudioCodec' "WAV"

{-# COMPLETE
  ACAC3,
  ACAac,
  ACAiff,
  ACEAC3,
  ACEAC3Atmos,
  ACMP2,
  ACMP3,
  ACPassthrough,
  ACWav,
  AudioCodec' #-}

instance FromText AudioCodec where
    parser = (AudioCodec' . mk) <$> takeText

instance ToText AudioCodec where
    toText (AudioCodec' ci) = original ci

-- | Represents an enum of /known/ $AudioCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioCodec where
    toEnum i = case i of
        0 -> ACAC3
        1 -> ACAac
        2 -> ACAiff
        3 -> ACEAC3
        4 -> ACEAC3Atmos
        5 -> ACMP2
        6 -> ACMP3
        7 -> ACPassthrough
        8 -> ACWav
        _ -> (error . showText) $ "Unknown index for AudioCodec: " <> toText i
    fromEnum x = case x of
        ACAC3 -> 0
        ACAac -> 1
        ACAiff -> 2
        ACEAC3 -> 3
        ACEAC3Atmos -> 4
        ACMP2 -> 5
        ACMP3 -> 6
        ACPassthrough -> 7
        ACWav -> 8
        AudioCodec' name -> (error . showText) $ "Unknown AudioCodec: " <> original name

-- | Represents the bounds of /known/ $AudioCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioCodec where
    minBound = ACAC3
    maxBound = ACWav

instance Hashable     AudioCodec
instance NFData       AudioCodec
instance ToByteString AudioCodec
instance ToQuery      AudioCodec
instance ToHeader     AudioCodec

instance ToJSON AudioCodec where
    toJSON = toJSONText

instance FromJSON AudioCodec where
    parseJSON = parseJSONText "AudioCodec"
