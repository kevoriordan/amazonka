{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation (
  AudioNormalizationPeakCalculation (
    ..
    , ANPCNone
    , ANPCTruePeak
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
data AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern ANPCNone :: AudioNormalizationPeakCalculation
pattern ANPCNone = AudioNormalizationPeakCalculation' "NONE"

pattern ANPCTruePeak :: AudioNormalizationPeakCalculation
pattern ANPCTruePeak = AudioNormalizationPeakCalculation' "TRUE_PEAK"

{-# COMPLETE
  ANPCNone,
  ANPCTruePeak,
  AudioNormalizationPeakCalculation' #-}

instance FromText AudioNormalizationPeakCalculation where
    parser = (AudioNormalizationPeakCalculation' . mk) <$> takeText

instance ToText AudioNormalizationPeakCalculation where
    toText (AudioNormalizationPeakCalculation' ci) = original ci

-- | Represents an enum of /known/ $AudioNormalizationPeakCalculation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioNormalizationPeakCalculation where
    toEnum i = case i of
        0 -> ANPCNone
        1 -> ANPCTruePeak
        _ -> (error . showText) $ "Unknown index for AudioNormalizationPeakCalculation: " <> toText i
    fromEnum x = case x of
        ANPCNone -> 0
        ANPCTruePeak -> 1
        AudioNormalizationPeakCalculation' name -> (error . showText) $ "Unknown AudioNormalizationPeakCalculation: " <> original name

-- | Represents the bounds of /known/ $AudioNormalizationPeakCalculation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioNormalizationPeakCalculation where
    minBound = ANPCNone
    maxBound = ANPCTruePeak

instance Hashable     AudioNormalizationPeakCalculation
instance NFData       AudioNormalizationPeakCalculation
instance ToByteString AudioNormalizationPeakCalculation
instance ToQuery      AudioNormalizationPeakCalculation
instance ToHeader     AudioNormalizationPeakCalculation

instance ToJSON AudioNormalizationPeakCalculation where
    toJSON = toJSONText

instance FromJSON AudioNormalizationPeakCalculation where
    parseJSON = parseJSONText "AudioNormalizationPeakCalculation"
