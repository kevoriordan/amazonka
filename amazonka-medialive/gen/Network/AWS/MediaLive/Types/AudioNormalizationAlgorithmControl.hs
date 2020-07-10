{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl (
  AudioNormalizationAlgorithmControl (
    ..
    , CorrectAudio
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Normalization Algorithm Control
data AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern CorrectAudio :: AudioNormalizationAlgorithmControl
pattern CorrectAudio = AudioNormalizationAlgorithmControl' "CORRECT_AUDIO"

{-# COMPLETE
  CorrectAudio,
  AudioNormalizationAlgorithmControl' #-}

instance FromText AudioNormalizationAlgorithmControl where
    parser = (AudioNormalizationAlgorithmControl' . mk) <$> takeText

instance ToText AudioNormalizationAlgorithmControl where
    toText (AudioNormalizationAlgorithmControl' ci) = original ci

-- | Represents an enum of /known/ $AudioNormalizationAlgorithmControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioNormalizationAlgorithmControl where
    toEnum i = case i of
        0 -> CorrectAudio
        _ -> (error . showText) $ "Unknown index for AudioNormalizationAlgorithmControl: " <> toText i
    fromEnum x = case x of
        CorrectAudio -> 0
        AudioNormalizationAlgorithmControl' name -> (error . showText) $ "Unknown AudioNormalizationAlgorithmControl: " <> original name

-- | Represents the bounds of /known/ $AudioNormalizationAlgorithmControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioNormalizationAlgorithmControl where
    minBound = CorrectAudio
    maxBound = CorrectAudio

instance Hashable     AudioNormalizationAlgorithmControl
instance NFData       AudioNormalizationAlgorithmControl
instance ToByteString AudioNormalizationAlgorithmControl
instance ToQuery      AudioNormalizationAlgorithmControl
instance ToHeader     AudioNormalizationAlgorithmControl

instance ToJSON AudioNormalizationAlgorithmControl where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithmControl where
    parseJSON = parseJSONText "AudioNormalizationAlgorithmControl"
