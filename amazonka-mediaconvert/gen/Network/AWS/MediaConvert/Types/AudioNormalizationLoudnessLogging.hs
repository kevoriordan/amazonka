{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging (
  AudioNormalizationLoudnessLogging (
    ..
    , DontLog
    , Log
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If set to LOG, log each output's audio track loudness to a CSV file.
data AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern DontLog :: AudioNormalizationLoudnessLogging
pattern DontLog = AudioNormalizationLoudnessLogging' "DONT_LOG"

pattern Log :: AudioNormalizationLoudnessLogging
pattern Log = AudioNormalizationLoudnessLogging' "LOG"

{-# COMPLETE
  DontLog,
  Log,
  AudioNormalizationLoudnessLogging' #-}

instance FromText AudioNormalizationLoudnessLogging where
    parser = (AudioNormalizationLoudnessLogging' . mk) <$> takeText

instance ToText AudioNormalizationLoudnessLogging where
    toText (AudioNormalizationLoudnessLogging' ci) = original ci

-- | Represents an enum of /known/ $AudioNormalizationLoudnessLogging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioNormalizationLoudnessLogging where
    toEnum i = case i of
        0 -> DontLog
        1 -> Log
        _ -> (error . showText) $ "Unknown index for AudioNormalizationLoudnessLogging: " <> toText i
    fromEnum x = case x of
        DontLog -> 0
        Log -> 1
        AudioNormalizationLoudnessLogging' name -> (error . showText) $ "Unknown AudioNormalizationLoudnessLogging: " <> original name

-- | Represents the bounds of /known/ $AudioNormalizationLoudnessLogging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioNormalizationLoudnessLogging where
    minBound = DontLog
    maxBound = Log

instance Hashable     AudioNormalizationLoudnessLogging
instance NFData       AudioNormalizationLoudnessLogging
instance ToByteString AudioNormalizationLoudnessLogging
instance ToQuery      AudioNormalizationLoudnessLogging
instance ToHeader     AudioNormalizationLoudnessLogging

instance ToJSON AudioNormalizationLoudnessLogging where
    toJSON = toJSONText

instance FromJSON AudioNormalizationLoudnessLogging where
    parseJSON = parseJSONText "AudioNormalizationLoudnessLogging"
