{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm (
  AudioNormalizationAlgorithm (
    ..
    , Itu17701
    , Itu17702
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Normalization Algorithm
data AudioNormalizationAlgorithm = AudioNormalizationAlgorithm' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern Itu17701 :: AudioNormalizationAlgorithm
pattern Itu17701 = AudioNormalizationAlgorithm' "ITU_1770_1"

pattern Itu17702 :: AudioNormalizationAlgorithm
pattern Itu17702 = AudioNormalizationAlgorithm' "ITU_1770_2"

{-# COMPLETE
  Itu17701,
  Itu17702,
  AudioNormalizationAlgorithm' #-}

instance FromText AudioNormalizationAlgorithm where
    parser = (AudioNormalizationAlgorithm' . mk) <$> takeText

instance ToText AudioNormalizationAlgorithm where
    toText (AudioNormalizationAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $AudioNormalizationAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioNormalizationAlgorithm where
    toEnum i = case i of
        0 -> Itu17701
        1 -> Itu17702
        _ -> (error . showText) $ "Unknown index for AudioNormalizationAlgorithm: " <> toText i
    fromEnum x = case x of
        Itu17701 -> 0
        Itu17702 -> 1
        AudioNormalizationAlgorithm' name -> (error . showText) $ "Unknown AudioNormalizationAlgorithm: " <> original name

-- | Represents the bounds of /known/ $AudioNormalizationAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioNormalizationAlgorithm where
    minBound = Itu17701
    maxBound = Itu17702

instance Hashable     AudioNormalizationAlgorithm
instance NFData       AudioNormalizationAlgorithm
instance ToByteString AudioNormalizationAlgorithm
instance ToQuery      AudioNormalizationAlgorithm
instance ToHeader     AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
    parseJSON = parseJSONText "AudioNormalizationAlgorithm"
