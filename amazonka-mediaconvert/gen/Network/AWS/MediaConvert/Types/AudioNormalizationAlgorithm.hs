{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm (
  AudioNormalizationAlgorithm (
    ..
    , ItuBs17701
    , ItuBs17702
    , ItuBs17703
    , ItuBs17704
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
data AudioNormalizationAlgorithm = AudioNormalizationAlgorithm' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern ItuBs17701 :: AudioNormalizationAlgorithm
pattern ItuBs17701 = AudioNormalizationAlgorithm' "ITU_BS_1770_1"

pattern ItuBs17702 :: AudioNormalizationAlgorithm
pattern ItuBs17702 = AudioNormalizationAlgorithm' "ITU_BS_1770_2"

pattern ItuBs17703 :: AudioNormalizationAlgorithm
pattern ItuBs17703 = AudioNormalizationAlgorithm' "ITU_BS_1770_3"

pattern ItuBs17704 :: AudioNormalizationAlgorithm
pattern ItuBs17704 = AudioNormalizationAlgorithm' "ITU_BS_1770_4"

{-# COMPLETE
  ItuBs17701,
  ItuBs17702,
  ItuBs17703,
  ItuBs17704,
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
        0 -> ItuBs17701
        1 -> ItuBs17702
        2 -> ItuBs17703
        3 -> ItuBs17704
        _ -> (error . showText) $ "Unknown index for AudioNormalizationAlgorithm: " <> toText i
    fromEnum x = case x of
        ItuBs17701 -> 0
        ItuBs17702 -> 1
        ItuBs17703 -> 2
        ItuBs17704 -> 3
        AudioNormalizationAlgorithm' name -> (error . showText) $ "Unknown AudioNormalizationAlgorithm: " <> original name

-- | Represents the bounds of /known/ $AudioNormalizationAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioNormalizationAlgorithm where
    minBound = ItuBs17701
    maxBound = ItuBs17704

instance Hashable     AudioNormalizationAlgorithm
instance NFData       AudioNormalizationAlgorithm
instance ToByteString AudioNormalizationAlgorithm
instance ToQuery      AudioNormalizationAlgorithm
instance ToHeader     AudioNormalizationAlgorithm

instance ToJSON AudioNormalizationAlgorithm where
    toJSON = toJSONText

instance FromJSON AudioNormalizationAlgorithm where
    parseJSON = parseJSONText "AudioNormalizationAlgorithm"
