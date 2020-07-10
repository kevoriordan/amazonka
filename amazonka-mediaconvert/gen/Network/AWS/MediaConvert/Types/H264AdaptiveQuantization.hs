{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264AdaptiveQuantization (
  H264AdaptiveQuantization (
    ..
    , HHigh
    , HHigher
    , HLow
    , HMax
    , HMedium
    , HOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data H264AdaptiveQuantization = H264AdaptiveQuantization' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HHigh :: H264AdaptiveQuantization
pattern HHigh = H264AdaptiveQuantization' "HIGH"

pattern HHigher :: H264AdaptiveQuantization
pattern HHigher = H264AdaptiveQuantization' "HIGHER"

pattern HLow :: H264AdaptiveQuantization
pattern HLow = H264AdaptiveQuantization' "LOW"

pattern HMax :: H264AdaptiveQuantization
pattern HMax = H264AdaptiveQuantization' "MAX"

pattern HMedium :: H264AdaptiveQuantization
pattern HMedium = H264AdaptiveQuantization' "MEDIUM"

pattern HOff :: H264AdaptiveQuantization
pattern HOff = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  HHigh,
  HHigher,
  HLow,
  HMax,
  HMedium,
  HOff,
  H264AdaptiveQuantization' #-}

instance FromText H264AdaptiveQuantization where
    parser = (H264AdaptiveQuantization' . mk) <$> takeText

instance ToText H264AdaptiveQuantization where
    toText (H264AdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H264AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264AdaptiveQuantization where
    toEnum i = case i of
        0 -> HHigh
        1 -> HHigher
        2 -> HLow
        3 -> HMax
        4 -> HMedium
        5 -> HOff
        _ -> (error . showText) $ "Unknown index for H264AdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HHigh -> 0
        HHigher -> 1
        HLow -> 2
        HMax -> 3
        HMedium -> 4
        HOff -> 5
        H264AdaptiveQuantization' name -> (error . showText) $ "Unknown H264AdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H264AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264AdaptiveQuantization where
    minBound = HHigh
    maxBound = HOff

instance Hashable     H264AdaptiveQuantization
instance NFData       H264AdaptiveQuantization
instance ToByteString H264AdaptiveQuantization
instance ToQuery      H264AdaptiveQuantization
instance ToHeader     H264AdaptiveQuantization

instance ToJSON H264AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264AdaptiveQuantization where
    parseJSON = parseJSONText "H264AdaptiveQuantization"
