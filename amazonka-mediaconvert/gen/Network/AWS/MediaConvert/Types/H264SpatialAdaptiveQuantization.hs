{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264SpatialAdaptiveQuantization (
  H264SpatialAdaptiveQuantization (
    ..
    , H264SptlAdptvQntztnDisabled
    , H264SptlAdptvQntztnEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data H264SpatialAdaptiveQuantization = H264SpatialAdaptiveQuantization' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern H264SptlAdptvQntztnDisabled :: H264SpatialAdaptiveQuantization
pattern H264SptlAdptvQntztnDisabled = H264SpatialAdaptiveQuantization' "DISABLED"

pattern H264SptlAdptvQntztnEnabled :: H264SpatialAdaptiveQuantization
pattern H264SptlAdptvQntztnEnabled = H264SpatialAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H264SptlAdptvQntztnDisabled,
  H264SptlAdptvQntztnEnabled,
  H264SpatialAdaptiveQuantization' #-}

instance FromText H264SpatialAdaptiveQuantization where
    parser = (H264SpatialAdaptiveQuantization' . mk) <$> takeText

instance ToText H264SpatialAdaptiveQuantization where
    toText (H264SpatialAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H264SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264SpatialAdaptiveQuantization where
    toEnum i = case i of
        0 -> H264SptlAdptvQntztnDisabled
        1 -> H264SptlAdptvQntztnEnabled
        _ -> (error . showText) $ "Unknown index for H264SpatialAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        H264SptlAdptvQntztnDisabled -> 0
        H264SptlAdptvQntztnEnabled -> 1
        H264SpatialAdaptiveQuantization' name -> (error . showText) $ "Unknown H264SpatialAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H264SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264SpatialAdaptiveQuantization where
    minBound = H264SptlAdptvQntztnDisabled
    maxBound = H264SptlAdptvQntztnEnabled

instance Hashable     H264SpatialAdaptiveQuantization
instance NFData       H264SpatialAdaptiveQuantization
instance ToByteString H264SpatialAdaptiveQuantization
instance ToQuery      H264SpatialAdaptiveQuantization
instance ToHeader     H264SpatialAdaptiveQuantization

instance ToJSON H264SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "H264SpatialAdaptiveQuantization"
