{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265SpatialAdaptiveQuantization (
  H265SpatialAdaptiveQuantization (
    ..
    , HSAQDisabled
    , HSAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data H265SpatialAdaptiveQuantization = H265SpatialAdaptiveQuantization' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern HSAQDisabled :: H265SpatialAdaptiveQuantization
pattern HSAQDisabled = H265SpatialAdaptiveQuantization' "DISABLED"

pattern HSAQEnabled :: H265SpatialAdaptiveQuantization
pattern HSAQEnabled = H265SpatialAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  HSAQDisabled,
  HSAQEnabled,
  H265SpatialAdaptiveQuantization' #-}

instance FromText H265SpatialAdaptiveQuantization where
    parser = (H265SpatialAdaptiveQuantization' . mk) <$> takeText

instance ToText H265SpatialAdaptiveQuantization where
    toText (H265SpatialAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H265SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265SpatialAdaptiveQuantization where
    toEnum i = case i of
        0 -> HSAQDisabled
        1 -> HSAQEnabled
        _ -> (error . showText) $ "Unknown index for H265SpatialAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HSAQDisabled -> 0
        HSAQEnabled -> 1
        H265SpatialAdaptiveQuantization' name -> (error . showText) $ "Unknown H265SpatialAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H265SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265SpatialAdaptiveQuantization where
    minBound = HSAQDisabled
    maxBound = HSAQEnabled

instance Hashable     H265SpatialAdaptiveQuantization
instance NFData       H265SpatialAdaptiveQuantization
instance ToByteString H265SpatialAdaptiveQuantization
instance ToQuery      H265SpatialAdaptiveQuantization
instance ToHeader     H265SpatialAdaptiveQuantization

instance ToJSON H265SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "H265SpatialAdaptiveQuantization"
