{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization (
  Mpeg2SpatialAdaptiveQuantization (
    ..
    , MSAQDisabled
    , MSAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data Mpeg2SpatialAdaptiveQuantization = Mpeg2SpatialAdaptiveQuantization' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern MSAQDisabled :: Mpeg2SpatialAdaptiveQuantization
pattern MSAQDisabled = Mpeg2SpatialAdaptiveQuantization' "DISABLED"

pattern MSAQEnabled :: Mpeg2SpatialAdaptiveQuantization
pattern MSAQEnabled = Mpeg2SpatialAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  MSAQDisabled,
  MSAQEnabled,
  Mpeg2SpatialAdaptiveQuantization' #-}

instance FromText Mpeg2SpatialAdaptiveQuantization where
    parser = (Mpeg2SpatialAdaptiveQuantization' . mk) <$> takeText

instance ToText Mpeg2SpatialAdaptiveQuantization where
    toText (Mpeg2SpatialAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2SpatialAdaptiveQuantization where
    toEnum i = case i of
        0 -> MSAQDisabled
        1 -> MSAQEnabled
        _ -> (error . showText) $ "Unknown index for Mpeg2SpatialAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        MSAQDisabled -> 0
        MSAQEnabled -> 1
        Mpeg2SpatialAdaptiveQuantization' name -> (error . showText) $ "Unknown Mpeg2SpatialAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $Mpeg2SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2SpatialAdaptiveQuantization where
    minBound = MSAQDisabled
    maxBound = MSAQEnabled

instance Hashable     Mpeg2SpatialAdaptiveQuantization
instance NFData       Mpeg2SpatialAdaptiveQuantization
instance ToByteString Mpeg2SpatialAdaptiveQuantization
instance ToQuery      Mpeg2SpatialAdaptiveQuantization
instance ToHeader     Mpeg2SpatialAdaptiveQuantization

instance ToJSON Mpeg2SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2SpatialAdaptiveQuantization"
