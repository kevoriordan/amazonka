{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1SpatialAdaptiveQuantization (
  Av1SpatialAdaptiveQuantization (
    ..
    , ASAQDisabled
    , ASAQEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame based on spatial variation of content complexity.
data Av1SpatialAdaptiveQuantization = Av1SpatialAdaptiveQuantization' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern ASAQDisabled :: Av1SpatialAdaptiveQuantization
pattern ASAQDisabled = Av1SpatialAdaptiveQuantization' "DISABLED"

pattern ASAQEnabled :: Av1SpatialAdaptiveQuantization
pattern ASAQEnabled = Av1SpatialAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  ASAQDisabled,
  ASAQEnabled,
  Av1SpatialAdaptiveQuantization' #-}

instance FromText Av1SpatialAdaptiveQuantization where
    parser = (Av1SpatialAdaptiveQuantization' . mk) <$> takeText

instance ToText Av1SpatialAdaptiveQuantization where
    toText (Av1SpatialAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $Av1SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Av1SpatialAdaptiveQuantization where
    toEnum i = case i of
        0 -> ASAQDisabled
        1 -> ASAQEnabled
        _ -> (error . showText) $ "Unknown index for Av1SpatialAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        ASAQDisabled -> 0
        ASAQEnabled -> 1
        Av1SpatialAdaptiveQuantization' name -> (error . showText) $ "Unknown Av1SpatialAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $Av1SpatialAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Av1SpatialAdaptiveQuantization where
    minBound = ASAQDisabled
    maxBound = ASAQEnabled

instance Hashable     Av1SpatialAdaptiveQuantization
instance NFData       Av1SpatialAdaptiveQuantization
instance ToByteString Av1SpatialAdaptiveQuantization
instance ToQuery      Av1SpatialAdaptiveQuantization
instance ToHeader     Av1SpatialAdaptiveQuantization

instance ToJSON Av1SpatialAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Av1SpatialAdaptiveQuantization where
    parseJSON = parseJSONText "Av1SpatialAdaptiveQuantization"
