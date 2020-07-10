{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseReducerFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.NoiseReducerFilter (
  NoiseReducerFilter (
    ..
    , Bilateral
    , Conserve
    , Gaussian
    , Lanczos
    , Mean
    , Sharpen
    , Spatial
    , Temporal
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral preserves edges while reducing noise. * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) do convolution filtering. * Conserve does min/max noise reduction. * Spatial does frequency-domain filtering based on JND principles. * Temporal optimizes video quality for complex motion.
data NoiseReducerFilter = NoiseReducerFilter' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Bilateral :: NoiseReducerFilter
pattern Bilateral = NoiseReducerFilter' "BILATERAL"

pattern Conserve :: NoiseReducerFilter
pattern Conserve = NoiseReducerFilter' "CONSERVE"

pattern Gaussian :: NoiseReducerFilter
pattern Gaussian = NoiseReducerFilter' "GAUSSIAN"

pattern Lanczos :: NoiseReducerFilter
pattern Lanczos = NoiseReducerFilter' "LANCZOS"

pattern Mean :: NoiseReducerFilter
pattern Mean = NoiseReducerFilter' "MEAN"

pattern Sharpen :: NoiseReducerFilter
pattern Sharpen = NoiseReducerFilter' "SHARPEN"

pattern Spatial :: NoiseReducerFilter
pattern Spatial = NoiseReducerFilter' "SPATIAL"

pattern Temporal :: NoiseReducerFilter
pattern Temporal = NoiseReducerFilter' "TEMPORAL"

{-# COMPLETE
  Bilateral,
  Conserve,
  Gaussian,
  Lanczos,
  Mean,
  Sharpen,
  Spatial,
  Temporal,
  NoiseReducerFilter' #-}

instance FromText NoiseReducerFilter where
    parser = (NoiseReducerFilter' . mk) <$> takeText

instance ToText NoiseReducerFilter where
    toText (NoiseReducerFilter' ci) = original ci

-- | Represents an enum of /known/ $NoiseReducerFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NoiseReducerFilter where
    toEnum i = case i of
        0 -> Bilateral
        1 -> Conserve
        2 -> Gaussian
        3 -> Lanczos
        4 -> Mean
        5 -> Sharpen
        6 -> Spatial
        7 -> Temporal
        _ -> (error . showText) $ "Unknown index for NoiseReducerFilter: " <> toText i
    fromEnum x = case x of
        Bilateral -> 0
        Conserve -> 1
        Gaussian -> 2
        Lanczos -> 3
        Mean -> 4
        Sharpen -> 5
        Spatial -> 6
        Temporal -> 7
        NoiseReducerFilter' name -> (error . showText) $ "Unknown NoiseReducerFilter: " <> original name

-- | Represents the bounds of /known/ $NoiseReducerFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NoiseReducerFilter where
    minBound = Bilateral
    maxBound = Temporal

instance Hashable     NoiseReducerFilter
instance NFData       NoiseReducerFilter
instance ToByteString NoiseReducerFilter
instance ToQuery      NoiseReducerFilter
instance ToHeader     NoiseReducerFilter

instance ToJSON NoiseReducerFilter where
    toJSON = toJSONText

instance FromJSON NoiseReducerFilter where
    parseJSON = parseJSONText "NoiseReducerFilter"
