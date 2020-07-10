{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm (
  H264FramerateConversionAlgorithm (
    ..
    , HDuplicateDrop
    , HInterpolate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
data H264FramerateConversionAlgorithm = H264FramerateConversionAlgorithm' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern HDuplicateDrop :: H264FramerateConversionAlgorithm
pattern HDuplicateDrop = H264FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern HInterpolate :: H264FramerateConversionAlgorithm
pattern HInterpolate = H264FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  HDuplicateDrop,
  HInterpolate,
  H264FramerateConversionAlgorithm' #-}

instance FromText H264FramerateConversionAlgorithm where
    parser = (H264FramerateConversionAlgorithm' . mk) <$> takeText

instance ToText H264FramerateConversionAlgorithm where
    toText (H264FramerateConversionAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $H264FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264FramerateConversionAlgorithm where
    toEnum i = case i of
        0 -> HDuplicateDrop
        1 -> HInterpolate
        _ -> (error . showText) $ "Unknown index for H264FramerateConversionAlgorithm: " <> toText i
    fromEnum x = case x of
        HDuplicateDrop -> 0
        HInterpolate -> 1
        H264FramerateConversionAlgorithm' name -> (error . showText) $ "Unknown H264FramerateConversionAlgorithm: " <> original name

-- | Represents the bounds of /known/ $H264FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264FramerateConversionAlgorithm where
    minBound = HDuplicateDrop
    maxBound = HInterpolate

instance Hashable     H264FramerateConversionAlgorithm
instance NFData       H264FramerateConversionAlgorithm
instance ToByteString H264FramerateConversionAlgorithm
instance ToQuery      H264FramerateConversionAlgorithm
instance ToHeader     H264FramerateConversionAlgorithm

instance ToJSON H264FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON H264FramerateConversionAlgorithm where
    parseJSON = parseJSONText "H264FramerateConversionAlgorithm"
