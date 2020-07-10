{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265FramerateConversionAlgorithm (
  H265FramerateConversionAlgorithm (
    ..
    , HFCADuplicateDrop
    , HFCAInterpolate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
data H265FramerateConversionAlgorithm = H265FramerateConversionAlgorithm' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern HFCADuplicateDrop :: H265FramerateConversionAlgorithm
pattern HFCADuplicateDrop = H265FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern HFCAInterpolate :: H265FramerateConversionAlgorithm
pattern HFCAInterpolate = H265FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  HFCADuplicateDrop,
  HFCAInterpolate,
  H265FramerateConversionAlgorithm' #-}

instance FromText H265FramerateConversionAlgorithm where
    parser = (H265FramerateConversionAlgorithm' . mk) <$> takeText

instance ToText H265FramerateConversionAlgorithm where
    toText (H265FramerateConversionAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $H265FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265FramerateConversionAlgorithm where
    toEnum i = case i of
        0 -> HFCADuplicateDrop
        1 -> HFCAInterpolate
        _ -> (error . showText) $ "Unknown index for H265FramerateConversionAlgorithm: " <> toText i
    fromEnum x = case x of
        HFCADuplicateDrop -> 0
        HFCAInterpolate -> 1
        H265FramerateConversionAlgorithm' name -> (error . showText) $ "Unknown H265FramerateConversionAlgorithm: " <> original name

-- | Represents the bounds of /known/ $H265FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265FramerateConversionAlgorithm where
    minBound = HFCADuplicateDrop
    maxBound = HFCAInterpolate

instance Hashable     H265FramerateConversionAlgorithm
instance NFData       H265FramerateConversionAlgorithm
instance ToByteString H265FramerateConversionAlgorithm
instance ToQuery      H265FramerateConversionAlgorithm
instance ToHeader     H265FramerateConversionAlgorithm

instance ToJSON H265FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON H265FramerateConversionAlgorithm where
    parseJSON = parseJSONText "H265FramerateConversionAlgorithm"
