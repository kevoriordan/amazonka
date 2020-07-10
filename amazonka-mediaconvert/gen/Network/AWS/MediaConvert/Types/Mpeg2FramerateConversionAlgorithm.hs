{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm (
  Mpeg2FramerateConversionAlgorithm (
    ..
    , MFCADuplicateDrop
    , MFCAInterpolate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
data Mpeg2FramerateConversionAlgorithm = Mpeg2FramerateConversionAlgorithm' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern MFCADuplicateDrop :: Mpeg2FramerateConversionAlgorithm
pattern MFCADuplicateDrop = Mpeg2FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern MFCAInterpolate :: Mpeg2FramerateConversionAlgorithm
pattern MFCAInterpolate = Mpeg2FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  MFCADuplicateDrop,
  MFCAInterpolate,
  Mpeg2FramerateConversionAlgorithm' #-}

instance FromText Mpeg2FramerateConversionAlgorithm where
    parser = (Mpeg2FramerateConversionAlgorithm' . mk) <$> takeText

instance ToText Mpeg2FramerateConversionAlgorithm where
    toText (Mpeg2FramerateConversionAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2FramerateConversionAlgorithm where
    toEnum i = case i of
        0 -> MFCADuplicateDrop
        1 -> MFCAInterpolate
        _ -> (error . showText) $ "Unknown index for Mpeg2FramerateConversionAlgorithm: " <> toText i
    fromEnum x = case x of
        MFCADuplicateDrop -> 0
        MFCAInterpolate -> 1
        Mpeg2FramerateConversionAlgorithm' name -> (error . showText) $ "Unknown Mpeg2FramerateConversionAlgorithm: " <> original name

-- | Represents the bounds of /known/ $Mpeg2FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2FramerateConversionAlgorithm where
    minBound = MFCADuplicateDrop
    maxBound = MFCAInterpolate

instance Hashable     Mpeg2FramerateConversionAlgorithm
instance NFData       Mpeg2FramerateConversionAlgorithm
instance ToByteString Mpeg2FramerateConversionAlgorithm
instance ToQuery      Mpeg2FramerateConversionAlgorithm
instance ToHeader     Mpeg2FramerateConversionAlgorithm

instance ToJSON Mpeg2FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON Mpeg2FramerateConversionAlgorithm where
    parseJSON = parseJSONText "Mpeg2FramerateConversionAlgorithm"
