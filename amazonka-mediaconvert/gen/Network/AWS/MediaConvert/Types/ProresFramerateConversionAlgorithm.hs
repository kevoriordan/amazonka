{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm (
  ProresFramerateConversionAlgorithm (
    ..
    , PFCADuplicateDrop
    , PFCAInterpolate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
data ProresFramerateConversionAlgorithm = ProresFramerateConversionAlgorithm' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern PFCADuplicateDrop :: ProresFramerateConversionAlgorithm
pattern PFCADuplicateDrop = ProresFramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern PFCAInterpolate :: ProresFramerateConversionAlgorithm
pattern PFCAInterpolate = ProresFramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  PFCADuplicateDrop,
  PFCAInterpolate,
  ProresFramerateConversionAlgorithm' #-}

instance FromText ProresFramerateConversionAlgorithm where
    parser = (ProresFramerateConversionAlgorithm' . mk) <$> takeText

instance ToText ProresFramerateConversionAlgorithm where
    toText (ProresFramerateConversionAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $ProresFramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresFramerateConversionAlgorithm where
    toEnum i = case i of
        0 -> PFCADuplicateDrop
        1 -> PFCAInterpolate
        _ -> (error . showText) $ "Unknown index for ProresFramerateConversionAlgorithm: " <> toText i
    fromEnum x = case x of
        PFCADuplicateDrop -> 0
        PFCAInterpolate -> 1
        ProresFramerateConversionAlgorithm' name -> (error . showText) $ "Unknown ProresFramerateConversionAlgorithm: " <> original name

-- | Represents the bounds of /known/ $ProresFramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresFramerateConversionAlgorithm where
    minBound = PFCADuplicateDrop
    maxBound = PFCAInterpolate

instance Hashable     ProresFramerateConversionAlgorithm
instance NFData       ProresFramerateConversionAlgorithm
instance ToByteString ProresFramerateConversionAlgorithm
instance ToQuery      ProresFramerateConversionAlgorithm
instance ToHeader     ProresFramerateConversionAlgorithm

instance ToJSON ProresFramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON ProresFramerateConversionAlgorithm where
    parseJSON = parseJSONText "ProresFramerateConversionAlgorithm"
