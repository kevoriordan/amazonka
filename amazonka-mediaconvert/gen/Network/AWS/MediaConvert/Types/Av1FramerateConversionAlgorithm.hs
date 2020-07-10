{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1FramerateConversionAlgorithm (
  Av1FramerateConversionAlgorithm (
    ..
    , DuplicateDrop
    , Interpolate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INTERPOLATE, produces smoother motion during frame rate conversion.
data Av1FramerateConversionAlgorithm = Av1FramerateConversionAlgorithm' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern DuplicateDrop :: Av1FramerateConversionAlgorithm
pattern DuplicateDrop = Av1FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern Interpolate :: Av1FramerateConversionAlgorithm
pattern Interpolate = Av1FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  DuplicateDrop,
  Interpolate,
  Av1FramerateConversionAlgorithm' #-}

instance FromText Av1FramerateConversionAlgorithm where
    parser = (Av1FramerateConversionAlgorithm' . mk) <$> takeText

instance ToText Av1FramerateConversionAlgorithm where
    toText (Av1FramerateConversionAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $Av1FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Av1FramerateConversionAlgorithm where
    toEnum i = case i of
        0 -> DuplicateDrop
        1 -> Interpolate
        _ -> (error . showText) $ "Unknown index for Av1FramerateConversionAlgorithm: " <> toText i
    fromEnum x = case x of
        DuplicateDrop -> 0
        Interpolate -> 1
        Av1FramerateConversionAlgorithm' name -> (error . showText) $ "Unknown Av1FramerateConversionAlgorithm: " <> original name

-- | Represents the bounds of /known/ $Av1FramerateConversionAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Av1FramerateConversionAlgorithm where
    minBound = DuplicateDrop
    maxBound = Interpolate

instance Hashable     Av1FramerateConversionAlgorithm
instance NFData       Av1FramerateConversionAlgorithm
instance ToByteString Av1FramerateConversionAlgorithm
instance ToQuery      Av1FramerateConversionAlgorithm
instance ToHeader     Av1FramerateConversionAlgorithm

instance ToJSON Av1FramerateConversionAlgorithm where
    toJSON = toJSONText

instance FromJSON Av1FramerateConversionAlgorithm where
    parseJSON = parseJSONText "Av1FramerateConversionAlgorithm"
