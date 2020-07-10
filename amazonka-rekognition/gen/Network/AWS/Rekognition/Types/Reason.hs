{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Reason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Reason (
  Reason (
    ..
    , ExceedsMaxFaces
    , ExtremePose
    , LowBrightness
    , LowConfidence
    , LowFaceQuality
    , LowSharpness
    , SmallBoundingBox
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Reason = Reason' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern ExceedsMaxFaces :: Reason
pattern ExceedsMaxFaces = Reason' "EXCEEDS_MAX_FACES"

pattern ExtremePose :: Reason
pattern ExtremePose = Reason' "EXTREME_POSE"

pattern LowBrightness :: Reason
pattern LowBrightness = Reason' "LOW_BRIGHTNESS"

pattern LowConfidence :: Reason
pattern LowConfidence = Reason' "LOW_CONFIDENCE"

pattern LowFaceQuality :: Reason
pattern LowFaceQuality = Reason' "LOW_FACE_QUALITY"

pattern LowSharpness :: Reason
pattern LowSharpness = Reason' "LOW_SHARPNESS"

pattern SmallBoundingBox :: Reason
pattern SmallBoundingBox = Reason' "SMALL_BOUNDING_BOX"

{-# COMPLETE
  ExceedsMaxFaces,
  ExtremePose,
  LowBrightness,
  LowConfidence,
  LowFaceQuality,
  LowSharpness,
  SmallBoundingBox,
  Reason' #-}

instance FromText Reason where
    parser = (Reason' . mk) <$> takeText

instance ToText Reason where
    toText (Reason' ci) = original ci

-- | Represents an enum of /known/ $Reason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Reason where
    toEnum i = case i of
        0 -> ExceedsMaxFaces
        1 -> ExtremePose
        2 -> LowBrightness
        3 -> LowConfidence
        4 -> LowFaceQuality
        5 -> LowSharpness
        6 -> SmallBoundingBox
        _ -> (error . showText) $ "Unknown index for Reason: " <> toText i
    fromEnum x = case x of
        ExceedsMaxFaces -> 0
        ExtremePose -> 1
        LowBrightness -> 2
        LowConfidence -> 3
        LowFaceQuality -> 4
        LowSharpness -> 5
        SmallBoundingBox -> 6
        Reason' name -> (error . showText) $ "Unknown Reason: " <> original name

-- | Represents the bounds of /known/ $Reason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Reason where
    minBound = ExceedsMaxFaces
    maxBound = SmallBoundingBox

instance Hashable     Reason
instance NFData       Reason
instance ToByteString Reason
instance ToQuery      Reason
instance ToHeader     Reason

instance FromJSON Reason where
    parseJSON = parseJSONText "Reason"
