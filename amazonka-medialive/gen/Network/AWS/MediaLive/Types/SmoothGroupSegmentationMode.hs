{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode (
  SmoothGroupSegmentationMode (
    ..
    , UseInputSegmentation
    , UseSegmentDuration
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Segmentation Mode
data SmoothGroupSegmentationMode = SmoothGroupSegmentationMode' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern UseInputSegmentation :: SmoothGroupSegmentationMode
pattern UseInputSegmentation = SmoothGroupSegmentationMode' "USE_INPUT_SEGMENTATION"

pattern UseSegmentDuration :: SmoothGroupSegmentationMode
pattern UseSegmentDuration = SmoothGroupSegmentationMode' "USE_SEGMENT_DURATION"

{-# COMPLETE
  UseInputSegmentation,
  UseSegmentDuration,
  SmoothGroupSegmentationMode' #-}

instance FromText SmoothGroupSegmentationMode where
    parser = (SmoothGroupSegmentationMode' . mk) <$> takeText

instance ToText SmoothGroupSegmentationMode where
    toText (SmoothGroupSegmentationMode' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupSegmentationMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupSegmentationMode where
    toEnum i = case i of
        0 -> UseInputSegmentation
        1 -> UseSegmentDuration
        _ -> (error . showText) $ "Unknown index for SmoothGroupSegmentationMode: " <> toText i
    fromEnum x = case x of
        UseInputSegmentation -> 0
        UseSegmentDuration -> 1
        SmoothGroupSegmentationMode' name -> (error . showText) $ "Unknown SmoothGroupSegmentationMode: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupSegmentationMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupSegmentationMode where
    minBound = UseInputSegmentation
    maxBound = UseSegmentDuration

instance Hashable     SmoothGroupSegmentationMode
instance NFData       SmoothGroupSegmentationMode
instance ToByteString SmoothGroupSegmentationMode
instance ToQuery      SmoothGroupSegmentationMode
instance ToHeader     SmoothGroupSegmentationMode

instance ToJSON SmoothGroupSegmentationMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupSegmentationMode where
    parseJSON = parseJSONText "SmoothGroupSegmentationMode"
