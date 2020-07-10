{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimecodeBurninPosition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.TimecodeBurninPosition (
  TimecodeBurninPosition (
    ..
    , BottomCenter
    , BottomLeft
    , BottomRight
    , MiddleCenter
    , MiddleLeft
    , MiddleRight
    , TopCenter
    , TopLeft
    , TopRight
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
data TimecodeBurninPosition = TimecodeBurninPosition' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern BottomCenter :: TimecodeBurninPosition
pattern BottomCenter = TimecodeBurninPosition' "BOTTOM_CENTER"

pattern BottomLeft :: TimecodeBurninPosition
pattern BottomLeft = TimecodeBurninPosition' "BOTTOM_LEFT"

pattern BottomRight :: TimecodeBurninPosition
pattern BottomRight = TimecodeBurninPosition' "BOTTOM_RIGHT"

pattern MiddleCenter :: TimecodeBurninPosition
pattern MiddleCenter = TimecodeBurninPosition' "MIDDLE_CENTER"

pattern MiddleLeft :: TimecodeBurninPosition
pattern MiddleLeft = TimecodeBurninPosition' "MIDDLE_LEFT"

pattern MiddleRight :: TimecodeBurninPosition
pattern MiddleRight = TimecodeBurninPosition' "MIDDLE_RIGHT"

pattern TopCenter :: TimecodeBurninPosition
pattern TopCenter = TimecodeBurninPosition' "TOP_CENTER"

pattern TopLeft :: TimecodeBurninPosition
pattern TopLeft = TimecodeBurninPosition' "TOP_LEFT"

pattern TopRight :: TimecodeBurninPosition
pattern TopRight = TimecodeBurninPosition' "TOP_RIGHT"

{-# COMPLETE
  BottomCenter,
  BottomLeft,
  BottomRight,
  MiddleCenter,
  MiddleLeft,
  MiddleRight,
  TopCenter,
  TopLeft,
  TopRight,
  TimecodeBurninPosition' #-}

instance FromText TimecodeBurninPosition where
    parser = (TimecodeBurninPosition' . mk) <$> takeText

instance ToText TimecodeBurninPosition where
    toText (TimecodeBurninPosition' ci) = original ci

-- | Represents an enum of /known/ $TimecodeBurninPosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TimecodeBurninPosition where
    toEnum i = case i of
        0 -> BottomCenter
        1 -> BottomLeft
        2 -> BottomRight
        3 -> MiddleCenter
        4 -> MiddleLeft
        5 -> MiddleRight
        6 -> TopCenter
        7 -> TopLeft
        8 -> TopRight
        _ -> (error . showText) $ "Unknown index for TimecodeBurninPosition: " <> toText i
    fromEnum x = case x of
        BottomCenter -> 0
        BottomLeft -> 1
        BottomRight -> 2
        MiddleCenter -> 3
        MiddleLeft -> 4
        MiddleRight -> 5
        TopCenter -> 6
        TopLeft -> 7
        TopRight -> 8
        TimecodeBurninPosition' name -> (error . showText) $ "Unknown TimecodeBurninPosition: " <> original name

-- | Represents the bounds of /known/ $TimecodeBurninPosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimecodeBurninPosition where
    minBound = BottomCenter
    maxBound = TopRight

instance Hashable     TimecodeBurninPosition
instance NFData       TimecodeBurninPosition
instance ToByteString TimecodeBurninPosition
instance ToQuery      TimecodeBurninPosition
instance ToHeader     TimecodeBurninPosition

instance ToJSON TimecodeBurninPosition where
    toJSON = toJSONText

instance FromJSON TimecodeBurninPosition where
    parseJSON = parseJSONText "TimecodeBurninPosition"
