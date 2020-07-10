{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SegmentTemplateFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.SegmentTemplateFormat (
  SegmentTemplateFormat (
    ..
    , NumberWithDuration
    , NumberWithTimeline
    , TimeWithTimeline
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SegmentTemplateFormat = SegmentTemplateFormat' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern NumberWithDuration :: SegmentTemplateFormat
pattern NumberWithDuration = SegmentTemplateFormat' "NUMBER_WITH_DURATION"

pattern NumberWithTimeline :: SegmentTemplateFormat
pattern NumberWithTimeline = SegmentTemplateFormat' "NUMBER_WITH_TIMELINE"

pattern TimeWithTimeline :: SegmentTemplateFormat
pattern TimeWithTimeline = SegmentTemplateFormat' "TIME_WITH_TIMELINE"

{-# COMPLETE
  NumberWithDuration,
  NumberWithTimeline,
  TimeWithTimeline,
  SegmentTemplateFormat' #-}

instance FromText SegmentTemplateFormat where
    parser = (SegmentTemplateFormat' . mk) <$> takeText

instance ToText SegmentTemplateFormat where
    toText (SegmentTemplateFormat' ci) = original ci

-- | Represents an enum of /known/ $SegmentTemplateFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SegmentTemplateFormat where
    toEnum i = case i of
        0 -> NumberWithDuration
        1 -> NumberWithTimeline
        2 -> TimeWithTimeline
        _ -> (error . showText) $ "Unknown index for SegmentTemplateFormat: " <> toText i
    fromEnum x = case x of
        NumberWithDuration -> 0
        NumberWithTimeline -> 1
        TimeWithTimeline -> 2
        SegmentTemplateFormat' name -> (error . showText) $ "Unknown SegmentTemplateFormat: " <> original name

-- | Represents the bounds of /known/ $SegmentTemplateFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SegmentTemplateFormat where
    minBound = NumberWithDuration
    maxBound = TimeWithTimeline

instance Hashable     SegmentTemplateFormat
instance NFData       SegmentTemplateFormat
instance ToByteString SegmentTemplateFormat
instance ToQuery      SegmentTemplateFormat
instance ToHeader     SegmentTemplateFormat

instance ToJSON SegmentTemplateFormat where
    toJSON = toJSONText

instance FromJSON SegmentTemplateFormat where
    parseJSON = parseJSONText "SegmentTemplateFormat"
