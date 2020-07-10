{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupTimestampOffsetMode (
  SmoothGroupTimestampOffsetMode (
    ..
    , UseConfiguredOffset
    , UseEventStartDate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Timestamp Offset Mode
data SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern UseConfiguredOffset :: SmoothGroupTimestampOffsetMode
pattern UseConfiguredOffset = SmoothGroupTimestampOffsetMode' "USE_CONFIGURED_OFFSET"

pattern UseEventStartDate :: SmoothGroupTimestampOffsetMode
pattern UseEventStartDate = SmoothGroupTimestampOffsetMode' "USE_EVENT_START_DATE"

{-# COMPLETE
  UseConfiguredOffset,
  UseEventStartDate,
  SmoothGroupTimestampOffsetMode' #-}

instance FromText SmoothGroupTimestampOffsetMode where
    parser = (SmoothGroupTimestampOffsetMode' . mk) <$> takeText

instance ToText SmoothGroupTimestampOffsetMode where
    toText (SmoothGroupTimestampOffsetMode' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupTimestampOffsetMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupTimestampOffsetMode where
    toEnum i = case i of
        0 -> UseConfiguredOffset
        1 -> UseEventStartDate
        _ -> (error . showText) $ "Unknown index for SmoothGroupTimestampOffsetMode: " <> toText i
    fromEnum x = case x of
        UseConfiguredOffset -> 0
        UseEventStartDate -> 1
        SmoothGroupTimestampOffsetMode' name -> (error . showText) $ "Unknown SmoothGroupTimestampOffsetMode: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupTimestampOffsetMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupTimestampOffsetMode where
    minBound = UseConfiguredOffset
    maxBound = UseEventStartDate

instance Hashable     SmoothGroupTimestampOffsetMode
instance NFData       SmoothGroupTimestampOffsetMode
instance ToByteString SmoothGroupTimestampOffsetMode
instance ToQuery      SmoothGroupTimestampOffsetMode
instance ToHeader     SmoothGroupTimestampOffsetMode

instance ToJSON SmoothGroupTimestampOffsetMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupTimestampOffsetMode where
    parseJSON = parseJSONText "SmoothGroupTimestampOffsetMode"
