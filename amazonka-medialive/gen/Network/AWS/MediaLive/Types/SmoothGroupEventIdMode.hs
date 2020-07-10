{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupEventIdMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupEventIdMode (
  SmoothGroupEventIdMode (
    ..
    , NoEventId
    , UseConfigured
    , UseTimestamp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Event Id Mode
data SmoothGroupEventIdMode = SmoothGroupEventIdMode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern NoEventId :: SmoothGroupEventIdMode
pattern NoEventId = SmoothGroupEventIdMode' "NO_EVENT_ID"

pattern UseConfigured :: SmoothGroupEventIdMode
pattern UseConfigured = SmoothGroupEventIdMode' "USE_CONFIGURED"

pattern UseTimestamp :: SmoothGroupEventIdMode
pattern UseTimestamp = SmoothGroupEventIdMode' "USE_TIMESTAMP"

{-# COMPLETE
  NoEventId,
  UseConfigured,
  UseTimestamp,
  SmoothGroupEventIdMode' #-}

instance FromText SmoothGroupEventIdMode where
    parser = (SmoothGroupEventIdMode' . mk) <$> takeText

instance ToText SmoothGroupEventIdMode where
    toText (SmoothGroupEventIdMode' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupEventIdMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupEventIdMode where
    toEnum i = case i of
        0 -> NoEventId
        1 -> UseConfigured
        2 -> UseTimestamp
        _ -> (error . showText) $ "Unknown index for SmoothGroupEventIdMode: " <> toText i
    fromEnum x = case x of
        NoEventId -> 0
        UseConfigured -> 1
        UseTimestamp -> 2
        SmoothGroupEventIdMode' name -> (error . showText) $ "Unknown SmoothGroupEventIdMode: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupEventIdMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupEventIdMode where
    minBound = NoEventId
    maxBound = UseTimestamp

instance Hashable     SmoothGroupEventIdMode
instance NFData       SmoothGroupEventIdMode
instance ToByteString SmoothGroupEventIdMode
instance ToQuery      SmoothGroupEventIdMode
instance ToHeader     SmoothGroupEventIdMode

instance ToJSON SmoothGroupEventIdMode where
    toJSON = toJSONText

instance FromJSON SmoothGroupEventIdMode where
    parseJSON = parseJSONText "SmoothGroupEventIdMode"
