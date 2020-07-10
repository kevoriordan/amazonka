{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.EventSourceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.EventSourceState (
  EventSourceState (
    ..
    , Active
    , Deleted
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventSourceState = EventSourceState' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Active :: EventSourceState
pattern Active = EventSourceState' "ACTIVE"

pattern Deleted :: EventSourceState
pattern Deleted = EventSourceState' "DELETED"

pattern Pending :: EventSourceState
pattern Pending = EventSourceState' "PENDING"

{-# COMPLETE
  Active,
  Deleted,
  Pending,
  EventSourceState' #-}

instance FromText EventSourceState where
    parser = (EventSourceState' . mk) <$> takeText

instance ToText EventSourceState where
    toText (EventSourceState' ci) = original ci

-- | Represents an enum of /known/ $EventSourceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventSourceState where
    toEnum i = case i of
        0 -> Active
        1 -> Deleted
        2 -> Pending
        _ -> (error . showText) $ "Unknown index for EventSourceState: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Deleted -> 1
        Pending -> 2
        EventSourceState' name -> (error . showText) $ "Unknown EventSourceState: " <> original name

-- | Represents the bounds of /known/ $EventSourceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventSourceState where
    minBound = Active
    maxBound = Pending

instance Hashable     EventSourceState
instance NFData       EventSourceState
instance ToByteString EventSourceState
instance ToQuery      EventSourceState
instance ToHeader     EventSourceState

instance FromJSON EventSourceState where
    parseJSON = parseJSONText "EventSourceState"
