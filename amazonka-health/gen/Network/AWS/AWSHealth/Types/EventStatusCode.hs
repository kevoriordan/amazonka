{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventStatusCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventStatusCode (
  EventStatusCode (
    ..
    , Closed
    , Open
    , Upcoming
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventStatusCode = EventStatusCode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Closed :: EventStatusCode
pattern Closed = EventStatusCode' "closed"

pattern Open :: EventStatusCode
pattern Open = EventStatusCode' "open"

pattern Upcoming :: EventStatusCode
pattern Upcoming = EventStatusCode' "upcoming"

{-# COMPLETE
  Closed,
  Open,
  Upcoming,
  EventStatusCode' #-}

instance FromText EventStatusCode where
    parser = (EventStatusCode' . mk) <$> takeText

instance ToText EventStatusCode where
    toText (EventStatusCode' ci) = original ci

-- | Represents an enum of /known/ $EventStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventStatusCode where
    toEnum i = case i of
        0 -> Closed
        1 -> Open
        2 -> Upcoming
        _ -> (error . showText) $ "Unknown index for EventStatusCode: " <> toText i
    fromEnum x = case x of
        Closed -> 0
        Open -> 1
        Upcoming -> 2
        EventStatusCode' name -> (error . showText) $ "Unknown EventStatusCode: " <> original name

-- | Represents the bounds of /known/ $EventStatusCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventStatusCode where
    minBound = Closed
    maxBound = Upcoming

instance Hashable     EventStatusCode
instance NFData       EventStatusCode
instance ToByteString EventStatusCode
instance ToQuery      EventStatusCode
instance ToHeader     EventStatusCode

instance ToJSON EventStatusCode where
    toJSON = toJSONText

instance FromJSON EventStatusCode where
    parseJSON = parseJSONText "EventStatusCode"
