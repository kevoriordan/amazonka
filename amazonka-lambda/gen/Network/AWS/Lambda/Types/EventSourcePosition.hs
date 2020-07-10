{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.EventSourcePosition (
  EventSourcePosition (
    ..
    , AtTimestamp
    , Latest
    , TrimHorizon
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventSourcePosition = EventSourcePosition' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern AtTimestamp :: EventSourcePosition
pattern AtTimestamp = EventSourcePosition' "AT_TIMESTAMP"

pattern Latest :: EventSourcePosition
pattern Latest = EventSourcePosition' "LATEST"

pattern TrimHorizon :: EventSourcePosition
pattern TrimHorizon = EventSourcePosition' "TRIM_HORIZON"

{-# COMPLETE
  AtTimestamp,
  Latest,
  TrimHorizon,
  EventSourcePosition' #-}

instance FromText EventSourcePosition where
    parser = (EventSourcePosition' . mk) <$> takeText

instance ToText EventSourcePosition where
    toText (EventSourcePosition' ci) = original ci

-- | Represents an enum of /known/ $EventSourcePosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventSourcePosition where
    toEnum i = case i of
        0 -> AtTimestamp
        1 -> Latest
        2 -> TrimHorizon
        _ -> (error . showText) $ "Unknown index for EventSourcePosition: " <> toText i
    fromEnum x = case x of
        AtTimestamp -> 0
        Latest -> 1
        TrimHorizon -> 2
        EventSourcePosition' name -> (error . showText) $ "Unknown EventSourcePosition: " <> original name

-- | Represents the bounds of /known/ $EventSourcePosition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventSourcePosition where
    minBound = AtTimestamp
    maxBound = TrimHorizon

instance Hashable     EventSourcePosition
instance NFData       EventSourcePosition
instance ToByteString EventSourcePosition
instance ToQuery      EventSourcePosition
instance ToHeader     EventSourcePosition

instance ToJSON EventSourcePosition where
    toJSON = toJSONText
