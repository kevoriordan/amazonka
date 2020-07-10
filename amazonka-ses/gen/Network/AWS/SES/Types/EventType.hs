{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.EventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.EventType (
  EventType (
    ..
    , ETBounce
    , ETClick
    , ETComplaint
    , ETDelivery
    , ETOpen
    , ETReject
    , ETRenderingFailure
    , ETSend
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventType = EventType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern ETBounce :: EventType
pattern ETBounce = EventType' "bounce"

pattern ETClick :: EventType
pattern ETClick = EventType' "click"

pattern ETComplaint :: EventType
pattern ETComplaint = EventType' "complaint"

pattern ETDelivery :: EventType
pattern ETDelivery = EventType' "delivery"

pattern ETOpen :: EventType
pattern ETOpen = EventType' "open"

pattern ETReject :: EventType
pattern ETReject = EventType' "reject"

pattern ETRenderingFailure :: EventType
pattern ETRenderingFailure = EventType' "renderingFailure"

pattern ETSend :: EventType
pattern ETSend = EventType' "send"

{-# COMPLETE
  ETBounce,
  ETClick,
  ETComplaint,
  ETDelivery,
  ETOpen,
  ETReject,
  ETRenderingFailure,
  ETSend,
  EventType' #-}

instance FromText EventType where
    parser = (EventType' . mk) <$> takeText

instance ToText EventType where
    toText (EventType' ci) = original ci

-- | Represents an enum of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventType where
    toEnum i = case i of
        0 -> ETBounce
        1 -> ETClick
        2 -> ETComplaint
        3 -> ETDelivery
        4 -> ETOpen
        5 -> ETReject
        6 -> ETRenderingFailure
        7 -> ETSend
        _ -> (error . showText) $ "Unknown index for EventType: " <> toText i
    fromEnum x = case x of
        ETBounce -> 0
        ETClick -> 1
        ETComplaint -> 2
        ETDelivery -> 3
        ETOpen -> 4
        ETReject -> 5
        ETRenderingFailure -> 6
        ETSend -> 7
        EventType' name -> (error . showText) $ "Unknown EventType: " <> original name

-- | Represents the bounds of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventType where
    minBound = ETBounce
    maxBound = ETSend

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromXML EventType where
    parseXML = parseXMLText "EventType"
