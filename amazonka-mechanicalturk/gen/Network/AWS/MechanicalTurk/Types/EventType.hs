{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.EventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.EventType (
  EventType (
    ..
    , AssignmentAbandoned
    , AssignmentAccepted
    , AssignmentApproved
    , AssignmentRejected
    , AssignmentReturned
    , AssignmentSubmitted
    , HITCreated
    , HITDisposed
    , HITExpired
    , HITExtended
    , HITReviewable
    , Ping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventType = EventType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AssignmentAbandoned :: EventType
pattern AssignmentAbandoned = EventType' "AssignmentAbandoned"

pattern AssignmentAccepted :: EventType
pattern AssignmentAccepted = EventType' "AssignmentAccepted"

pattern AssignmentApproved :: EventType
pattern AssignmentApproved = EventType' "AssignmentApproved"

pattern AssignmentRejected :: EventType
pattern AssignmentRejected = EventType' "AssignmentRejected"

pattern AssignmentReturned :: EventType
pattern AssignmentReturned = EventType' "AssignmentReturned"

pattern AssignmentSubmitted :: EventType
pattern AssignmentSubmitted = EventType' "AssignmentSubmitted"

pattern HITCreated :: EventType
pattern HITCreated = EventType' "HITCreated"

pattern HITDisposed :: EventType
pattern HITDisposed = EventType' "HITDisposed"

pattern HITExpired :: EventType
pattern HITExpired = EventType' "HITExpired"

pattern HITExtended :: EventType
pattern HITExtended = EventType' "HITExtended"

pattern HITReviewable :: EventType
pattern HITReviewable = EventType' "HITReviewable"

pattern Ping :: EventType
pattern Ping = EventType' "Ping"

{-# COMPLETE
  AssignmentAbandoned,
  AssignmentAccepted,
  AssignmentApproved,
  AssignmentRejected,
  AssignmentReturned,
  AssignmentSubmitted,
  HITCreated,
  HITDisposed,
  HITExpired,
  HITExtended,
  HITReviewable,
  Ping,
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
        0 -> AssignmentAbandoned
        1 -> AssignmentAccepted
        2 -> AssignmentApproved
        3 -> AssignmentRejected
        4 -> AssignmentReturned
        5 -> AssignmentSubmitted
        6 -> HITCreated
        7 -> HITDisposed
        8 -> HITExpired
        9 -> HITExtended
        10 -> HITReviewable
        11 -> Ping
        _ -> (error . showText) $ "Unknown index for EventType: " <> toText i
    fromEnum x = case x of
        AssignmentAbandoned -> 0
        AssignmentAccepted -> 1
        AssignmentApproved -> 2
        AssignmentRejected -> 3
        AssignmentReturned -> 4
        AssignmentSubmitted -> 5
        HITCreated -> 6
        HITDisposed -> 7
        HITExpired -> 8
        HITExtended -> 9
        HITReviewable -> 10
        Ping -> 11
        EventType' name -> (error . showText) $ "Unknown EventType: " <> original name

-- | Represents the bounds of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventType where
    minBound = AssignmentAbandoned
    maxBound = Ping

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance ToJSON EventType where
    toJSON = toJSONText
