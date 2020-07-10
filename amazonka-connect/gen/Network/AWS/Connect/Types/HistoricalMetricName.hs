{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HistoricalMetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HistoricalMetricName (
  HistoricalMetricName (
    ..
    , APIContactsHandled
    , AbandonTime
    , AfterContactWorkTime
    , CallbackContactsHandled
    , ContactsAbandoned
    , ContactsAgentHungUpFirst
    , ContactsConsulted
    , ContactsHandled
    , ContactsHandledIncoming
    , ContactsHandledOutbound
    , ContactsHoldAbandons
    , ContactsMissed
    , ContactsQueued
    , ContactsTransferredIn
    , ContactsTransferredInFromQueue
    , ContactsTransferredOut
    , ContactsTransferredOutFromQueue
    , HandleTime
    , HoldTime
    , InteractionAndHoldTime
    , InteractionTime
    , Occupancy
    , QueueAnswerTime
    , QueuedTime
    , ServiceLevel
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The historical metric names.
--
--
data HistoricalMetricName = HistoricalMetricName' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern APIContactsHandled :: HistoricalMetricName
pattern APIContactsHandled = HistoricalMetricName' "API_CONTACTS_HANDLED"

pattern AbandonTime :: HistoricalMetricName
pattern AbandonTime = HistoricalMetricName' "ABANDON_TIME"

pattern AfterContactWorkTime :: HistoricalMetricName
pattern AfterContactWorkTime = HistoricalMetricName' "AFTER_CONTACT_WORK_TIME"

pattern CallbackContactsHandled :: HistoricalMetricName
pattern CallbackContactsHandled = HistoricalMetricName' "CALLBACK_CONTACTS_HANDLED"

pattern ContactsAbandoned :: HistoricalMetricName
pattern ContactsAbandoned = HistoricalMetricName' "CONTACTS_ABANDONED"

pattern ContactsAgentHungUpFirst :: HistoricalMetricName
pattern ContactsAgentHungUpFirst = HistoricalMetricName' "CONTACTS_AGENT_HUNG_UP_FIRST"

pattern ContactsConsulted :: HistoricalMetricName
pattern ContactsConsulted = HistoricalMetricName' "CONTACTS_CONSULTED"

pattern ContactsHandled :: HistoricalMetricName
pattern ContactsHandled = HistoricalMetricName' "CONTACTS_HANDLED"

pattern ContactsHandledIncoming :: HistoricalMetricName
pattern ContactsHandledIncoming = HistoricalMetricName' "CONTACTS_HANDLED_INCOMING"

pattern ContactsHandledOutbound :: HistoricalMetricName
pattern ContactsHandledOutbound = HistoricalMetricName' "CONTACTS_HANDLED_OUTBOUND"

pattern ContactsHoldAbandons :: HistoricalMetricName
pattern ContactsHoldAbandons = HistoricalMetricName' "CONTACTS_HOLD_ABANDONS"

pattern ContactsMissed :: HistoricalMetricName
pattern ContactsMissed = HistoricalMetricName' "CONTACTS_MISSED"

pattern ContactsQueued :: HistoricalMetricName
pattern ContactsQueued = HistoricalMetricName' "CONTACTS_QUEUED"

pattern ContactsTransferredIn :: HistoricalMetricName
pattern ContactsTransferredIn = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN"

pattern ContactsTransferredInFromQueue :: HistoricalMetricName
pattern ContactsTransferredInFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_IN_FROM_QUEUE"

pattern ContactsTransferredOut :: HistoricalMetricName
pattern ContactsTransferredOut = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT"

pattern ContactsTransferredOutFromQueue :: HistoricalMetricName
pattern ContactsTransferredOutFromQueue = HistoricalMetricName' "CONTACTS_TRANSFERRED_OUT_FROM_QUEUE"

pattern HandleTime :: HistoricalMetricName
pattern HandleTime = HistoricalMetricName' "HANDLE_TIME"

pattern HoldTime :: HistoricalMetricName
pattern HoldTime = HistoricalMetricName' "HOLD_TIME"

pattern InteractionAndHoldTime :: HistoricalMetricName
pattern InteractionAndHoldTime = HistoricalMetricName' "INTERACTION_AND_HOLD_TIME"

pattern InteractionTime :: HistoricalMetricName
pattern InteractionTime = HistoricalMetricName' "INTERACTION_TIME"

pattern Occupancy :: HistoricalMetricName
pattern Occupancy = HistoricalMetricName' "OCCUPANCY"

pattern QueueAnswerTime :: HistoricalMetricName
pattern QueueAnswerTime = HistoricalMetricName' "QUEUE_ANSWER_TIME"

pattern QueuedTime :: HistoricalMetricName
pattern QueuedTime = HistoricalMetricName' "QUEUED_TIME"

pattern ServiceLevel :: HistoricalMetricName
pattern ServiceLevel = HistoricalMetricName' "SERVICE_LEVEL"

{-# COMPLETE
  APIContactsHandled,
  AbandonTime,
  AfterContactWorkTime,
  CallbackContactsHandled,
  ContactsAbandoned,
  ContactsAgentHungUpFirst,
  ContactsConsulted,
  ContactsHandled,
  ContactsHandledIncoming,
  ContactsHandledOutbound,
  ContactsHoldAbandons,
  ContactsMissed,
  ContactsQueued,
  ContactsTransferredIn,
  ContactsTransferredInFromQueue,
  ContactsTransferredOut,
  ContactsTransferredOutFromQueue,
  HandleTime,
  HoldTime,
  InteractionAndHoldTime,
  InteractionTime,
  Occupancy,
  QueueAnswerTime,
  QueuedTime,
  ServiceLevel,
  HistoricalMetricName' #-}

instance FromText HistoricalMetricName where
    parser = (HistoricalMetricName' . mk) <$> takeText

instance ToText HistoricalMetricName where
    toText (HistoricalMetricName' ci) = original ci

-- | Represents an enum of /known/ $HistoricalMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HistoricalMetricName where
    toEnum i = case i of
        0 -> APIContactsHandled
        1 -> AbandonTime
        2 -> AfterContactWorkTime
        3 -> CallbackContactsHandled
        4 -> ContactsAbandoned
        5 -> ContactsAgentHungUpFirst
        6 -> ContactsConsulted
        7 -> ContactsHandled
        8 -> ContactsHandledIncoming
        9 -> ContactsHandledOutbound
        10 -> ContactsHoldAbandons
        11 -> ContactsMissed
        12 -> ContactsQueued
        13 -> ContactsTransferredIn
        14 -> ContactsTransferredInFromQueue
        15 -> ContactsTransferredOut
        16 -> ContactsTransferredOutFromQueue
        17 -> HandleTime
        18 -> HoldTime
        19 -> InteractionAndHoldTime
        20 -> InteractionTime
        21 -> Occupancy
        22 -> QueueAnswerTime
        23 -> QueuedTime
        24 -> ServiceLevel
        _ -> (error . showText) $ "Unknown index for HistoricalMetricName: " <> toText i
    fromEnum x = case x of
        APIContactsHandled -> 0
        AbandonTime -> 1
        AfterContactWorkTime -> 2
        CallbackContactsHandled -> 3
        ContactsAbandoned -> 4
        ContactsAgentHungUpFirst -> 5
        ContactsConsulted -> 6
        ContactsHandled -> 7
        ContactsHandledIncoming -> 8
        ContactsHandledOutbound -> 9
        ContactsHoldAbandons -> 10
        ContactsMissed -> 11
        ContactsQueued -> 12
        ContactsTransferredIn -> 13
        ContactsTransferredInFromQueue -> 14
        ContactsTransferredOut -> 15
        ContactsTransferredOutFromQueue -> 16
        HandleTime -> 17
        HoldTime -> 18
        InteractionAndHoldTime -> 19
        InteractionTime -> 20
        Occupancy -> 21
        QueueAnswerTime -> 22
        QueuedTime -> 23
        ServiceLevel -> 24
        HistoricalMetricName' name -> (error . showText) $ "Unknown HistoricalMetricName: " <> original name

-- | Represents the bounds of /known/ $HistoricalMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HistoricalMetricName where
    minBound = APIContactsHandled
    maxBound = ServiceLevel

instance Hashable     HistoricalMetricName
instance NFData       HistoricalMetricName
instance ToByteString HistoricalMetricName
instance ToQuery      HistoricalMetricName
instance ToHeader     HistoricalMetricName

instance ToJSON HistoricalMetricName where
    toJSON = toJSONText

instance FromJSON HistoricalMetricName where
    parseJSON = parseJSONText "HistoricalMetricName"
