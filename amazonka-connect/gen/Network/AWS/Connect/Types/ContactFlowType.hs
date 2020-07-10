{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.ContactFlowType (
  ContactFlowType (
    ..
    , AgentHold
    , AgentTransfer
    , AgentWhisper
    , ContactFlow
    , CustomerHold
    , CustomerQueue
    , CustomerWhisper
    , OutboundWhisper
    , QueueTransfer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContactFlowType = ContactFlowType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AgentHold :: ContactFlowType
pattern AgentHold = ContactFlowType' "AGENT_HOLD"

pattern AgentTransfer :: ContactFlowType
pattern AgentTransfer = ContactFlowType' "AGENT_TRANSFER"

pattern AgentWhisper :: ContactFlowType
pattern AgentWhisper = ContactFlowType' "AGENT_WHISPER"

pattern ContactFlow :: ContactFlowType
pattern ContactFlow = ContactFlowType' "CONTACT_FLOW"

pattern CustomerHold :: ContactFlowType
pattern CustomerHold = ContactFlowType' "CUSTOMER_HOLD"

pattern CustomerQueue :: ContactFlowType
pattern CustomerQueue = ContactFlowType' "CUSTOMER_QUEUE"

pattern CustomerWhisper :: ContactFlowType
pattern CustomerWhisper = ContactFlowType' "CUSTOMER_WHISPER"

pattern OutboundWhisper :: ContactFlowType
pattern OutboundWhisper = ContactFlowType' "OUTBOUND_WHISPER"

pattern QueueTransfer :: ContactFlowType
pattern QueueTransfer = ContactFlowType' "QUEUE_TRANSFER"

{-# COMPLETE
  AgentHold,
  AgentTransfer,
  AgentWhisper,
  ContactFlow,
  CustomerHold,
  CustomerQueue,
  CustomerWhisper,
  OutboundWhisper,
  QueueTransfer,
  ContactFlowType' #-}

instance FromText ContactFlowType where
    parser = (ContactFlowType' . mk) <$> takeText

instance ToText ContactFlowType where
    toText (ContactFlowType' ci) = original ci

-- | Represents an enum of /known/ $ContactFlowType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContactFlowType where
    toEnum i = case i of
        0 -> AgentHold
        1 -> AgentTransfer
        2 -> AgentWhisper
        3 -> ContactFlow
        4 -> CustomerHold
        5 -> CustomerQueue
        6 -> CustomerWhisper
        7 -> OutboundWhisper
        8 -> QueueTransfer
        _ -> (error . showText) $ "Unknown index for ContactFlowType: " <> toText i
    fromEnum x = case x of
        AgentHold -> 0
        AgentTransfer -> 1
        AgentWhisper -> 2
        ContactFlow -> 3
        CustomerHold -> 4
        CustomerQueue -> 5
        CustomerWhisper -> 6
        OutboundWhisper -> 7
        QueueTransfer -> 8
        ContactFlowType' name -> (error . showText) $ "Unknown ContactFlowType: " <> original name

-- | Represents the bounds of /known/ $ContactFlowType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContactFlowType where
    minBound = AgentHold
    maxBound = QueueTransfer

instance Hashable     ContactFlowType
instance NFData       ContactFlowType
instance ToByteString ContactFlowType
instance ToQuery      ContactFlowType
instance ToHeader     ContactFlowType

instance ToJSON ContactFlowType where
    toJSON = toJSONText

instance FromJSON ContactFlowType where
    parseJSON = parseJSONText "ContactFlowType"
