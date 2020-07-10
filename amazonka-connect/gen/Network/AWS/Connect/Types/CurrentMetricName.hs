{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.CurrentMetricName (
  CurrentMetricName (
    ..
    , AgentsAfterContactWork
    , AgentsAvailable
    , AgentsError
    , AgentsNonProductive
    , AgentsOnCall
    , AgentsOnContact
    , AgentsOnline
    , AgentsStaffed
    , ContactsInQueue
    , ContactsScheduled
    , OldestContactAge
    , SlotsActive
    , SlotsAvailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The current metric names.
--
--
data CurrentMetricName = CurrentMetricName' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AgentsAfterContactWork :: CurrentMetricName
pattern AgentsAfterContactWork = CurrentMetricName' "AGENTS_AFTER_CONTACT_WORK"

pattern AgentsAvailable :: CurrentMetricName
pattern AgentsAvailable = CurrentMetricName' "AGENTS_AVAILABLE"

pattern AgentsError :: CurrentMetricName
pattern AgentsError = CurrentMetricName' "AGENTS_ERROR"

pattern AgentsNonProductive :: CurrentMetricName
pattern AgentsNonProductive = CurrentMetricName' "AGENTS_NON_PRODUCTIVE"

pattern AgentsOnCall :: CurrentMetricName
pattern AgentsOnCall = CurrentMetricName' "AGENTS_ON_CALL"

pattern AgentsOnContact :: CurrentMetricName
pattern AgentsOnContact = CurrentMetricName' "AGENTS_ON_CONTACT"

pattern AgentsOnline :: CurrentMetricName
pattern AgentsOnline = CurrentMetricName' "AGENTS_ONLINE"

pattern AgentsStaffed :: CurrentMetricName
pattern AgentsStaffed = CurrentMetricName' "AGENTS_STAFFED"

pattern ContactsInQueue :: CurrentMetricName
pattern ContactsInQueue = CurrentMetricName' "CONTACTS_IN_QUEUE"

pattern ContactsScheduled :: CurrentMetricName
pattern ContactsScheduled = CurrentMetricName' "CONTACTS_SCHEDULED"

pattern OldestContactAge :: CurrentMetricName
pattern OldestContactAge = CurrentMetricName' "OLDEST_CONTACT_AGE"

pattern SlotsActive :: CurrentMetricName
pattern SlotsActive = CurrentMetricName' "SLOTS_ACTIVE"

pattern SlotsAvailable :: CurrentMetricName
pattern SlotsAvailable = CurrentMetricName' "SLOTS_AVAILABLE"

{-# COMPLETE
  AgentsAfterContactWork,
  AgentsAvailable,
  AgentsError,
  AgentsNonProductive,
  AgentsOnCall,
  AgentsOnContact,
  AgentsOnline,
  AgentsStaffed,
  ContactsInQueue,
  ContactsScheduled,
  OldestContactAge,
  SlotsActive,
  SlotsAvailable,
  CurrentMetricName' #-}

instance FromText CurrentMetricName where
    parser = (CurrentMetricName' . mk) <$> takeText

instance ToText CurrentMetricName where
    toText (CurrentMetricName' ci) = original ci

-- | Represents an enum of /known/ $CurrentMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CurrentMetricName where
    toEnum i = case i of
        0 -> AgentsAfterContactWork
        1 -> AgentsAvailable
        2 -> AgentsError
        3 -> AgentsNonProductive
        4 -> AgentsOnCall
        5 -> AgentsOnContact
        6 -> AgentsOnline
        7 -> AgentsStaffed
        8 -> ContactsInQueue
        9 -> ContactsScheduled
        10 -> OldestContactAge
        11 -> SlotsActive
        12 -> SlotsAvailable
        _ -> (error . showText) $ "Unknown index for CurrentMetricName: " <> toText i
    fromEnum x = case x of
        AgentsAfterContactWork -> 0
        AgentsAvailable -> 1
        AgentsError -> 2
        AgentsNonProductive -> 3
        AgentsOnCall -> 4
        AgentsOnContact -> 5
        AgentsOnline -> 6
        AgentsStaffed -> 7
        ContactsInQueue -> 8
        ContactsScheduled -> 9
        OldestContactAge -> 10
        SlotsActive -> 11
        SlotsAvailable -> 12
        CurrentMetricName' name -> (error . showText) $ "Unknown CurrentMetricName: " <> original name

-- | Represents the bounds of /known/ $CurrentMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CurrentMetricName where
    minBound = AgentsAfterContactWork
    maxBound = SlotsAvailable

instance Hashable     CurrentMetricName
instance NFData       CurrentMetricName
instance ToByteString CurrentMetricName
instance ToQuery      CurrentMetricName
instance ToHeader     CurrentMetricName

instance ToJSON CurrentMetricName where
    toJSON = toJSONText

instance FromJSON CurrentMetricName where
    parseJSON = parseJSONText "CurrentMetricName"
