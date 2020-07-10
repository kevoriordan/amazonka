{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LifecycleState (
  LifecycleState (
    ..
    , Detached
    , Detaching
    , EnteringStandby
    , InService
    , Pending
    , PendingProceed
    , PendingWait
    , Quarantined
    , Standby
    , Terminated
    , Terminating
    , TerminatingProceed
    , TerminatingWait
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LifecycleState = LifecycleState' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Detached :: LifecycleState
pattern Detached = LifecycleState' "Detached"

pattern Detaching :: LifecycleState
pattern Detaching = LifecycleState' "Detaching"

pattern EnteringStandby :: LifecycleState
pattern EnteringStandby = LifecycleState' "EnteringStandby"

pattern InService :: LifecycleState
pattern InService = LifecycleState' "InService"

pattern Pending :: LifecycleState
pattern Pending = LifecycleState' "Pending"

pattern PendingProceed :: LifecycleState
pattern PendingProceed = LifecycleState' "Pending:Proceed"

pattern PendingWait :: LifecycleState
pattern PendingWait = LifecycleState' "Pending:Wait"

pattern Quarantined :: LifecycleState
pattern Quarantined = LifecycleState' "Quarantined"

pattern Standby :: LifecycleState
pattern Standby = LifecycleState' "Standby"

pattern Terminated :: LifecycleState
pattern Terminated = LifecycleState' "Terminated"

pattern Terminating :: LifecycleState
pattern Terminating = LifecycleState' "Terminating"

pattern TerminatingProceed :: LifecycleState
pattern TerminatingProceed = LifecycleState' "Terminating:Proceed"

pattern TerminatingWait :: LifecycleState
pattern TerminatingWait = LifecycleState' "Terminating:Wait"

{-# COMPLETE
  Detached,
  Detaching,
  EnteringStandby,
  InService,
  Pending,
  PendingProceed,
  PendingWait,
  Quarantined,
  Standby,
  Terminated,
  Terminating,
  TerminatingProceed,
  TerminatingWait,
  LifecycleState' #-}

instance FromText LifecycleState where
    parser = (LifecycleState' . mk) <$> takeText

instance ToText LifecycleState where
    toText (LifecycleState' ci) = original ci

-- | Represents an enum of /known/ $LifecycleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LifecycleState where
    toEnum i = case i of
        0 -> Detached
        1 -> Detaching
        2 -> EnteringStandby
        3 -> InService
        4 -> Pending
        5 -> PendingProceed
        6 -> PendingWait
        7 -> Quarantined
        8 -> Standby
        9 -> Terminated
        10 -> Terminating
        11 -> TerminatingProceed
        12 -> TerminatingWait
        _ -> (error . showText) $ "Unknown index for LifecycleState: " <> toText i
    fromEnum x = case x of
        Detached -> 0
        Detaching -> 1
        EnteringStandby -> 2
        InService -> 3
        Pending -> 4
        PendingProceed -> 5
        PendingWait -> 6
        Quarantined -> 7
        Standby -> 8
        Terminated -> 9
        Terminating -> 10
        TerminatingProceed -> 11
        TerminatingWait -> 12
        LifecycleState' name -> (error . showText) $ "Unknown LifecycleState: " <> original name

-- | Represents the bounds of /known/ $LifecycleState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LifecycleState where
    minBound = Detached
    maxBound = TerminatingWait

instance Hashable     LifecycleState
instance NFData       LifecycleState
instance ToByteString LifecycleState
instance ToQuery      LifecycleState
instance ToHeader     LifecycleState

instance FromXML LifecycleState where
    parseXML = parseXMLText "LifecycleState"
