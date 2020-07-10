{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepState (
  StepState (
    ..
    , SSCancelPending
    , SSCancelled
    , SSCompleted
    , SSFailed
    , SSInterrupted
    , SSPending
    , SSRunning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StepState = StepState' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern SSCancelPending :: StepState
pattern SSCancelPending = StepState' "CANCEL_PENDING"

pattern SSCancelled :: StepState
pattern SSCancelled = StepState' "CANCELLED"

pattern SSCompleted :: StepState
pattern SSCompleted = StepState' "COMPLETED"

pattern SSFailed :: StepState
pattern SSFailed = StepState' "FAILED"

pattern SSInterrupted :: StepState
pattern SSInterrupted = StepState' "INTERRUPTED"

pattern SSPending :: StepState
pattern SSPending = StepState' "PENDING"

pattern SSRunning :: StepState
pattern SSRunning = StepState' "RUNNING"

{-# COMPLETE
  SSCancelPending,
  SSCancelled,
  SSCompleted,
  SSFailed,
  SSInterrupted,
  SSPending,
  SSRunning,
  StepState' #-}

instance FromText StepState where
    parser = (StepState' . mk) <$> takeText

instance ToText StepState where
    toText (StepState' ci) = original ci

-- | Represents an enum of /known/ $StepState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StepState where
    toEnum i = case i of
        0 -> SSCancelPending
        1 -> SSCancelled
        2 -> SSCompleted
        3 -> SSFailed
        4 -> SSInterrupted
        5 -> SSPending
        6 -> SSRunning
        _ -> (error . showText) $ "Unknown index for StepState: " <> toText i
    fromEnum x = case x of
        SSCancelPending -> 0
        SSCancelled -> 1
        SSCompleted -> 2
        SSFailed -> 3
        SSInterrupted -> 4
        SSPending -> 5
        SSRunning -> 6
        StepState' name -> (error . showText) $ "Unknown StepState: " <> original name

-- | Represents the bounds of /known/ $StepState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StepState where
    minBound = SSCancelPending
    maxBound = SSRunning

instance Hashable     StepState
instance NFData       StepState
instance ToByteString StepState
instance ToQuery      StepState
instance ToHeader     StepState

instance ToJSON StepState where
    toJSON = toJSONText

instance FromJSON StepState where
    parseJSON = parseJSONText "StepState"
