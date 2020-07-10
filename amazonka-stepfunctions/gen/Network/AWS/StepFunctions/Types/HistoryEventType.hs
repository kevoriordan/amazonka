{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.HistoryEventType (
  HistoryEventType (
    ..
    , ActivityFailed
    , ActivityScheduleFailed
    , ActivityScheduled
    , ActivityStarted
    , ActivitySucceeded
    , ActivityTimedOut
    , ChoiceStateEntered
    , ChoiceStateExited
    , ExecutionAborted
    , ExecutionFailed
    , ExecutionStarted
    , ExecutionSucceeded
    , ExecutionTimedOut
    , FailStateEntered
    , LambdaFunctionFailed
    , LambdaFunctionScheduleFailed
    , LambdaFunctionScheduled
    , LambdaFunctionStartFailed
    , LambdaFunctionStarted
    , LambdaFunctionSucceeded
    , LambdaFunctionTimedOut
    , MapIterationAborted
    , MapIterationFailed
    , MapIterationStarted
    , MapIterationSucceeded
    , MapStateAborted
    , MapStateEntered
    , MapStateExited
    , MapStateFailed
    , MapStateStarted
    , MapStateSucceeded
    , ParallelStateAborted
    , ParallelStateEntered
    , ParallelStateExited
    , ParallelStateFailed
    , ParallelStateStarted
    , ParallelStateSucceeded
    , PassStateEntered
    , PassStateExited
    , SucceedStateEntered
    , SucceedStateExited
    , TaskFailed
    , TaskScheduled
    , TaskStartFailed
    , TaskStarted
    , TaskStateAborted
    , TaskStateEntered
    , TaskStateExited
    , TaskSubmitFailed
    , TaskSubmitted
    , TaskSucceeded
    , TaskTimedOut
    , WaitStateAborted
    , WaitStateEntered
    , WaitStateExited
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HistoryEventType = HistoryEventType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ActivityFailed :: HistoryEventType
pattern ActivityFailed = HistoryEventType' "ActivityFailed"

pattern ActivityScheduleFailed :: HistoryEventType
pattern ActivityScheduleFailed = HistoryEventType' "ActivityScheduleFailed"

pattern ActivityScheduled :: HistoryEventType
pattern ActivityScheduled = HistoryEventType' "ActivityScheduled"

pattern ActivityStarted :: HistoryEventType
pattern ActivityStarted = HistoryEventType' "ActivityStarted"

pattern ActivitySucceeded :: HistoryEventType
pattern ActivitySucceeded = HistoryEventType' "ActivitySucceeded"

pattern ActivityTimedOut :: HistoryEventType
pattern ActivityTimedOut = HistoryEventType' "ActivityTimedOut"

pattern ChoiceStateEntered :: HistoryEventType
pattern ChoiceStateEntered = HistoryEventType' "ChoiceStateEntered"

pattern ChoiceStateExited :: HistoryEventType
pattern ChoiceStateExited = HistoryEventType' "ChoiceStateExited"

pattern ExecutionAborted :: HistoryEventType
pattern ExecutionAborted = HistoryEventType' "ExecutionAborted"

pattern ExecutionFailed :: HistoryEventType
pattern ExecutionFailed = HistoryEventType' "ExecutionFailed"

pattern ExecutionStarted :: HistoryEventType
pattern ExecutionStarted = HistoryEventType' "ExecutionStarted"

pattern ExecutionSucceeded :: HistoryEventType
pattern ExecutionSucceeded = HistoryEventType' "ExecutionSucceeded"

pattern ExecutionTimedOut :: HistoryEventType
pattern ExecutionTimedOut = HistoryEventType' "ExecutionTimedOut"

pattern FailStateEntered :: HistoryEventType
pattern FailStateEntered = HistoryEventType' "FailStateEntered"

pattern LambdaFunctionFailed :: HistoryEventType
pattern LambdaFunctionFailed = HistoryEventType' "LambdaFunctionFailed"

pattern LambdaFunctionScheduleFailed :: HistoryEventType
pattern LambdaFunctionScheduleFailed = HistoryEventType' "LambdaFunctionScheduleFailed"

pattern LambdaFunctionScheduled :: HistoryEventType
pattern LambdaFunctionScheduled = HistoryEventType' "LambdaFunctionScheduled"

pattern LambdaFunctionStartFailed :: HistoryEventType
pattern LambdaFunctionStartFailed = HistoryEventType' "LambdaFunctionStartFailed"

pattern LambdaFunctionStarted :: HistoryEventType
pattern LambdaFunctionStarted = HistoryEventType' "LambdaFunctionStarted"

pattern LambdaFunctionSucceeded :: HistoryEventType
pattern LambdaFunctionSucceeded = HistoryEventType' "LambdaFunctionSucceeded"

pattern LambdaFunctionTimedOut :: HistoryEventType
pattern LambdaFunctionTimedOut = HistoryEventType' "LambdaFunctionTimedOut"

pattern MapIterationAborted :: HistoryEventType
pattern MapIterationAborted = HistoryEventType' "MapIterationAborted"

pattern MapIterationFailed :: HistoryEventType
pattern MapIterationFailed = HistoryEventType' "MapIterationFailed"

pattern MapIterationStarted :: HistoryEventType
pattern MapIterationStarted = HistoryEventType' "MapIterationStarted"

pattern MapIterationSucceeded :: HistoryEventType
pattern MapIterationSucceeded = HistoryEventType' "MapIterationSucceeded"

pattern MapStateAborted :: HistoryEventType
pattern MapStateAborted = HistoryEventType' "MapStateAborted"

pattern MapStateEntered :: HistoryEventType
pattern MapStateEntered = HistoryEventType' "MapStateEntered"

pattern MapStateExited :: HistoryEventType
pattern MapStateExited = HistoryEventType' "MapStateExited"

pattern MapStateFailed :: HistoryEventType
pattern MapStateFailed = HistoryEventType' "MapStateFailed"

pattern MapStateStarted :: HistoryEventType
pattern MapStateStarted = HistoryEventType' "MapStateStarted"

pattern MapStateSucceeded :: HistoryEventType
pattern MapStateSucceeded = HistoryEventType' "MapStateSucceeded"

pattern ParallelStateAborted :: HistoryEventType
pattern ParallelStateAborted = HistoryEventType' "ParallelStateAborted"

pattern ParallelStateEntered :: HistoryEventType
pattern ParallelStateEntered = HistoryEventType' "ParallelStateEntered"

pattern ParallelStateExited :: HistoryEventType
pattern ParallelStateExited = HistoryEventType' "ParallelStateExited"

pattern ParallelStateFailed :: HistoryEventType
pattern ParallelStateFailed = HistoryEventType' "ParallelStateFailed"

pattern ParallelStateStarted :: HistoryEventType
pattern ParallelStateStarted = HistoryEventType' "ParallelStateStarted"

pattern ParallelStateSucceeded :: HistoryEventType
pattern ParallelStateSucceeded = HistoryEventType' "ParallelStateSucceeded"

pattern PassStateEntered :: HistoryEventType
pattern PassStateEntered = HistoryEventType' "PassStateEntered"

pattern PassStateExited :: HistoryEventType
pattern PassStateExited = HistoryEventType' "PassStateExited"

pattern SucceedStateEntered :: HistoryEventType
pattern SucceedStateEntered = HistoryEventType' "SucceedStateEntered"

pattern SucceedStateExited :: HistoryEventType
pattern SucceedStateExited = HistoryEventType' "SucceedStateExited"

pattern TaskFailed :: HistoryEventType
pattern TaskFailed = HistoryEventType' "TaskFailed"

pattern TaskScheduled :: HistoryEventType
pattern TaskScheduled = HistoryEventType' "TaskScheduled"

pattern TaskStartFailed :: HistoryEventType
pattern TaskStartFailed = HistoryEventType' "TaskStartFailed"

pattern TaskStarted :: HistoryEventType
pattern TaskStarted = HistoryEventType' "TaskStarted"

pattern TaskStateAborted :: HistoryEventType
pattern TaskStateAborted = HistoryEventType' "TaskStateAborted"

pattern TaskStateEntered :: HistoryEventType
pattern TaskStateEntered = HistoryEventType' "TaskStateEntered"

pattern TaskStateExited :: HistoryEventType
pattern TaskStateExited = HistoryEventType' "TaskStateExited"

pattern TaskSubmitFailed :: HistoryEventType
pattern TaskSubmitFailed = HistoryEventType' "TaskSubmitFailed"

pattern TaskSubmitted :: HistoryEventType
pattern TaskSubmitted = HistoryEventType' "TaskSubmitted"

pattern TaskSucceeded :: HistoryEventType
pattern TaskSucceeded = HistoryEventType' "TaskSucceeded"

pattern TaskTimedOut :: HistoryEventType
pattern TaskTimedOut = HistoryEventType' "TaskTimedOut"

pattern WaitStateAborted :: HistoryEventType
pattern WaitStateAborted = HistoryEventType' "WaitStateAborted"

pattern WaitStateEntered :: HistoryEventType
pattern WaitStateEntered = HistoryEventType' "WaitStateEntered"

pattern WaitStateExited :: HistoryEventType
pattern WaitStateExited = HistoryEventType' "WaitStateExited"

{-# COMPLETE
  ActivityFailed,
  ActivityScheduleFailed,
  ActivityScheduled,
  ActivityStarted,
  ActivitySucceeded,
  ActivityTimedOut,
  ChoiceStateEntered,
  ChoiceStateExited,
  ExecutionAborted,
  ExecutionFailed,
  ExecutionStarted,
  ExecutionSucceeded,
  ExecutionTimedOut,
  FailStateEntered,
  LambdaFunctionFailed,
  LambdaFunctionScheduleFailed,
  LambdaFunctionScheduled,
  LambdaFunctionStartFailed,
  LambdaFunctionStarted,
  LambdaFunctionSucceeded,
  LambdaFunctionTimedOut,
  MapIterationAborted,
  MapIterationFailed,
  MapIterationStarted,
  MapIterationSucceeded,
  MapStateAborted,
  MapStateEntered,
  MapStateExited,
  MapStateFailed,
  MapStateStarted,
  MapStateSucceeded,
  ParallelStateAborted,
  ParallelStateEntered,
  ParallelStateExited,
  ParallelStateFailed,
  ParallelStateStarted,
  ParallelStateSucceeded,
  PassStateEntered,
  PassStateExited,
  SucceedStateEntered,
  SucceedStateExited,
  TaskFailed,
  TaskScheduled,
  TaskStartFailed,
  TaskStarted,
  TaskStateAborted,
  TaskStateEntered,
  TaskStateExited,
  TaskSubmitFailed,
  TaskSubmitted,
  TaskSucceeded,
  TaskTimedOut,
  WaitStateAborted,
  WaitStateEntered,
  WaitStateExited,
  HistoryEventType' #-}

instance FromText HistoryEventType where
    parser = (HistoryEventType' . mk) <$> takeText

instance ToText HistoryEventType where
    toText (HistoryEventType' ci) = original ci

-- | Represents an enum of /known/ $HistoryEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HistoryEventType where
    toEnum i = case i of
        0 -> ActivityFailed
        1 -> ActivityScheduleFailed
        2 -> ActivityScheduled
        3 -> ActivityStarted
        4 -> ActivitySucceeded
        5 -> ActivityTimedOut
        6 -> ChoiceStateEntered
        7 -> ChoiceStateExited
        8 -> ExecutionAborted
        9 -> ExecutionFailed
        10 -> ExecutionStarted
        11 -> ExecutionSucceeded
        12 -> ExecutionTimedOut
        13 -> FailStateEntered
        14 -> LambdaFunctionFailed
        15 -> LambdaFunctionScheduleFailed
        16 -> LambdaFunctionScheduled
        17 -> LambdaFunctionStartFailed
        18 -> LambdaFunctionStarted
        19 -> LambdaFunctionSucceeded
        20 -> LambdaFunctionTimedOut
        21 -> MapIterationAborted
        22 -> MapIterationFailed
        23 -> MapIterationStarted
        24 -> MapIterationSucceeded
        25 -> MapStateAborted
        26 -> MapStateEntered
        27 -> MapStateExited
        28 -> MapStateFailed
        29 -> MapStateStarted
        30 -> MapStateSucceeded
        31 -> ParallelStateAborted
        32 -> ParallelStateEntered
        33 -> ParallelStateExited
        34 -> ParallelStateFailed
        35 -> ParallelStateStarted
        36 -> ParallelStateSucceeded
        37 -> PassStateEntered
        38 -> PassStateExited
        39 -> SucceedStateEntered
        40 -> SucceedStateExited
        41 -> TaskFailed
        42 -> TaskScheduled
        43 -> TaskStartFailed
        44 -> TaskStarted
        45 -> TaskStateAborted
        46 -> TaskStateEntered
        47 -> TaskStateExited
        48 -> TaskSubmitFailed
        49 -> TaskSubmitted
        50 -> TaskSucceeded
        51 -> TaskTimedOut
        52 -> WaitStateAborted
        53 -> WaitStateEntered
        54 -> WaitStateExited
        _ -> (error . showText) $ "Unknown index for HistoryEventType: " <> toText i
    fromEnum x = case x of
        ActivityFailed -> 0
        ActivityScheduleFailed -> 1
        ActivityScheduled -> 2
        ActivityStarted -> 3
        ActivitySucceeded -> 4
        ActivityTimedOut -> 5
        ChoiceStateEntered -> 6
        ChoiceStateExited -> 7
        ExecutionAborted -> 8
        ExecutionFailed -> 9
        ExecutionStarted -> 10
        ExecutionSucceeded -> 11
        ExecutionTimedOut -> 12
        FailStateEntered -> 13
        LambdaFunctionFailed -> 14
        LambdaFunctionScheduleFailed -> 15
        LambdaFunctionScheduled -> 16
        LambdaFunctionStartFailed -> 17
        LambdaFunctionStarted -> 18
        LambdaFunctionSucceeded -> 19
        LambdaFunctionTimedOut -> 20
        MapIterationAborted -> 21
        MapIterationFailed -> 22
        MapIterationStarted -> 23
        MapIterationSucceeded -> 24
        MapStateAborted -> 25
        MapStateEntered -> 26
        MapStateExited -> 27
        MapStateFailed -> 28
        MapStateStarted -> 29
        MapStateSucceeded -> 30
        ParallelStateAborted -> 31
        ParallelStateEntered -> 32
        ParallelStateExited -> 33
        ParallelStateFailed -> 34
        ParallelStateStarted -> 35
        ParallelStateSucceeded -> 36
        PassStateEntered -> 37
        PassStateExited -> 38
        SucceedStateEntered -> 39
        SucceedStateExited -> 40
        TaskFailed -> 41
        TaskScheduled -> 42
        TaskStartFailed -> 43
        TaskStarted -> 44
        TaskStateAborted -> 45
        TaskStateEntered -> 46
        TaskStateExited -> 47
        TaskSubmitFailed -> 48
        TaskSubmitted -> 49
        TaskSucceeded -> 50
        TaskTimedOut -> 51
        WaitStateAborted -> 52
        WaitStateEntered -> 53
        WaitStateExited -> 54
        HistoryEventType' name -> (error . showText) $ "Unknown HistoryEventType: " <> original name

-- | Represents the bounds of /known/ $HistoryEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HistoryEventType where
    minBound = ActivityFailed
    maxBound = WaitStateExited

instance Hashable     HistoryEventType
instance NFData       HistoryEventType
instance ToByteString HistoryEventType
instance ToQuery      HistoryEventType
instance ToHeader     HistoryEventType

instance FromJSON HistoryEventType where
    parseJSON = parseJSONText "HistoryEventType"
