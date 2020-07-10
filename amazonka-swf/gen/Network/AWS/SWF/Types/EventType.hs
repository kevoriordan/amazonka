{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.EventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.EventType (
  EventType (
    ..
    , ActivityTaskCancelRequested
    , ActivityTaskCanceled
    , ActivityTaskCompleted
    , ActivityTaskFailed
    , ActivityTaskScheduled
    , ActivityTaskStarted
    , ActivityTaskTimedOut
    , CancelTimerFailed
    , CancelWorkflowExecutionFailed
    , ChildWorkflowExecutionCanceled
    , ChildWorkflowExecutionCompleted
    , ChildWorkflowExecutionFailed
    , ChildWorkflowExecutionStarted
    , ChildWorkflowExecutionTerminated
    , ChildWorkflowExecutionTimedOut
    , CompleteWorkflowExecutionFailed
    , ContinueAsNewWorkflowExecutionFailed
    , DecisionTaskCompleted
    , DecisionTaskScheduled
    , DecisionTaskStarted
    , DecisionTaskTimedOut
    , ExternalWorkflowExecutionCancelRequested
    , ExternalWorkflowExecutionSignaled
    , FailWorkflowExecutionFailed
    , LambdaFunctionCompleted
    , LambdaFunctionFailed
    , LambdaFunctionScheduled
    , LambdaFunctionStarted
    , LambdaFunctionTimedOut
    , MarkerRecorded
    , RecordMarkerFailed
    , RequestCancelActivityTaskFailed
    , RequestCancelExternalWorkflowExecutionFailed
    , RequestCancelExternalWorkflowExecutionInitiated
    , ScheduleActivityTaskFailed
    , ScheduleLambdaFunctionFailed
    , SignalExternalWorkflowExecutionFailed
    , SignalExternalWorkflowExecutionInitiated
    , StartChildWorkflowExecutionFailed
    , StartChildWorkflowExecutionInitiated
    , StartLambdaFunctionFailed
    , StartTimerFailed
    , TimerCanceled
    , TimerFired
    , TimerStarted
    , WorkflowExecutionCancelRequested
    , WorkflowExecutionCanceled
    , WorkflowExecutionCompleted
    , WorkflowExecutionContinuedAsNew
    , WorkflowExecutionFailed
    , WorkflowExecutionSignaled
    , WorkflowExecutionStarted
    , WorkflowExecutionTerminated
    , WorkflowExecutionTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EventType = EventType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern ActivityTaskCancelRequested :: EventType
pattern ActivityTaskCancelRequested = EventType' "ActivityTaskCancelRequested"

pattern ActivityTaskCanceled :: EventType
pattern ActivityTaskCanceled = EventType' "ActivityTaskCanceled"

pattern ActivityTaskCompleted :: EventType
pattern ActivityTaskCompleted = EventType' "ActivityTaskCompleted"

pattern ActivityTaskFailed :: EventType
pattern ActivityTaskFailed = EventType' "ActivityTaskFailed"

pattern ActivityTaskScheduled :: EventType
pattern ActivityTaskScheduled = EventType' "ActivityTaskScheduled"

pattern ActivityTaskStarted :: EventType
pattern ActivityTaskStarted = EventType' "ActivityTaskStarted"

pattern ActivityTaskTimedOut :: EventType
pattern ActivityTaskTimedOut = EventType' "ActivityTaskTimedOut"

pattern CancelTimerFailed :: EventType
pattern CancelTimerFailed = EventType' "CancelTimerFailed"

pattern CancelWorkflowExecutionFailed :: EventType
pattern CancelWorkflowExecutionFailed = EventType' "CancelWorkflowExecutionFailed"

pattern ChildWorkflowExecutionCanceled :: EventType
pattern ChildWorkflowExecutionCanceled = EventType' "ChildWorkflowExecutionCanceled"

pattern ChildWorkflowExecutionCompleted :: EventType
pattern ChildWorkflowExecutionCompleted = EventType' "ChildWorkflowExecutionCompleted"

pattern ChildWorkflowExecutionFailed :: EventType
pattern ChildWorkflowExecutionFailed = EventType' "ChildWorkflowExecutionFailed"

pattern ChildWorkflowExecutionStarted :: EventType
pattern ChildWorkflowExecutionStarted = EventType' "ChildWorkflowExecutionStarted"

pattern ChildWorkflowExecutionTerminated :: EventType
pattern ChildWorkflowExecutionTerminated = EventType' "ChildWorkflowExecutionTerminated"

pattern ChildWorkflowExecutionTimedOut :: EventType
pattern ChildWorkflowExecutionTimedOut = EventType' "ChildWorkflowExecutionTimedOut"

pattern CompleteWorkflowExecutionFailed :: EventType
pattern CompleteWorkflowExecutionFailed = EventType' "CompleteWorkflowExecutionFailed"

pattern ContinueAsNewWorkflowExecutionFailed :: EventType
pattern ContinueAsNewWorkflowExecutionFailed = EventType' "ContinueAsNewWorkflowExecutionFailed"

pattern DecisionTaskCompleted :: EventType
pattern DecisionTaskCompleted = EventType' "DecisionTaskCompleted"

pattern DecisionTaskScheduled :: EventType
pattern DecisionTaskScheduled = EventType' "DecisionTaskScheduled"

pattern DecisionTaskStarted :: EventType
pattern DecisionTaskStarted = EventType' "DecisionTaskStarted"

pattern DecisionTaskTimedOut :: EventType
pattern DecisionTaskTimedOut = EventType' "DecisionTaskTimedOut"

pattern ExternalWorkflowExecutionCancelRequested :: EventType
pattern ExternalWorkflowExecutionCancelRequested = EventType' "ExternalWorkflowExecutionCancelRequested"

pattern ExternalWorkflowExecutionSignaled :: EventType
pattern ExternalWorkflowExecutionSignaled = EventType' "ExternalWorkflowExecutionSignaled"

pattern FailWorkflowExecutionFailed :: EventType
pattern FailWorkflowExecutionFailed = EventType' "FailWorkflowExecutionFailed"

pattern LambdaFunctionCompleted :: EventType
pattern LambdaFunctionCompleted = EventType' "LambdaFunctionCompleted"

pattern LambdaFunctionFailed :: EventType
pattern LambdaFunctionFailed = EventType' "LambdaFunctionFailed"

pattern LambdaFunctionScheduled :: EventType
pattern LambdaFunctionScheduled = EventType' "LambdaFunctionScheduled"

pattern LambdaFunctionStarted :: EventType
pattern LambdaFunctionStarted = EventType' "LambdaFunctionStarted"

pattern LambdaFunctionTimedOut :: EventType
pattern LambdaFunctionTimedOut = EventType' "LambdaFunctionTimedOut"

pattern MarkerRecorded :: EventType
pattern MarkerRecorded = EventType' "MarkerRecorded"

pattern RecordMarkerFailed :: EventType
pattern RecordMarkerFailed = EventType' "RecordMarkerFailed"

pattern RequestCancelActivityTaskFailed :: EventType
pattern RequestCancelActivityTaskFailed = EventType' "RequestCancelActivityTaskFailed"

pattern RequestCancelExternalWorkflowExecutionFailed :: EventType
pattern RequestCancelExternalWorkflowExecutionFailed = EventType' "RequestCancelExternalWorkflowExecutionFailed"

pattern RequestCancelExternalWorkflowExecutionInitiated :: EventType
pattern RequestCancelExternalWorkflowExecutionInitiated = EventType' "RequestCancelExternalWorkflowExecutionInitiated"

pattern ScheduleActivityTaskFailed :: EventType
pattern ScheduleActivityTaskFailed = EventType' "ScheduleActivityTaskFailed"

pattern ScheduleLambdaFunctionFailed :: EventType
pattern ScheduleLambdaFunctionFailed = EventType' "ScheduleLambdaFunctionFailed"

pattern SignalExternalWorkflowExecutionFailed :: EventType
pattern SignalExternalWorkflowExecutionFailed = EventType' "SignalExternalWorkflowExecutionFailed"

pattern SignalExternalWorkflowExecutionInitiated :: EventType
pattern SignalExternalWorkflowExecutionInitiated = EventType' "SignalExternalWorkflowExecutionInitiated"

pattern StartChildWorkflowExecutionFailed :: EventType
pattern StartChildWorkflowExecutionFailed = EventType' "StartChildWorkflowExecutionFailed"

pattern StartChildWorkflowExecutionInitiated :: EventType
pattern StartChildWorkflowExecutionInitiated = EventType' "StartChildWorkflowExecutionInitiated"

pattern StartLambdaFunctionFailed :: EventType
pattern StartLambdaFunctionFailed = EventType' "StartLambdaFunctionFailed"

pattern StartTimerFailed :: EventType
pattern StartTimerFailed = EventType' "StartTimerFailed"

pattern TimerCanceled :: EventType
pattern TimerCanceled = EventType' "TimerCanceled"

pattern TimerFired :: EventType
pattern TimerFired = EventType' "TimerFired"

pattern TimerStarted :: EventType
pattern TimerStarted = EventType' "TimerStarted"

pattern WorkflowExecutionCancelRequested :: EventType
pattern WorkflowExecutionCancelRequested = EventType' "WorkflowExecutionCancelRequested"

pattern WorkflowExecutionCanceled :: EventType
pattern WorkflowExecutionCanceled = EventType' "WorkflowExecutionCanceled"

pattern WorkflowExecutionCompleted :: EventType
pattern WorkflowExecutionCompleted = EventType' "WorkflowExecutionCompleted"

pattern WorkflowExecutionContinuedAsNew :: EventType
pattern WorkflowExecutionContinuedAsNew = EventType' "WorkflowExecutionContinuedAsNew"

pattern WorkflowExecutionFailed :: EventType
pattern WorkflowExecutionFailed = EventType' "WorkflowExecutionFailed"

pattern WorkflowExecutionSignaled :: EventType
pattern WorkflowExecutionSignaled = EventType' "WorkflowExecutionSignaled"

pattern WorkflowExecutionStarted :: EventType
pattern WorkflowExecutionStarted = EventType' "WorkflowExecutionStarted"

pattern WorkflowExecutionTerminated :: EventType
pattern WorkflowExecutionTerminated = EventType' "WorkflowExecutionTerminated"

pattern WorkflowExecutionTimedOut :: EventType
pattern WorkflowExecutionTimedOut = EventType' "WorkflowExecutionTimedOut"

{-# COMPLETE
  ActivityTaskCancelRequested,
  ActivityTaskCanceled,
  ActivityTaskCompleted,
  ActivityTaskFailed,
  ActivityTaskScheduled,
  ActivityTaskStarted,
  ActivityTaskTimedOut,
  CancelTimerFailed,
  CancelWorkflowExecutionFailed,
  ChildWorkflowExecutionCanceled,
  ChildWorkflowExecutionCompleted,
  ChildWorkflowExecutionFailed,
  ChildWorkflowExecutionStarted,
  ChildWorkflowExecutionTerminated,
  ChildWorkflowExecutionTimedOut,
  CompleteWorkflowExecutionFailed,
  ContinueAsNewWorkflowExecutionFailed,
  DecisionTaskCompleted,
  DecisionTaskScheduled,
  DecisionTaskStarted,
  DecisionTaskTimedOut,
  ExternalWorkflowExecutionCancelRequested,
  ExternalWorkflowExecutionSignaled,
  FailWorkflowExecutionFailed,
  LambdaFunctionCompleted,
  LambdaFunctionFailed,
  LambdaFunctionScheduled,
  LambdaFunctionStarted,
  LambdaFunctionTimedOut,
  MarkerRecorded,
  RecordMarkerFailed,
  RequestCancelActivityTaskFailed,
  RequestCancelExternalWorkflowExecutionFailed,
  RequestCancelExternalWorkflowExecutionInitiated,
  ScheduleActivityTaskFailed,
  ScheduleLambdaFunctionFailed,
  SignalExternalWorkflowExecutionFailed,
  SignalExternalWorkflowExecutionInitiated,
  StartChildWorkflowExecutionFailed,
  StartChildWorkflowExecutionInitiated,
  StartLambdaFunctionFailed,
  StartTimerFailed,
  TimerCanceled,
  TimerFired,
  TimerStarted,
  WorkflowExecutionCancelRequested,
  WorkflowExecutionCanceled,
  WorkflowExecutionCompleted,
  WorkflowExecutionContinuedAsNew,
  WorkflowExecutionFailed,
  WorkflowExecutionSignaled,
  WorkflowExecutionStarted,
  WorkflowExecutionTerminated,
  WorkflowExecutionTimedOut,
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
        0 -> ActivityTaskCancelRequested
        1 -> ActivityTaskCanceled
        2 -> ActivityTaskCompleted
        3 -> ActivityTaskFailed
        4 -> ActivityTaskScheduled
        5 -> ActivityTaskStarted
        6 -> ActivityTaskTimedOut
        7 -> CancelTimerFailed
        8 -> CancelWorkflowExecutionFailed
        9 -> ChildWorkflowExecutionCanceled
        10 -> ChildWorkflowExecutionCompleted
        11 -> ChildWorkflowExecutionFailed
        12 -> ChildWorkflowExecutionStarted
        13 -> ChildWorkflowExecutionTerminated
        14 -> ChildWorkflowExecutionTimedOut
        15 -> CompleteWorkflowExecutionFailed
        16 -> ContinueAsNewWorkflowExecutionFailed
        17 -> DecisionTaskCompleted
        18 -> DecisionTaskScheduled
        19 -> DecisionTaskStarted
        20 -> DecisionTaskTimedOut
        21 -> ExternalWorkflowExecutionCancelRequested
        22 -> ExternalWorkflowExecutionSignaled
        23 -> FailWorkflowExecutionFailed
        24 -> LambdaFunctionCompleted
        25 -> LambdaFunctionFailed
        26 -> LambdaFunctionScheduled
        27 -> LambdaFunctionStarted
        28 -> LambdaFunctionTimedOut
        29 -> MarkerRecorded
        30 -> RecordMarkerFailed
        31 -> RequestCancelActivityTaskFailed
        32 -> RequestCancelExternalWorkflowExecutionFailed
        33 -> RequestCancelExternalWorkflowExecutionInitiated
        34 -> ScheduleActivityTaskFailed
        35 -> ScheduleLambdaFunctionFailed
        36 -> SignalExternalWorkflowExecutionFailed
        37 -> SignalExternalWorkflowExecutionInitiated
        38 -> StartChildWorkflowExecutionFailed
        39 -> StartChildWorkflowExecutionInitiated
        40 -> StartLambdaFunctionFailed
        41 -> StartTimerFailed
        42 -> TimerCanceled
        43 -> TimerFired
        44 -> TimerStarted
        45 -> WorkflowExecutionCancelRequested
        46 -> WorkflowExecutionCanceled
        47 -> WorkflowExecutionCompleted
        48 -> WorkflowExecutionContinuedAsNew
        49 -> WorkflowExecutionFailed
        50 -> WorkflowExecutionSignaled
        51 -> WorkflowExecutionStarted
        52 -> WorkflowExecutionTerminated
        53 -> WorkflowExecutionTimedOut
        _ -> (error . showText) $ "Unknown index for EventType: " <> toText i
    fromEnum x = case x of
        ActivityTaskCancelRequested -> 0
        ActivityTaskCanceled -> 1
        ActivityTaskCompleted -> 2
        ActivityTaskFailed -> 3
        ActivityTaskScheduled -> 4
        ActivityTaskStarted -> 5
        ActivityTaskTimedOut -> 6
        CancelTimerFailed -> 7
        CancelWorkflowExecutionFailed -> 8
        ChildWorkflowExecutionCanceled -> 9
        ChildWorkflowExecutionCompleted -> 10
        ChildWorkflowExecutionFailed -> 11
        ChildWorkflowExecutionStarted -> 12
        ChildWorkflowExecutionTerminated -> 13
        ChildWorkflowExecutionTimedOut -> 14
        CompleteWorkflowExecutionFailed -> 15
        ContinueAsNewWorkflowExecutionFailed -> 16
        DecisionTaskCompleted -> 17
        DecisionTaskScheduled -> 18
        DecisionTaskStarted -> 19
        DecisionTaskTimedOut -> 20
        ExternalWorkflowExecutionCancelRequested -> 21
        ExternalWorkflowExecutionSignaled -> 22
        FailWorkflowExecutionFailed -> 23
        LambdaFunctionCompleted -> 24
        LambdaFunctionFailed -> 25
        LambdaFunctionScheduled -> 26
        LambdaFunctionStarted -> 27
        LambdaFunctionTimedOut -> 28
        MarkerRecorded -> 29
        RecordMarkerFailed -> 30
        RequestCancelActivityTaskFailed -> 31
        RequestCancelExternalWorkflowExecutionFailed -> 32
        RequestCancelExternalWorkflowExecutionInitiated -> 33
        ScheduleActivityTaskFailed -> 34
        ScheduleLambdaFunctionFailed -> 35
        SignalExternalWorkflowExecutionFailed -> 36
        SignalExternalWorkflowExecutionInitiated -> 37
        StartChildWorkflowExecutionFailed -> 38
        StartChildWorkflowExecutionInitiated -> 39
        StartLambdaFunctionFailed -> 40
        StartTimerFailed -> 41
        TimerCanceled -> 42
        TimerFired -> 43
        TimerStarted -> 44
        WorkflowExecutionCancelRequested -> 45
        WorkflowExecutionCanceled -> 46
        WorkflowExecutionCompleted -> 47
        WorkflowExecutionContinuedAsNew -> 48
        WorkflowExecutionFailed -> 49
        WorkflowExecutionSignaled -> 50
        WorkflowExecutionStarted -> 51
        WorkflowExecutionTerminated -> 52
        WorkflowExecutionTimedOut -> 53
        EventType' name -> (error . showText) $ "Unknown EventType: " <> original name

-- | Represents the bounds of /known/ $EventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventType where
    minBound = ActivityTaskCancelRequested
    maxBound = WorkflowExecutionTimedOut

instance Hashable     EventType
instance NFData       EventType
instance ToByteString EventType
instance ToQuery      EventType
instance ToHeader     EventType

instance FromJSON EventType where
    parseJSON = parseJSONText "EventType"
