{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.DecisionType (
  DecisionType (
    ..
    , CancelTimer
    , CancelWorkflowExecution
    , CompleteWorkflowExecution
    , ContinueAsNewWorkflowExecution
    , FailWorkflowExecution
    , RecordMarker
    , RequestCancelActivityTask
    , RequestCancelExternalWorkflowExecution
    , ScheduleActivityTask
    , ScheduleLambdaFunction
    , SignalExternalWorkflowExecution
    , StartChildWorkflowExecution
    , StartTimer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DecisionType = DecisionType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern CancelTimer :: DecisionType
pattern CancelTimer = DecisionType' "CancelTimer"

pattern CancelWorkflowExecution :: DecisionType
pattern CancelWorkflowExecution = DecisionType' "CancelWorkflowExecution"

pattern CompleteWorkflowExecution :: DecisionType
pattern CompleteWorkflowExecution = DecisionType' "CompleteWorkflowExecution"

pattern ContinueAsNewWorkflowExecution :: DecisionType
pattern ContinueAsNewWorkflowExecution = DecisionType' "ContinueAsNewWorkflowExecution"

pattern FailWorkflowExecution :: DecisionType
pattern FailWorkflowExecution = DecisionType' "FailWorkflowExecution"

pattern RecordMarker :: DecisionType
pattern RecordMarker = DecisionType' "RecordMarker"

pattern RequestCancelActivityTask :: DecisionType
pattern RequestCancelActivityTask = DecisionType' "RequestCancelActivityTask"

pattern RequestCancelExternalWorkflowExecution :: DecisionType
pattern RequestCancelExternalWorkflowExecution = DecisionType' "RequestCancelExternalWorkflowExecution"

pattern ScheduleActivityTask :: DecisionType
pattern ScheduleActivityTask = DecisionType' "ScheduleActivityTask"

pattern ScheduleLambdaFunction :: DecisionType
pattern ScheduleLambdaFunction = DecisionType' "ScheduleLambdaFunction"

pattern SignalExternalWorkflowExecution :: DecisionType
pattern SignalExternalWorkflowExecution = DecisionType' "SignalExternalWorkflowExecution"

pattern StartChildWorkflowExecution :: DecisionType
pattern StartChildWorkflowExecution = DecisionType' "StartChildWorkflowExecution"

pattern StartTimer :: DecisionType
pattern StartTimer = DecisionType' "StartTimer"

{-# COMPLETE
  CancelTimer,
  CancelWorkflowExecution,
  CompleteWorkflowExecution,
  ContinueAsNewWorkflowExecution,
  FailWorkflowExecution,
  RecordMarker,
  RequestCancelActivityTask,
  RequestCancelExternalWorkflowExecution,
  ScheduleActivityTask,
  ScheduleLambdaFunction,
  SignalExternalWorkflowExecution,
  StartChildWorkflowExecution,
  StartTimer,
  DecisionType' #-}

instance FromText DecisionType where
    parser = (DecisionType' . mk) <$> takeText

instance ToText DecisionType where
    toText (DecisionType' ci) = original ci

-- | Represents an enum of /known/ $DecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DecisionType where
    toEnum i = case i of
        0 -> CancelTimer
        1 -> CancelWorkflowExecution
        2 -> CompleteWorkflowExecution
        3 -> ContinueAsNewWorkflowExecution
        4 -> FailWorkflowExecution
        5 -> RecordMarker
        6 -> RequestCancelActivityTask
        7 -> RequestCancelExternalWorkflowExecution
        8 -> ScheduleActivityTask
        9 -> ScheduleLambdaFunction
        10 -> SignalExternalWorkflowExecution
        11 -> StartChildWorkflowExecution
        12 -> StartTimer
        _ -> (error . showText) $ "Unknown index for DecisionType: " <> toText i
    fromEnum x = case x of
        CancelTimer -> 0
        CancelWorkflowExecution -> 1
        CompleteWorkflowExecution -> 2
        ContinueAsNewWorkflowExecution -> 3
        FailWorkflowExecution -> 4
        RecordMarker -> 5
        RequestCancelActivityTask -> 6
        RequestCancelExternalWorkflowExecution -> 7
        ScheduleActivityTask -> 8
        ScheduleLambdaFunction -> 9
        SignalExternalWorkflowExecution -> 10
        StartChildWorkflowExecution -> 11
        StartTimer -> 12
        DecisionType' name -> (error . showText) $ "Unknown DecisionType: " <> original name

-- | Represents the bounds of /known/ $DecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DecisionType where
    minBound = CancelTimer
    maxBound = StartTimer

instance Hashable     DecisionType
instance NFData       DecisionType
instance ToByteString DecisionType
instance ToQuery      DecisionType
instance ToHeader     DecisionType

instance ToJSON DecisionType where
    toJSON = toJSONText
