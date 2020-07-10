{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ContinueAsNewWorkflowExecutionFailedCause (
  ContinueAsNewWorkflowExecutionFailedCause (
    ..
    , CANWEFCContinueAsNewWorkflowExecutionRateExceeded
    , CANWEFCDefaultChildPolicyUndefined
    , CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
    , CANWEFCDefaultTaskListUndefined
    , CANWEFCDefaultTaskStartToCloseTimeoutUndefined
    , CANWEFCOperationNotPermitted
    , CANWEFCUnhandledDecision
    , CANWEFCWorkflowTypeDeprecated
    , CANWEFCWorkflowTypeDoesNotExist
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause' (CI
                                                                                               Text)
                                                   deriving (Eq, Ord, Read,
                                                             Show, Data,
                                                             Typeable, Generic)

pattern CANWEFCContinueAsNewWorkflowExecutionRateExceeded :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCContinueAsNewWorkflowExecutionRateExceeded = ContinueAsNewWorkflowExecutionFailedCause' "CONTINUE_AS_NEW_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern CANWEFCDefaultChildPolicyUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultChildPolicyUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_CHILD_POLICY_UNDEFINED"

pattern CANWEFCDefaultExecutionStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultExecutionStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_EXECUTION_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern CANWEFCDefaultTaskListUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultTaskListUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_LIST_UNDEFINED"

pattern CANWEFCDefaultTaskStartToCloseTimeoutUndefined :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCDefaultTaskStartToCloseTimeoutUndefined = ContinueAsNewWorkflowExecutionFailedCause' "DEFAULT_TASK_START_TO_CLOSE_TIMEOUT_UNDEFINED"

pattern CANWEFCOperationNotPermitted :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCOperationNotPermitted = ContinueAsNewWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CANWEFCUnhandledDecision :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCUnhandledDecision = ContinueAsNewWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

pattern CANWEFCWorkflowTypeDeprecated :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCWorkflowTypeDeprecated = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DEPRECATED"

pattern CANWEFCWorkflowTypeDoesNotExist :: ContinueAsNewWorkflowExecutionFailedCause
pattern CANWEFCWorkflowTypeDoesNotExist = ContinueAsNewWorkflowExecutionFailedCause' "WORKFLOW_TYPE_DOES_NOT_EXIST"

{-# COMPLETE
  CANWEFCContinueAsNewWorkflowExecutionRateExceeded,
  CANWEFCDefaultChildPolicyUndefined,
  CANWEFCDefaultExecutionStartToCloseTimeoutUndefined,
  CANWEFCDefaultTaskListUndefined,
  CANWEFCDefaultTaskStartToCloseTimeoutUndefined,
  CANWEFCOperationNotPermitted,
  CANWEFCUnhandledDecision,
  CANWEFCWorkflowTypeDeprecated,
  CANWEFCWorkflowTypeDoesNotExist,
  ContinueAsNewWorkflowExecutionFailedCause' #-}

instance FromText ContinueAsNewWorkflowExecutionFailedCause where
    parser = (ContinueAsNewWorkflowExecutionFailedCause' . mk) <$> takeText

instance ToText ContinueAsNewWorkflowExecutionFailedCause where
    toText (ContinueAsNewWorkflowExecutionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $ContinueAsNewWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContinueAsNewWorkflowExecutionFailedCause where
    toEnum i = case i of
        0 -> CANWEFCContinueAsNewWorkflowExecutionRateExceeded
        1 -> CANWEFCDefaultChildPolicyUndefined
        2 -> CANWEFCDefaultExecutionStartToCloseTimeoutUndefined
        3 -> CANWEFCDefaultTaskListUndefined
        4 -> CANWEFCDefaultTaskStartToCloseTimeoutUndefined
        5 -> CANWEFCOperationNotPermitted
        6 -> CANWEFCUnhandledDecision
        7 -> CANWEFCWorkflowTypeDeprecated
        8 -> CANWEFCWorkflowTypeDoesNotExist
        _ -> (error . showText) $ "Unknown index for ContinueAsNewWorkflowExecutionFailedCause: " <> toText i
    fromEnum x = case x of
        CANWEFCContinueAsNewWorkflowExecutionRateExceeded -> 0
        CANWEFCDefaultChildPolicyUndefined -> 1
        CANWEFCDefaultExecutionStartToCloseTimeoutUndefined -> 2
        CANWEFCDefaultTaskListUndefined -> 3
        CANWEFCDefaultTaskStartToCloseTimeoutUndefined -> 4
        CANWEFCOperationNotPermitted -> 5
        CANWEFCUnhandledDecision -> 6
        CANWEFCWorkflowTypeDeprecated -> 7
        CANWEFCWorkflowTypeDoesNotExist -> 8
        ContinueAsNewWorkflowExecutionFailedCause' name -> (error . showText) $ "Unknown ContinueAsNewWorkflowExecutionFailedCause: " <> original name

-- | Represents the bounds of /known/ $ContinueAsNewWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContinueAsNewWorkflowExecutionFailedCause where
    minBound = CANWEFCContinueAsNewWorkflowExecutionRateExceeded
    maxBound = CANWEFCWorkflowTypeDoesNotExist

instance Hashable     ContinueAsNewWorkflowExecutionFailedCause
instance NFData       ContinueAsNewWorkflowExecutionFailedCause
instance ToByteString ContinueAsNewWorkflowExecutionFailedCause
instance ToQuery      ContinueAsNewWorkflowExecutionFailedCause
instance ToHeader     ContinueAsNewWorkflowExecutionFailedCause

instance FromJSON ContinueAsNewWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "ContinueAsNewWorkflowExecutionFailedCause"
