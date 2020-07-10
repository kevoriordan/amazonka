{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause (
  WorkflowExecutionTerminatedCause (
    ..
    , WETCChildPolicyApplied
    , WETCEventLimitExceeded
    , WETCOperatorInitiated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkflowExecutionTerminatedCause = WorkflowExecutionTerminatedCause' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern WETCChildPolicyApplied :: WorkflowExecutionTerminatedCause
pattern WETCChildPolicyApplied = WorkflowExecutionTerminatedCause' "CHILD_POLICY_APPLIED"

pattern WETCEventLimitExceeded :: WorkflowExecutionTerminatedCause
pattern WETCEventLimitExceeded = WorkflowExecutionTerminatedCause' "EVENT_LIMIT_EXCEEDED"

pattern WETCOperatorInitiated :: WorkflowExecutionTerminatedCause
pattern WETCOperatorInitiated = WorkflowExecutionTerminatedCause' "OPERATOR_INITIATED"

{-# COMPLETE
  WETCChildPolicyApplied,
  WETCEventLimitExceeded,
  WETCOperatorInitiated,
  WorkflowExecutionTerminatedCause' #-}

instance FromText WorkflowExecutionTerminatedCause where
    parser = (WorkflowExecutionTerminatedCause' . mk) <$> takeText

instance ToText WorkflowExecutionTerminatedCause where
    toText (WorkflowExecutionTerminatedCause' ci) = original ci

-- | Represents an enum of /known/ $WorkflowExecutionTerminatedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkflowExecutionTerminatedCause where
    toEnum i = case i of
        0 -> WETCChildPolicyApplied
        1 -> WETCEventLimitExceeded
        2 -> WETCOperatorInitiated
        _ -> (error . showText) $ "Unknown index for WorkflowExecutionTerminatedCause: " <> toText i
    fromEnum x = case x of
        WETCChildPolicyApplied -> 0
        WETCEventLimitExceeded -> 1
        WETCOperatorInitiated -> 2
        WorkflowExecutionTerminatedCause' name -> (error . showText) $ "Unknown WorkflowExecutionTerminatedCause: " <> original name

-- | Represents the bounds of /known/ $WorkflowExecutionTerminatedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkflowExecutionTerminatedCause where
    minBound = WETCChildPolicyApplied
    maxBound = WETCOperatorInitiated

instance Hashable     WorkflowExecutionTerminatedCause
instance NFData       WorkflowExecutionTerminatedCause
instance ToByteString WorkflowExecutionTerminatedCause
instance ToQuery      WorkflowExecutionTerminatedCause
instance ToHeader     WorkflowExecutionTerminatedCause

instance FromJSON WorkflowExecutionTerminatedCause where
    parseJSON = parseJSONText "WorkflowExecutionTerminatedCause"
