{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause (
  WorkflowExecutionCancelRequestedCause (
    ..
    , ChildPolicyApplied
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkflowExecutionCancelRequestedCause = WorkflowExecutionCancelRequestedCause' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern ChildPolicyApplied :: WorkflowExecutionCancelRequestedCause
pattern ChildPolicyApplied = WorkflowExecutionCancelRequestedCause' "CHILD_POLICY_APPLIED"

{-# COMPLETE
  ChildPolicyApplied,
  WorkflowExecutionCancelRequestedCause' #-}

instance FromText WorkflowExecutionCancelRequestedCause where
    parser = (WorkflowExecutionCancelRequestedCause' . mk) <$> takeText

instance ToText WorkflowExecutionCancelRequestedCause where
    toText (WorkflowExecutionCancelRequestedCause' ci) = original ci

-- | Represents an enum of /known/ $WorkflowExecutionCancelRequestedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkflowExecutionCancelRequestedCause where
    toEnum i = case i of
        0 -> ChildPolicyApplied
        _ -> (error . showText) $ "Unknown index for WorkflowExecutionCancelRequestedCause: " <> toText i
    fromEnum x = case x of
        ChildPolicyApplied -> 0
        WorkflowExecutionCancelRequestedCause' name -> (error . showText) $ "Unknown WorkflowExecutionCancelRequestedCause: " <> original name

-- | Represents the bounds of /known/ $WorkflowExecutionCancelRequestedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkflowExecutionCancelRequestedCause where
    minBound = ChildPolicyApplied
    maxBound = ChildPolicyApplied

instance Hashable     WorkflowExecutionCancelRequestedCause
instance NFData       WorkflowExecutionCancelRequestedCause
instance ToByteString WorkflowExecutionCancelRequestedCause
instance ToQuery      WorkflowExecutionCancelRequestedCause
instance ToHeader     WorkflowExecutionCancelRequestedCause

instance FromJSON WorkflowExecutionCancelRequestedCause where
    parseJSON = parseJSONText "WorkflowExecutionCancelRequestedCause"
