{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionFailedCause (
  SignalExternalWorkflowExecutionFailedCause (
    ..
    , SEWEFCOperationNotPermitted
    , SEWEFCSignalExternalWorkflowExecutionRateExceeded
    , SEWEFCUnknownExternalWorkflowExecution
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SignalExternalWorkflowExecutionFailedCause = SignalExternalWorkflowExecutionFailedCause' (CI
                                                                                                 Text)
                                                    deriving (Eq, Ord, Read,
                                                              Show, Data,
                                                              Typeable, Generic)

pattern SEWEFCOperationNotPermitted :: SignalExternalWorkflowExecutionFailedCause
pattern SEWEFCOperationNotPermitted = SignalExternalWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern SEWEFCSignalExternalWorkflowExecutionRateExceeded :: SignalExternalWorkflowExecutionFailedCause
pattern SEWEFCSignalExternalWorkflowExecutionRateExceeded = SignalExternalWorkflowExecutionFailedCause' "SIGNAL_EXTERNAL_WORKFLOW_EXECUTION_RATE_EXCEEDED"

pattern SEWEFCUnknownExternalWorkflowExecution :: SignalExternalWorkflowExecutionFailedCause
pattern SEWEFCUnknownExternalWorkflowExecution = SignalExternalWorkflowExecutionFailedCause' "UNKNOWN_EXTERNAL_WORKFLOW_EXECUTION"

{-# COMPLETE
  SEWEFCOperationNotPermitted,
  SEWEFCSignalExternalWorkflowExecutionRateExceeded,
  SEWEFCUnknownExternalWorkflowExecution,
  SignalExternalWorkflowExecutionFailedCause' #-}

instance FromText SignalExternalWorkflowExecutionFailedCause where
    parser = (SignalExternalWorkflowExecutionFailedCause' . mk) <$> takeText

instance ToText SignalExternalWorkflowExecutionFailedCause where
    toText (SignalExternalWorkflowExecutionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $SignalExternalWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SignalExternalWorkflowExecutionFailedCause where
    toEnum i = case i of
        0 -> SEWEFCOperationNotPermitted
        1 -> SEWEFCSignalExternalWorkflowExecutionRateExceeded
        2 -> SEWEFCUnknownExternalWorkflowExecution
        _ -> (error . showText) $ "Unknown index for SignalExternalWorkflowExecutionFailedCause: " <> toText i
    fromEnum x = case x of
        SEWEFCOperationNotPermitted -> 0
        SEWEFCSignalExternalWorkflowExecutionRateExceeded -> 1
        SEWEFCUnknownExternalWorkflowExecution -> 2
        SignalExternalWorkflowExecutionFailedCause' name -> (error . showText) $ "Unknown SignalExternalWorkflowExecutionFailedCause: " <> original name

-- | Represents the bounds of /known/ $SignalExternalWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SignalExternalWorkflowExecutionFailedCause where
    minBound = SEWEFCOperationNotPermitted
    maxBound = SEWEFCUnknownExternalWorkflowExecution

instance Hashable     SignalExternalWorkflowExecutionFailedCause
instance NFData       SignalExternalWorkflowExecutionFailedCause
instance ToByteString SignalExternalWorkflowExecutionFailedCause
instance ToQuery      SignalExternalWorkflowExecutionFailedCause
instance ToHeader     SignalExternalWorkflowExecutionFailedCause

instance FromJSON SignalExternalWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "SignalExternalWorkflowExecutionFailedCause"
