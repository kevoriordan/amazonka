{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.CompleteWorkflowExecutionFailedCause (
  CompleteWorkflowExecutionFailedCause (
    ..
    , CWEFCOperationNotPermitted
    , CWEFCUnhandledDecision
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CompleteWorkflowExecutionFailedCause = CompleteWorkflowExecutionFailedCause' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern CWEFCOperationNotPermitted :: CompleteWorkflowExecutionFailedCause
pattern CWEFCOperationNotPermitted = CompleteWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CWEFCUnhandledDecision :: CompleteWorkflowExecutionFailedCause
pattern CWEFCUnhandledDecision = CompleteWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  CWEFCOperationNotPermitted,
  CWEFCUnhandledDecision,
  CompleteWorkflowExecutionFailedCause' #-}

instance FromText CompleteWorkflowExecutionFailedCause where
    parser = (CompleteWorkflowExecutionFailedCause' . mk) <$> takeText

instance ToText CompleteWorkflowExecutionFailedCause where
    toText (CompleteWorkflowExecutionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $CompleteWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CompleteWorkflowExecutionFailedCause where
    toEnum i = case i of
        0 -> CWEFCOperationNotPermitted
        1 -> CWEFCUnhandledDecision
        _ -> (error . showText) $ "Unknown index for CompleteWorkflowExecutionFailedCause: " <> toText i
    fromEnum x = case x of
        CWEFCOperationNotPermitted -> 0
        CWEFCUnhandledDecision -> 1
        CompleteWorkflowExecutionFailedCause' name -> (error . showText) $ "Unknown CompleteWorkflowExecutionFailedCause: " <> original name

-- | Represents the bounds of /known/ $CompleteWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CompleteWorkflowExecutionFailedCause where
    minBound = CWEFCOperationNotPermitted
    maxBound = CWEFCUnhandledDecision

instance Hashable     CompleteWorkflowExecutionFailedCause
instance NFData       CompleteWorkflowExecutionFailedCause
instance ToByteString CompleteWorkflowExecutionFailedCause
instance ToQuery      CompleteWorkflowExecutionFailedCause
instance ToHeader     CompleteWorkflowExecutionFailedCause

instance FromJSON CompleteWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CompleteWorkflowExecutionFailedCause"
