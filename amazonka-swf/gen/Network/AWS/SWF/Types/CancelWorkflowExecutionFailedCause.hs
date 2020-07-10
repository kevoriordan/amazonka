{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.CancelWorkflowExecutionFailedCause (
  CancelWorkflowExecutionFailedCause (
    ..
    , COperationNotPermitted
    , CUnhandledDecision
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CancelWorkflowExecutionFailedCause = CancelWorkflowExecutionFailedCause' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern COperationNotPermitted :: CancelWorkflowExecutionFailedCause
pattern COperationNotPermitted = CancelWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern CUnhandledDecision :: CancelWorkflowExecutionFailedCause
pattern CUnhandledDecision = CancelWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  COperationNotPermitted,
  CUnhandledDecision,
  CancelWorkflowExecutionFailedCause' #-}

instance FromText CancelWorkflowExecutionFailedCause where
    parser = (CancelWorkflowExecutionFailedCause' . mk) <$> takeText

instance ToText CancelWorkflowExecutionFailedCause where
    toText (CancelWorkflowExecutionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $CancelWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CancelWorkflowExecutionFailedCause where
    toEnum i = case i of
        0 -> COperationNotPermitted
        1 -> CUnhandledDecision
        _ -> (error . showText) $ "Unknown index for CancelWorkflowExecutionFailedCause: " <> toText i
    fromEnum x = case x of
        COperationNotPermitted -> 0
        CUnhandledDecision -> 1
        CancelWorkflowExecutionFailedCause' name -> (error . showText) $ "Unknown CancelWorkflowExecutionFailedCause: " <> original name

-- | Represents the bounds of /known/ $CancelWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CancelWorkflowExecutionFailedCause where
    minBound = COperationNotPermitted
    maxBound = CUnhandledDecision

instance Hashable     CancelWorkflowExecutionFailedCause
instance NFData       CancelWorkflowExecutionFailedCause
instance ToByteString CancelWorkflowExecutionFailedCause
instance ToQuery      CancelWorkflowExecutionFailedCause
instance ToHeader     CancelWorkflowExecutionFailedCause

instance FromJSON CancelWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "CancelWorkflowExecutionFailedCause"
