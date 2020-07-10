{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause (
  FailWorkflowExecutionFailedCause (
    ..
    , FWEFCOperationNotPermitted
    , FWEFCUnhandledDecision
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FailWorkflowExecutionFailedCause = FailWorkflowExecutionFailedCause' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern FWEFCOperationNotPermitted :: FailWorkflowExecutionFailedCause
pattern FWEFCOperationNotPermitted = FailWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern FWEFCUnhandledDecision :: FailWorkflowExecutionFailedCause
pattern FWEFCUnhandledDecision = FailWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  FWEFCOperationNotPermitted,
  FWEFCUnhandledDecision,
  FailWorkflowExecutionFailedCause' #-}

instance FromText FailWorkflowExecutionFailedCause where
    parser = (FailWorkflowExecutionFailedCause' . mk) <$> takeText

instance ToText FailWorkflowExecutionFailedCause where
    toText (FailWorkflowExecutionFailedCause' ci) = original ci

-- | Represents an enum of /known/ $FailWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FailWorkflowExecutionFailedCause where
    toEnum i = case i of
        0 -> FWEFCOperationNotPermitted
        1 -> FWEFCUnhandledDecision
        _ -> (error . showText) $ "Unknown index for FailWorkflowExecutionFailedCause: " <> toText i
    fromEnum x = case x of
        FWEFCOperationNotPermitted -> 0
        FWEFCUnhandledDecision -> 1
        FailWorkflowExecutionFailedCause' name -> (error . showText) $ "Unknown FailWorkflowExecutionFailedCause: " <> original name

-- | Represents the bounds of /known/ $FailWorkflowExecutionFailedCause.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FailWorkflowExecutionFailedCause where
    minBound = FWEFCOperationNotPermitted
    maxBound = FWEFCUnhandledDecision

instance Hashable     FailWorkflowExecutionFailedCause
instance NFData       FailWorkflowExecutionFailedCause
instance ToByteString FailWorkflowExecutionFailedCause
instance ToQuery      FailWorkflowExecutionFailedCause
instance ToHeader     FailWorkflowExecutionFailedCause

instance FromJSON FailWorkflowExecutionFailedCause where
    parseJSON = parseJSONText "FailWorkflowExecutionFailedCause"
