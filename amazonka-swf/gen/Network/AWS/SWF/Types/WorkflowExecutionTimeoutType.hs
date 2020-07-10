{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionTimeoutType (
  WorkflowExecutionTimeoutType (
    ..
    , WETTStartToClose
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkflowExecutionTimeoutType = WorkflowExecutionTimeoutType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern WETTStartToClose :: WorkflowExecutionTimeoutType
pattern WETTStartToClose = WorkflowExecutionTimeoutType' "START_TO_CLOSE"

{-# COMPLETE
  WETTStartToClose,
  WorkflowExecutionTimeoutType' #-}

instance FromText WorkflowExecutionTimeoutType where
    parser = (WorkflowExecutionTimeoutType' . mk) <$> takeText

instance ToText WorkflowExecutionTimeoutType where
    toText (WorkflowExecutionTimeoutType' ci) = original ci

-- | Represents an enum of /known/ $WorkflowExecutionTimeoutType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkflowExecutionTimeoutType where
    toEnum i = case i of
        0 -> WETTStartToClose
        _ -> (error . showText) $ "Unknown index for WorkflowExecutionTimeoutType: " <> toText i
    fromEnum x = case x of
        WETTStartToClose -> 0
        WorkflowExecutionTimeoutType' name -> (error . showText) $ "Unknown WorkflowExecutionTimeoutType: " <> original name

-- | Represents the bounds of /known/ $WorkflowExecutionTimeoutType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkflowExecutionTimeoutType where
    minBound = WETTStartToClose
    maxBound = WETTStartToClose

instance Hashable     WorkflowExecutionTimeoutType
instance NFData       WorkflowExecutionTimeoutType
instance ToByteString WorkflowExecutionTimeoutType
instance ToQuery      WorkflowExecutionTimeoutType
instance ToHeader     WorkflowExecutionTimeoutType

instance FromJSON WorkflowExecutionTimeoutType where
    parseJSON = parseJSONText "WorkflowExecutionTimeoutType"
