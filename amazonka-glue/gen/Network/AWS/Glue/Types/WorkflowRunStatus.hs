{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.WorkflowRunStatus (
  WorkflowRunStatus (
    ..
    , WRSCompleted
    , WRSRunning
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkflowRunStatus = WorkflowRunStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern WRSCompleted :: WorkflowRunStatus
pattern WRSCompleted = WorkflowRunStatus' "COMPLETED"

pattern WRSRunning :: WorkflowRunStatus
pattern WRSRunning = WorkflowRunStatus' "RUNNING"

{-# COMPLETE
  WRSCompleted,
  WRSRunning,
  WorkflowRunStatus' #-}

instance FromText WorkflowRunStatus where
    parser = (WorkflowRunStatus' . mk) <$> takeText

instance ToText WorkflowRunStatus where
    toText (WorkflowRunStatus' ci) = original ci

-- | Represents an enum of /known/ $WorkflowRunStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkflowRunStatus where
    toEnum i = case i of
        0 -> WRSCompleted
        1 -> WRSRunning
        _ -> (error . showText) $ "Unknown index for WorkflowRunStatus: " <> toText i
    fromEnum x = case x of
        WRSCompleted -> 0
        WRSRunning -> 1
        WorkflowRunStatus' name -> (error . showText) $ "Unknown WorkflowRunStatus: " <> original name

-- | Represents the bounds of /known/ $WorkflowRunStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkflowRunStatus where
    minBound = WRSCompleted
    maxBound = WRSRunning

instance Hashable     WorkflowRunStatus
instance NFData       WorkflowRunStatus
instance ToByteString WorkflowRunStatus
instance ToQuery      WorkflowRunStatus
instance ToHeader     WorkflowRunStatus

instance FromJSON WorkflowRunStatus where
    parseJSON = parseJSONText "WorkflowRunStatus"
