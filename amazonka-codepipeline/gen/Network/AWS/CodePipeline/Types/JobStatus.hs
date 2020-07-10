{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.JobStatus (
  JobStatus (
    ..
    , JSCreated
    , JSDispatched
    , JSFailed
    , JSInProgress
    , JSQueued
    , JSSucceeded
    , JSTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobStatus = JobStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern JSCreated :: JobStatus
pattern JSCreated = JobStatus' "Created"

pattern JSDispatched :: JobStatus
pattern JSDispatched = JobStatus' "Dispatched"

pattern JSFailed :: JobStatus
pattern JSFailed = JobStatus' "Failed"

pattern JSInProgress :: JobStatus
pattern JSInProgress = JobStatus' "InProgress"

pattern JSQueued :: JobStatus
pattern JSQueued = JobStatus' "Queued"

pattern JSSucceeded :: JobStatus
pattern JSSucceeded = JobStatus' "Succeeded"

pattern JSTimedOut :: JobStatus
pattern JSTimedOut = JobStatus' "TimedOut"

{-# COMPLETE
  JSCreated,
  JSDispatched,
  JSFailed,
  JSInProgress,
  JSQueued,
  JSSucceeded,
  JSTimedOut,
  JobStatus' #-}

instance FromText JobStatus where
    parser = (JobStatus' . mk) <$> takeText

instance ToText JobStatus where
    toText (JobStatus' ci) = original ci

-- | Represents an enum of /known/ $JobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobStatus where
    toEnum i = case i of
        0 -> JSCreated
        1 -> JSDispatched
        2 -> JSFailed
        3 -> JSInProgress
        4 -> JSQueued
        5 -> JSSucceeded
        6 -> JSTimedOut
        _ -> (error . showText) $ "Unknown index for JobStatus: " <> toText i
    fromEnum x = case x of
        JSCreated -> 0
        JSDispatched -> 1
        JSFailed -> 2
        JSInProgress -> 3
        JSQueued -> 4
        JSSucceeded -> 5
        JSTimedOut -> 6
        JobStatus' name -> (error . showText) $ "Unknown JobStatus: " <> original name

-- | Represents the bounds of /known/ $JobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobStatus where
    minBound = JSCreated
    maxBound = JSTimedOut

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"
