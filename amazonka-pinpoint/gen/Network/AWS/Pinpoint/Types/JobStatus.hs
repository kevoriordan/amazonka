{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JobStatus (
  JobStatus (
    ..
    , JSCompleted
    , JSCompleting
    , JSCreated
    , JSFailed
    , JSFailing
    , JSInitializing
    , JSPendingJob
    , JSPreparingForInitialization
    , JSProcessing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobStatus = JobStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern JSCompleted :: JobStatus
pattern JSCompleted = JobStatus' "COMPLETED"

pattern JSCompleting :: JobStatus
pattern JSCompleting = JobStatus' "COMPLETING"

pattern JSCreated :: JobStatus
pattern JSCreated = JobStatus' "CREATED"

pattern JSFailed :: JobStatus
pattern JSFailed = JobStatus' "FAILED"

pattern JSFailing :: JobStatus
pattern JSFailing = JobStatus' "FAILING"

pattern JSInitializing :: JobStatus
pattern JSInitializing = JobStatus' "INITIALIZING"

pattern JSPendingJob :: JobStatus
pattern JSPendingJob = JobStatus' "PENDING_JOB"

pattern JSPreparingForInitialization :: JobStatus
pattern JSPreparingForInitialization = JobStatus' "PREPARING_FOR_INITIALIZATION"

pattern JSProcessing :: JobStatus
pattern JSProcessing = JobStatus' "PROCESSING"

{-# COMPLETE
  JSCompleted,
  JSCompleting,
  JSCreated,
  JSFailed,
  JSFailing,
  JSInitializing,
  JSPendingJob,
  JSPreparingForInitialization,
  JSProcessing,
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
        0 -> JSCompleted
        1 -> JSCompleting
        2 -> JSCreated
        3 -> JSFailed
        4 -> JSFailing
        5 -> JSInitializing
        6 -> JSPendingJob
        7 -> JSPreparingForInitialization
        8 -> JSProcessing
        _ -> (error . showText) $ "Unknown index for JobStatus: " <> toText i
    fromEnum x = case x of
        JSCompleted -> 0
        JSCompleting -> 1
        JSCreated -> 2
        JSFailed -> 3
        JSFailing -> 4
        JSInitializing -> 5
        JSPendingJob -> 6
        JSPreparingForInitialization -> 7
        JSProcessing -> 8
        JobStatus' name -> (error . showText) $ "Unknown JobStatus: " <> original name

-- | Represents the bounds of /known/ $JobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobStatus where
    minBound = JSCompleted
    maxBound = JSProcessing

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"
