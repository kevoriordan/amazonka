{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.JobStatus (
  JobStatus (
    ..
    , Failed
    , Pending
    , Runnable
    , Running
    , Starting
    , Submitted
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobStatus = JobStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Failed :: JobStatus
pattern Failed = JobStatus' "FAILED"

pattern Pending :: JobStatus
pattern Pending = JobStatus' "PENDING"

pattern Runnable :: JobStatus
pattern Runnable = JobStatus' "RUNNABLE"

pattern Running :: JobStatus
pattern Running = JobStatus' "RUNNING"

pattern Starting :: JobStatus
pattern Starting = JobStatus' "STARTING"

pattern Submitted :: JobStatus
pattern Submitted = JobStatus' "SUBMITTED"

pattern Succeeded :: JobStatus
pattern Succeeded = JobStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  Pending,
  Runnable,
  Running,
  Starting,
  Submitted,
  Succeeded,
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
        0 -> Failed
        1 -> Pending
        2 -> Runnable
        3 -> Running
        4 -> Starting
        5 -> Submitted
        6 -> Succeeded
        _ -> (error . showText) $ "Unknown index for JobStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        Pending -> 1
        Runnable -> 2
        Running -> 3
        Starting -> 4
        Submitted -> 5
        Succeeded -> 6
        JobStatus' name -> (error . showText) $ "Unknown JobStatus: " <> original name

-- | Represents the bounds of /known/ $JobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobStatus where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance ToJSON JobStatus where
    toJSON = toJSONText

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"
