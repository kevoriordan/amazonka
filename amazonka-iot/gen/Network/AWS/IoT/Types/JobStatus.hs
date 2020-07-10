{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobStatus (
  JobStatus (
    ..
    , JSCanceled
    , JSCompleted
    , JSDeletionInProgress
    , JSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobStatus = JobStatus' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern JSCanceled :: JobStatus
pattern JSCanceled = JobStatus' "CANCELED"

pattern JSCompleted :: JobStatus
pattern JSCompleted = JobStatus' "COMPLETED"

pattern JSDeletionInProgress :: JobStatus
pattern JSDeletionInProgress = JobStatus' "DELETION_IN_PROGRESS"

pattern JSInProgress :: JobStatus
pattern JSInProgress = JobStatus' "IN_PROGRESS"

{-# COMPLETE
  JSCanceled,
  JSCompleted,
  JSDeletionInProgress,
  JSInProgress,
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
        0 -> JSCanceled
        1 -> JSCompleted
        2 -> JSDeletionInProgress
        3 -> JSInProgress
        _ -> (error . showText) $ "Unknown index for JobStatus: " <> toText i
    fromEnum x = case x of
        JSCanceled -> 0
        JSCompleted -> 1
        JSDeletionInProgress -> 2
        JSInProgress -> 3
        JobStatus' name -> (error . showText) $ "Unknown JobStatus: " <> original name

-- | Represents the bounds of /known/ $JobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobStatus where
    minBound = JSCanceled
    maxBound = JSInProgress

instance Hashable     JobStatus
instance NFData       JobStatus
instance ToByteString JobStatus
instance ToQuery      JobStatus
instance ToHeader     JobStatus

instance ToJSON JobStatus where
    toJSON = toJSONText

instance FromJSON JobStatus where
    parseJSON = parseJSONText "JobStatus"
