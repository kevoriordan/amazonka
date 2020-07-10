{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobRunState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.JobRunState (
  JobRunState (
    ..
    , JRSFailed
    , JRSRunning
    , JRSStarting
    , JRSStopped
    , JRSStopping
    , JRSSucceeded
    , JRSTimeout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data JobRunState = JobRunState' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern JRSFailed :: JobRunState
pattern JRSFailed = JobRunState' "FAILED"

pattern JRSRunning :: JobRunState
pattern JRSRunning = JobRunState' "RUNNING"

pattern JRSStarting :: JobRunState
pattern JRSStarting = JobRunState' "STARTING"

pattern JRSStopped :: JobRunState
pattern JRSStopped = JobRunState' "STOPPED"

pattern JRSStopping :: JobRunState
pattern JRSStopping = JobRunState' "STOPPING"

pattern JRSSucceeded :: JobRunState
pattern JRSSucceeded = JobRunState' "SUCCEEDED"

pattern JRSTimeout :: JobRunState
pattern JRSTimeout = JobRunState' "TIMEOUT"

{-# COMPLETE
  JRSFailed,
  JRSRunning,
  JRSStarting,
  JRSStopped,
  JRSStopping,
  JRSSucceeded,
  JRSTimeout,
  JobRunState' #-}

instance FromText JobRunState where
    parser = (JobRunState' . mk) <$> takeText

instance ToText JobRunState where
    toText (JobRunState' ci) = original ci

-- | Represents an enum of /known/ $JobRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum JobRunState where
    toEnum i = case i of
        0 -> JRSFailed
        1 -> JRSRunning
        2 -> JRSStarting
        3 -> JRSStopped
        4 -> JRSStopping
        5 -> JRSSucceeded
        6 -> JRSTimeout
        _ -> (error . showText) $ "Unknown index for JobRunState: " <> toText i
    fromEnum x = case x of
        JRSFailed -> 0
        JRSRunning -> 1
        JRSStarting -> 2
        JRSStopped -> 3
        JRSStopping -> 4
        JRSSucceeded -> 5
        JRSTimeout -> 6
        JobRunState' name -> (error . showText) $ "Unknown JobRunState: " <> original name

-- | Represents the bounds of /known/ $JobRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded JobRunState where
    minBound = JRSFailed
    maxBound = JRSTimeout

instance Hashable     JobRunState
instance NFData       JobRunState
instance ToByteString JobRunState
instance ToQuery      JobRunState
instance ToHeader     JobRunState

instance ToJSON JobRunState where
    toJSON = toJSONText

instance FromJSON JobRunState where
    parseJSON = parseJSONText "JobRunState"
