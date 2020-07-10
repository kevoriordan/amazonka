{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.ExecutionStatus (
  ExecutionStatus (
    ..
    , Completed
    , Pending
    , PendingConcurrency
    , PendingDevice
    , Preparing
    , Processing
    , Running
    , Scheduling
    , Stopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExecutionStatus = ExecutionStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Completed :: ExecutionStatus
pattern Completed = ExecutionStatus' "COMPLETED"

pattern Pending :: ExecutionStatus
pattern Pending = ExecutionStatus' "PENDING"

pattern PendingConcurrency :: ExecutionStatus
pattern PendingConcurrency = ExecutionStatus' "PENDING_CONCURRENCY"

pattern PendingDevice :: ExecutionStatus
pattern PendingDevice = ExecutionStatus' "PENDING_DEVICE"

pattern Preparing :: ExecutionStatus
pattern Preparing = ExecutionStatus' "PREPARING"

pattern Processing :: ExecutionStatus
pattern Processing = ExecutionStatus' "PROCESSING"

pattern Running :: ExecutionStatus
pattern Running = ExecutionStatus' "RUNNING"

pattern Scheduling :: ExecutionStatus
pattern Scheduling = ExecutionStatus' "SCHEDULING"

pattern Stopping :: ExecutionStatus
pattern Stopping = ExecutionStatus' "STOPPING"

{-# COMPLETE
  Completed,
  Pending,
  PendingConcurrency,
  PendingDevice,
  Preparing,
  Processing,
  Running,
  Scheduling,
  Stopping,
  ExecutionStatus' #-}

instance FromText ExecutionStatus where
    parser = (ExecutionStatus' . mk) <$> takeText

instance ToText ExecutionStatus where
    toText (ExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExecutionStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> Pending
        2 -> PendingConcurrency
        3 -> PendingDevice
        4 -> Preparing
        5 -> Processing
        6 -> Running
        7 -> Scheduling
        8 -> Stopping
        _ -> (error . showText) $ "Unknown index for ExecutionStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Pending -> 1
        PendingConcurrency -> 2
        PendingDevice -> 3
        Preparing -> 4
        Processing -> 5
        Running -> 6
        Scheduling -> 7
        Stopping -> 8
        ExecutionStatus' name -> (error . showText) $ "Unknown ExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExecutionStatus where
    minBound = Completed
    maxBound = Stopping

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"
