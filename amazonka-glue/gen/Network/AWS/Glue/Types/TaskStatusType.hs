{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TaskStatusType (
  TaskStatusType (
    ..
    , Failed
    , Running
    , Starting
    , Stopped
    , Stopping
    , Succeeded
    , Timeout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskStatusType = TaskStatusType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Failed :: TaskStatusType
pattern Failed = TaskStatusType' "FAILED"

pattern Running :: TaskStatusType
pattern Running = TaskStatusType' "RUNNING"

pattern Starting :: TaskStatusType
pattern Starting = TaskStatusType' "STARTING"

pattern Stopped :: TaskStatusType
pattern Stopped = TaskStatusType' "STOPPED"

pattern Stopping :: TaskStatusType
pattern Stopping = TaskStatusType' "STOPPING"

pattern Succeeded :: TaskStatusType
pattern Succeeded = TaskStatusType' "SUCCEEDED"

pattern Timeout :: TaskStatusType
pattern Timeout = TaskStatusType' "TIMEOUT"

{-# COMPLETE
  Failed,
  Running,
  Starting,
  Stopped,
  Stopping,
  Succeeded,
  Timeout,
  TaskStatusType' #-}

instance FromText TaskStatusType where
    parser = (TaskStatusType' . mk) <$> takeText

instance ToText TaskStatusType where
    toText (TaskStatusType' ci) = original ci

-- | Represents an enum of /known/ $TaskStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskStatusType where
    toEnum i = case i of
        0 -> Failed
        1 -> Running
        2 -> Starting
        3 -> Stopped
        4 -> Stopping
        5 -> Succeeded
        6 -> Timeout
        _ -> (error . showText) $ "Unknown index for TaskStatusType: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        Running -> 1
        Starting -> 2
        Stopped -> 3
        Stopping -> 4
        Succeeded -> 5
        Timeout -> 6
        TaskStatusType' name -> (error . showText) $ "Unknown TaskStatusType: " <> original name

-- | Represents the bounds of /known/ $TaskStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskStatusType where
    minBound = Failed
    maxBound = Timeout

instance Hashable     TaskStatusType
instance NFData       TaskStatusType
instance ToByteString TaskStatusType
instance ToQuery      TaskStatusType
instance ToHeader     TaskStatusType

instance ToJSON TaskStatusType where
    toJSON = toJSONText

instance FromJSON TaskStatusType where
    parseJSON = parseJSONText "TaskStatusType"
