{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.TaskStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.TaskStatus (
  TaskStatus (
    ..
    , Completed
    , Failed
    , InProgress
    , Scheduled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskStatus = TaskStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Completed :: TaskStatus
pattern Completed = TaskStatus' "completed"

pattern Failed :: TaskStatus
pattern Failed = TaskStatus' "failed"

pattern InProgress :: TaskStatus
pattern InProgress = TaskStatus' "inProgress"

pattern Scheduled :: TaskStatus
pattern Scheduled = TaskStatus' "scheduled"

{-# COMPLETE
  Completed,
  Failed,
  InProgress,
  Scheduled,
  TaskStatus' #-}

instance FromText TaskStatus where
    parser = (TaskStatus' . mk) <$> takeText

instance ToText TaskStatus where
    toText (TaskStatus' ci) = original ci

-- | Represents an enum of /known/ $TaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> Failed
        2 -> InProgress
        3 -> Scheduled
        _ -> (error . showText) $ "Unknown index for TaskStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Failed -> 1
        InProgress -> 2
        Scheduled -> 3
        TaskStatus' name -> (error . showText) $ "Unknown TaskStatus: " <> original name

-- | Represents the bounds of /known/ $TaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskStatus where
    minBound = Completed
    maxBound = Scheduled

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance ToJSON TaskStatus where
    toJSON = toJSONText

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"
