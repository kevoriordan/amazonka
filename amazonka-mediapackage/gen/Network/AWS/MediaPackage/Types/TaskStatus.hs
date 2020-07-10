{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.TaskStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.TaskStatus (
  TaskStatus (
    ..
    , Failed
    , InProgress
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskStatus = TaskStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Failed :: TaskStatus
pattern Failed = TaskStatus' "FAILED"

pattern InProgress :: TaskStatus
pattern InProgress = TaskStatus' "IN_PROGRESS"

pattern Succeeded :: TaskStatus
pattern Succeeded = TaskStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  Succeeded,
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
        0 -> Failed
        1 -> InProgress
        2 -> Succeeded
        _ -> (error . showText) $ "Unknown index for TaskStatus: " <> toText i
    fromEnum x = case x of
        Failed -> 0
        InProgress -> 1
        Succeeded -> 2
        TaskStatus' name -> (error . showText) $ "Unknown TaskStatus: " <> original name

-- | Represents the bounds of /known/ $TaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskStatus where
    minBound = Failed
    maxBound = Succeeded

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"
