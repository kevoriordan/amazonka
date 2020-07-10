{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunSortColumnType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TaskRunSortColumnType (
  TaskRunSortColumnType (
    ..
    , TRSCStarted
    , TRSCStatus
    , TRSCTaskRunType
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskRunSortColumnType = TaskRunSortColumnType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern TRSCStarted :: TaskRunSortColumnType
pattern TRSCStarted = TaskRunSortColumnType' "STARTED"

pattern TRSCStatus :: TaskRunSortColumnType
pattern TRSCStatus = TaskRunSortColumnType' "STATUS"

pattern TRSCTaskRunType :: TaskRunSortColumnType
pattern TRSCTaskRunType = TaskRunSortColumnType' "TASK_RUN_TYPE"

{-# COMPLETE
  TRSCStarted,
  TRSCStatus,
  TRSCTaskRunType,
  TaskRunSortColumnType' #-}

instance FromText TaskRunSortColumnType where
    parser = (TaskRunSortColumnType' . mk) <$> takeText

instance ToText TaskRunSortColumnType where
    toText (TaskRunSortColumnType' ci) = original ci

-- | Represents an enum of /known/ $TaskRunSortColumnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskRunSortColumnType where
    toEnum i = case i of
        0 -> TRSCStarted
        1 -> TRSCStatus
        2 -> TRSCTaskRunType
        _ -> (error . showText) $ "Unknown index for TaskRunSortColumnType: " <> toText i
    fromEnum x = case x of
        TRSCStarted -> 0
        TRSCStatus -> 1
        TRSCTaskRunType -> 2
        TaskRunSortColumnType' name -> (error . showText) $ "Unknown TaskRunSortColumnType: " <> original name

-- | Represents the bounds of /known/ $TaskRunSortColumnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskRunSortColumnType where
    minBound = TRSCStarted
    maxBound = TRSCTaskRunType

instance Hashable     TaskRunSortColumnType
instance NFData       TaskRunSortColumnType
instance ToByteString TaskRunSortColumnType
instance ToQuery      TaskRunSortColumnType
instance ToHeader     TaskRunSortColumnType

instance ToJSON TaskRunSortColumnType where
    toJSON = toJSONText
