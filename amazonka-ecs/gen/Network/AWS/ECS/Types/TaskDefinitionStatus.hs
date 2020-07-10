{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.TaskDefinitionStatus (
  TaskDefinitionStatus (
    ..
    , TDSActive
    , TDSInactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskDefinitionStatus = TaskDefinitionStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern TDSActive :: TaskDefinitionStatus
pattern TDSActive = TaskDefinitionStatus' "ACTIVE"

pattern TDSInactive :: TaskDefinitionStatus
pattern TDSInactive = TaskDefinitionStatus' "INACTIVE"

{-# COMPLETE
  TDSActive,
  TDSInactive,
  TaskDefinitionStatus' #-}

instance FromText TaskDefinitionStatus where
    parser = (TaskDefinitionStatus' . mk) <$> takeText

instance ToText TaskDefinitionStatus where
    toText (TaskDefinitionStatus' ci) = original ci

-- | Represents an enum of /known/ $TaskDefinitionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskDefinitionStatus where
    toEnum i = case i of
        0 -> TDSActive
        1 -> TDSInactive
        _ -> (error . showText) $ "Unknown index for TaskDefinitionStatus: " <> toText i
    fromEnum x = case x of
        TDSActive -> 0
        TDSInactive -> 1
        TaskDefinitionStatus' name -> (error . showText) $ "Unknown TaskDefinitionStatus: " <> original name

-- | Represents the bounds of /known/ $TaskDefinitionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskDefinitionStatus where
    minBound = TDSActive
    maxBound = TDSInactive

instance Hashable     TaskDefinitionStatus
instance NFData       TaskDefinitionStatus
instance ToByteString TaskDefinitionStatus
instance ToQuery      TaskDefinitionStatus
instance ToHeader     TaskDefinitionStatus

instance ToJSON TaskDefinitionStatus where
    toJSON = toJSONText

instance FromJSON TaskDefinitionStatus where
    parseJSON = parseJSONText "TaskDefinitionStatus"
