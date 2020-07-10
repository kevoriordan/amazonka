{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AutomationExecutionFilterKey (
  AutomationExecutionFilterKey (
    ..
    , AEFKAutomationType
    , AEFKCurrentAction
    , AEFKDocumentNamePrefix
    , AEFKExecutionId
    , AEFKExecutionStatus
    , AEFKParentExecutionId
    , AEFKStartTimeAfter
    , AEFKStartTimeBefore
    , AEFKTagKey
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutomationExecutionFilterKey = AutomationExecutionFilterKey' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern AEFKAutomationType :: AutomationExecutionFilterKey
pattern AEFKAutomationType = AutomationExecutionFilterKey' "AutomationType"

pattern AEFKCurrentAction :: AutomationExecutionFilterKey
pattern AEFKCurrentAction = AutomationExecutionFilterKey' "CurrentAction"

pattern AEFKDocumentNamePrefix :: AutomationExecutionFilterKey
pattern AEFKDocumentNamePrefix = AutomationExecutionFilterKey' "DocumentNamePrefix"

pattern AEFKExecutionId :: AutomationExecutionFilterKey
pattern AEFKExecutionId = AutomationExecutionFilterKey' "ExecutionId"

pattern AEFKExecutionStatus :: AutomationExecutionFilterKey
pattern AEFKExecutionStatus = AutomationExecutionFilterKey' "ExecutionStatus"

pattern AEFKParentExecutionId :: AutomationExecutionFilterKey
pattern AEFKParentExecutionId = AutomationExecutionFilterKey' "ParentExecutionId"

pattern AEFKStartTimeAfter :: AutomationExecutionFilterKey
pattern AEFKStartTimeAfter = AutomationExecutionFilterKey' "StartTimeAfter"

pattern AEFKStartTimeBefore :: AutomationExecutionFilterKey
pattern AEFKStartTimeBefore = AutomationExecutionFilterKey' "StartTimeBefore"

pattern AEFKTagKey :: AutomationExecutionFilterKey
pattern AEFKTagKey = AutomationExecutionFilterKey' "TagKey"

{-# COMPLETE
  AEFKAutomationType,
  AEFKCurrentAction,
  AEFKDocumentNamePrefix,
  AEFKExecutionId,
  AEFKExecutionStatus,
  AEFKParentExecutionId,
  AEFKStartTimeAfter,
  AEFKStartTimeBefore,
  AEFKTagKey,
  AutomationExecutionFilterKey' #-}

instance FromText AutomationExecutionFilterKey where
    parser = (AutomationExecutionFilterKey' . mk) <$> takeText

instance ToText AutomationExecutionFilterKey where
    toText (AutomationExecutionFilterKey' ci) = original ci

-- | Represents an enum of /known/ $AutomationExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutomationExecutionFilterKey where
    toEnum i = case i of
        0 -> AEFKAutomationType
        1 -> AEFKCurrentAction
        2 -> AEFKDocumentNamePrefix
        3 -> AEFKExecutionId
        4 -> AEFKExecutionStatus
        5 -> AEFKParentExecutionId
        6 -> AEFKStartTimeAfter
        7 -> AEFKStartTimeBefore
        8 -> AEFKTagKey
        _ -> (error . showText) $ "Unknown index for AutomationExecutionFilterKey: " <> toText i
    fromEnum x = case x of
        AEFKAutomationType -> 0
        AEFKCurrentAction -> 1
        AEFKDocumentNamePrefix -> 2
        AEFKExecutionId -> 3
        AEFKExecutionStatus -> 4
        AEFKParentExecutionId -> 5
        AEFKStartTimeAfter -> 6
        AEFKStartTimeBefore -> 7
        AEFKTagKey -> 8
        AutomationExecutionFilterKey' name -> (error . showText) $ "Unknown AutomationExecutionFilterKey: " <> original name

-- | Represents the bounds of /known/ $AutomationExecutionFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutomationExecutionFilterKey where
    minBound = AEFKAutomationType
    maxBound = AEFKTagKey

instance Hashable     AutomationExecutionFilterKey
instance NFData       AutomationExecutionFilterKey
instance ToByteString AutomationExecutionFilterKey
instance ToQuery      AutomationExecutionFilterKey
instance ToHeader     AutomationExecutionFilterKey

instance ToJSON AutomationExecutionFilterKey where
    toJSON = toJSONText
