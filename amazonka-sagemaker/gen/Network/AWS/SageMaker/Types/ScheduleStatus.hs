{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ScheduleStatus (
  ScheduleStatus (
    ..
    , SSFailed
    , SSPending
    , SSScheduled
    , SSStopped
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScheduleStatus = ScheduleStatus' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern SSFailed :: ScheduleStatus
pattern SSFailed = ScheduleStatus' "Failed"

pattern SSPending :: ScheduleStatus
pattern SSPending = ScheduleStatus' "Pending"

pattern SSScheduled :: ScheduleStatus
pattern SSScheduled = ScheduleStatus' "Scheduled"

pattern SSStopped :: ScheduleStatus
pattern SSStopped = ScheduleStatus' "Stopped"

{-# COMPLETE
  SSFailed,
  SSPending,
  SSScheduled,
  SSStopped,
  ScheduleStatus' #-}

instance FromText ScheduleStatus where
    parser = (ScheduleStatus' . mk) <$> takeText

instance ToText ScheduleStatus where
    toText (ScheduleStatus' ci) = original ci

-- | Represents an enum of /known/ $ScheduleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScheduleStatus where
    toEnum i = case i of
        0 -> SSFailed
        1 -> SSPending
        2 -> SSScheduled
        3 -> SSStopped
        _ -> (error . showText) $ "Unknown index for ScheduleStatus: " <> toText i
    fromEnum x = case x of
        SSFailed -> 0
        SSPending -> 1
        SSScheduled -> 2
        SSStopped -> 3
        ScheduleStatus' name -> (error . showText) $ "Unknown ScheduleStatus: " <> original name

-- | Represents the bounds of /known/ $ScheduleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScheduleStatus where
    minBound = SSFailed
    maxBound = SSStopped

instance Hashable     ScheduleStatus
instance NFData       ScheduleStatus
instance ToByteString ScheduleStatus
instance ToQuery      ScheduleStatus
instance ToHeader     ScheduleStatus

instance ToJSON ScheduleStatus where
    toJSON = toJSONText

instance FromJSON ScheduleStatus where
    parseJSON = parseJSONText "ScheduleStatus"
