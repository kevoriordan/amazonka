{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus (
  MaintenanceWindowExecutionStatus (
    ..
    , MWESCancelled
    , MWESCancelling
    , MWESFailed
    , MWESInProgress
    , MWESPending
    , MWESSkippedOverlapping
    , MWESSuccess
    , MWESTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern MWESCancelled :: MaintenanceWindowExecutionStatus
pattern MWESCancelled = MaintenanceWindowExecutionStatus' "CANCELLED"

pattern MWESCancelling :: MaintenanceWindowExecutionStatus
pattern MWESCancelling = MaintenanceWindowExecutionStatus' "CANCELLING"

pattern MWESFailed :: MaintenanceWindowExecutionStatus
pattern MWESFailed = MaintenanceWindowExecutionStatus' "FAILED"

pattern MWESInProgress :: MaintenanceWindowExecutionStatus
pattern MWESInProgress = MaintenanceWindowExecutionStatus' "IN_PROGRESS"

pattern MWESPending :: MaintenanceWindowExecutionStatus
pattern MWESPending = MaintenanceWindowExecutionStatus' "PENDING"

pattern MWESSkippedOverlapping :: MaintenanceWindowExecutionStatus
pattern MWESSkippedOverlapping = MaintenanceWindowExecutionStatus' "SKIPPED_OVERLAPPING"

pattern MWESSuccess :: MaintenanceWindowExecutionStatus
pattern MWESSuccess = MaintenanceWindowExecutionStatus' "SUCCESS"

pattern MWESTimedOut :: MaintenanceWindowExecutionStatus
pattern MWESTimedOut = MaintenanceWindowExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  MWESCancelled,
  MWESCancelling,
  MWESFailed,
  MWESInProgress,
  MWESPending,
  MWESSkippedOverlapping,
  MWESSuccess,
  MWESTimedOut,
  MaintenanceWindowExecutionStatus' #-}

instance FromText MaintenanceWindowExecutionStatus where
    parser = (MaintenanceWindowExecutionStatus' . mk) <$> takeText

instance ToText MaintenanceWindowExecutionStatus where
    toText (MaintenanceWindowExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $MaintenanceWindowExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MaintenanceWindowExecutionStatus where
    toEnum i = case i of
        0 -> MWESCancelled
        1 -> MWESCancelling
        2 -> MWESFailed
        3 -> MWESInProgress
        4 -> MWESPending
        5 -> MWESSkippedOverlapping
        6 -> MWESSuccess
        7 -> MWESTimedOut
        _ -> (error . showText) $ "Unknown index for MaintenanceWindowExecutionStatus: " <> toText i
    fromEnum x = case x of
        MWESCancelled -> 0
        MWESCancelling -> 1
        MWESFailed -> 2
        MWESInProgress -> 3
        MWESPending -> 4
        MWESSkippedOverlapping -> 5
        MWESSuccess -> 6
        MWESTimedOut -> 7
        MaintenanceWindowExecutionStatus' name -> (error . showText) $ "Unknown MaintenanceWindowExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $MaintenanceWindowExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MaintenanceWindowExecutionStatus where
    minBound = MWESCancelled
    maxBound = MWESTimedOut

instance Hashable     MaintenanceWindowExecutionStatus
instance NFData       MaintenanceWindowExecutionStatus
instance ToByteString MaintenanceWindowExecutionStatus
instance ToQuery      MaintenanceWindowExecutionStatus
instance ToHeader     MaintenanceWindowExecutionStatus

instance FromJSON MaintenanceWindowExecutionStatus where
    parseJSON = parseJSONText "MaintenanceWindowExecutionStatus"
