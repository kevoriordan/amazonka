{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AutomationExecutionStatus (
  AutomationExecutionStatus (
    ..
    , AESCancelled
    , AESCancelling
    , AESFailed
    , AESInProgress
    , AESPending
    , AESSuccess
    , AESTimedOut
    , AESWaiting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutomationExecutionStatus = AutomationExecutionStatus' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AESCancelled :: AutomationExecutionStatus
pattern AESCancelled = AutomationExecutionStatus' "Cancelled"

pattern AESCancelling :: AutomationExecutionStatus
pattern AESCancelling = AutomationExecutionStatus' "Cancelling"

pattern AESFailed :: AutomationExecutionStatus
pattern AESFailed = AutomationExecutionStatus' "Failed"

pattern AESInProgress :: AutomationExecutionStatus
pattern AESInProgress = AutomationExecutionStatus' "InProgress"

pattern AESPending :: AutomationExecutionStatus
pattern AESPending = AutomationExecutionStatus' "Pending"

pattern AESSuccess :: AutomationExecutionStatus
pattern AESSuccess = AutomationExecutionStatus' "Success"

pattern AESTimedOut :: AutomationExecutionStatus
pattern AESTimedOut = AutomationExecutionStatus' "TimedOut"

pattern AESWaiting :: AutomationExecutionStatus
pattern AESWaiting = AutomationExecutionStatus' "Waiting"

{-# COMPLETE
  AESCancelled,
  AESCancelling,
  AESFailed,
  AESInProgress,
  AESPending,
  AESSuccess,
  AESTimedOut,
  AESWaiting,
  AutomationExecutionStatus' #-}

instance FromText AutomationExecutionStatus where
    parser = (AutomationExecutionStatus' . mk) <$> takeText

instance ToText AutomationExecutionStatus where
    toText (AutomationExecutionStatus' ci) = original ci

-- | Represents an enum of /known/ $AutomationExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutomationExecutionStatus where
    toEnum i = case i of
        0 -> AESCancelled
        1 -> AESCancelling
        2 -> AESFailed
        3 -> AESInProgress
        4 -> AESPending
        5 -> AESSuccess
        6 -> AESTimedOut
        7 -> AESWaiting
        _ -> (error . showText) $ "Unknown index for AutomationExecutionStatus: " <> toText i
    fromEnum x = case x of
        AESCancelled -> 0
        AESCancelling -> 1
        AESFailed -> 2
        AESInProgress -> 3
        AESPending -> 4
        AESSuccess -> 5
        AESTimedOut -> 6
        AESWaiting -> 7
        AutomationExecutionStatus' name -> (error . showText) $ "Unknown AutomationExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $AutomationExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutomationExecutionStatus where
    minBound = AESCancelled
    maxBound = AESWaiting

instance Hashable     AutomationExecutionStatus
instance NFData       AutomationExecutionStatus
instance ToByteString AutomationExecutionStatus
instance ToQuery      AutomationExecutionStatus
instance ToHeader     AutomationExecutionStatus

instance FromJSON AutomationExecutionStatus where
    parseJSON = parseJSONText "AutomationExecutionStatus"
