{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ExecutionStatus (
  ExecutionStatus (
    ..
    , ECompleted
    , ECompletedWithViolations
    , EFailed
    , EInProgress
    , EPending
    , EStopped
    , EStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExecutionStatus = ExecutionStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ECompleted :: ExecutionStatus
pattern ECompleted = ExecutionStatus' "Completed"

pattern ECompletedWithViolations :: ExecutionStatus
pattern ECompletedWithViolations = ExecutionStatus' "CompletedWithViolations"

pattern EFailed :: ExecutionStatus
pattern EFailed = ExecutionStatus' "Failed"

pattern EInProgress :: ExecutionStatus
pattern EInProgress = ExecutionStatus' "InProgress"

pattern EPending :: ExecutionStatus
pattern EPending = ExecutionStatus' "Pending"

pattern EStopped :: ExecutionStatus
pattern EStopped = ExecutionStatus' "Stopped"

pattern EStopping :: ExecutionStatus
pattern EStopping = ExecutionStatus' "Stopping"

{-# COMPLETE
  ECompleted,
  ECompletedWithViolations,
  EFailed,
  EInProgress,
  EPending,
  EStopped,
  EStopping,
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
        0 -> ECompleted
        1 -> ECompletedWithViolations
        2 -> EFailed
        3 -> EInProgress
        4 -> EPending
        5 -> EStopped
        6 -> EStopping
        _ -> (error . showText) $ "Unknown index for ExecutionStatus: " <> toText i
    fromEnum x = case x of
        ECompleted -> 0
        ECompletedWithViolations -> 1
        EFailed -> 2
        EInProgress -> 3
        EPending -> 4
        EStopped -> 5
        EStopping -> 6
        ExecutionStatus' name -> (error . showText) $ "Unknown ExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExecutionStatus where
    minBound = ECompleted
    maxBound = EStopping

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance ToJSON ExecutionStatus where
    toJSON = toJSONText

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"
