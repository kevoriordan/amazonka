{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.ExecutionStatus (
  ExecutionStatus (
    ..
    , Aborted
    , Failed
    , Running
    , Succeeded
    , TimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExecutionStatus = ExecutionStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Aborted :: ExecutionStatus
pattern Aborted = ExecutionStatus' "ABORTED"

pattern Failed :: ExecutionStatus
pattern Failed = ExecutionStatus' "FAILED"

pattern Running :: ExecutionStatus
pattern Running = ExecutionStatus' "RUNNING"

pattern Succeeded :: ExecutionStatus
pattern Succeeded = ExecutionStatus' "SUCCEEDED"

pattern TimedOut :: ExecutionStatus
pattern TimedOut = ExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  Aborted,
  Failed,
  Running,
  Succeeded,
  TimedOut,
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
        0 -> Aborted
        1 -> Failed
        2 -> Running
        3 -> Succeeded
        4 -> TimedOut
        _ -> (error . showText) $ "Unknown index for ExecutionStatus: " <> toText i
    fromEnum x = case x of
        Aborted -> 0
        Failed -> 1
        Running -> 2
        Succeeded -> 3
        TimedOut -> 4
        ExecutionStatus' name -> (error . showText) $ "Unknown ExecutionStatus: " <> original name

-- | Represents the bounds of /known/ $ExecutionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExecutionStatus where
    minBound = Aborted
    maxBound = TimedOut

instance Hashable     ExecutionStatus
instance NFData       ExecutionStatus
instance ToByteString ExecutionStatus
instance ToQuery      ExecutionStatus
instance ToHeader     ExecutionStatus

instance ToJSON ExecutionStatus where
    toJSON = toJSONText

instance FromJSON ExecutionStatus where
    parseJSON = parseJSONText "ExecutionStatus"
