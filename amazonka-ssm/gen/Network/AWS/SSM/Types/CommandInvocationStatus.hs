{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandInvocationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandInvocationStatus (
  CommandInvocationStatus (
    ..
    , CISCancelled
    , CISCancelling
    , CISDelayed
    , CISFailed
    , CISInProgress
    , CISPending
    , CISSuccess
    , CISTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CommandInvocationStatus = CommandInvocationStatus' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern CISCancelled :: CommandInvocationStatus
pattern CISCancelled = CommandInvocationStatus' "Cancelled"

pattern CISCancelling :: CommandInvocationStatus
pattern CISCancelling = CommandInvocationStatus' "Cancelling"

pattern CISDelayed :: CommandInvocationStatus
pattern CISDelayed = CommandInvocationStatus' "Delayed"

pattern CISFailed :: CommandInvocationStatus
pattern CISFailed = CommandInvocationStatus' "Failed"

pattern CISInProgress :: CommandInvocationStatus
pattern CISInProgress = CommandInvocationStatus' "InProgress"

pattern CISPending :: CommandInvocationStatus
pattern CISPending = CommandInvocationStatus' "Pending"

pattern CISSuccess :: CommandInvocationStatus
pattern CISSuccess = CommandInvocationStatus' "Success"

pattern CISTimedOut :: CommandInvocationStatus
pattern CISTimedOut = CommandInvocationStatus' "TimedOut"

{-# COMPLETE
  CISCancelled,
  CISCancelling,
  CISDelayed,
  CISFailed,
  CISInProgress,
  CISPending,
  CISSuccess,
  CISTimedOut,
  CommandInvocationStatus' #-}

instance FromText CommandInvocationStatus where
    parser = (CommandInvocationStatus' . mk) <$> takeText

instance ToText CommandInvocationStatus where
    toText (CommandInvocationStatus' ci) = original ci

-- | Represents an enum of /known/ $CommandInvocationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CommandInvocationStatus where
    toEnum i = case i of
        0 -> CISCancelled
        1 -> CISCancelling
        2 -> CISDelayed
        3 -> CISFailed
        4 -> CISInProgress
        5 -> CISPending
        6 -> CISSuccess
        7 -> CISTimedOut
        _ -> (error . showText) $ "Unknown index for CommandInvocationStatus: " <> toText i
    fromEnum x = case x of
        CISCancelled -> 0
        CISCancelling -> 1
        CISDelayed -> 2
        CISFailed -> 3
        CISInProgress -> 4
        CISPending -> 5
        CISSuccess -> 6
        CISTimedOut -> 7
        CommandInvocationStatus' name -> (error . showText) $ "Unknown CommandInvocationStatus: " <> original name

-- | Represents the bounds of /known/ $CommandInvocationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CommandInvocationStatus where
    minBound = CISCancelled
    maxBound = CISTimedOut

instance Hashable     CommandInvocationStatus
instance NFData       CommandInvocationStatus
instance ToByteString CommandInvocationStatus
instance ToQuery      CommandInvocationStatus
instance ToHeader     CommandInvocationStatus

instance FromJSON CommandInvocationStatus where
    parseJSON = parseJSONText "CommandInvocationStatus"
