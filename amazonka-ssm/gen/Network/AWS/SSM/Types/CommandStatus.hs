{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandStatus (
  CommandStatus (
    ..
    , CSCancelled
    , CSCancelling
    , CSFailed
    , CSInProgress
    , CSPending
    , CSSuccess
    , CSTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CommandStatus = CommandStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CSCancelled :: CommandStatus
pattern CSCancelled = CommandStatus' "Cancelled"

pattern CSCancelling :: CommandStatus
pattern CSCancelling = CommandStatus' "Cancelling"

pattern CSFailed :: CommandStatus
pattern CSFailed = CommandStatus' "Failed"

pattern CSInProgress :: CommandStatus
pattern CSInProgress = CommandStatus' "InProgress"

pattern CSPending :: CommandStatus
pattern CSPending = CommandStatus' "Pending"

pattern CSSuccess :: CommandStatus
pattern CSSuccess = CommandStatus' "Success"

pattern CSTimedOut :: CommandStatus
pattern CSTimedOut = CommandStatus' "TimedOut"

{-# COMPLETE
  CSCancelled,
  CSCancelling,
  CSFailed,
  CSInProgress,
  CSPending,
  CSSuccess,
  CSTimedOut,
  CommandStatus' #-}

instance FromText CommandStatus where
    parser = (CommandStatus' . mk) <$> takeText

instance ToText CommandStatus where
    toText (CommandStatus' ci) = original ci

-- | Represents an enum of /known/ $CommandStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CommandStatus where
    toEnum i = case i of
        0 -> CSCancelled
        1 -> CSCancelling
        2 -> CSFailed
        3 -> CSInProgress
        4 -> CSPending
        5 -> CSSuccess
        6 -> CSTimedOut
        _ -> (error . showText) $ "Unknown index for CommandStatus: " <> toText i
    fromEnum x = case x of
        CSCancelled -> 0
        CSCancelling -> 1
        CSFailed -> 2
        CSInProgress -> 3
        CSPending -> 4
        CSSuccess -> 5
        CSTimedOut -> 6
        CommandStatus' name -> (error . showText) $ "Unknown CommandStatus: " <> original name

-- | Represents the bounds of /known/ $CommandStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CommandStatus where
    minBound = CSCancelled
    maxBound = CSTimedOut

instance Hashable     CommandStatus
instance NFData       CommandStatus
instance ToByteString CommandStatus
instance ToQuery      CommandStatus
instance ToHeader     CommandStatus

instance FromJSON CommandStatus where
    parseJSON = parseJSONText "CommandStatus"
