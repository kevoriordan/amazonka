{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPluginStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandPluginStatus (
  CommandPluginStatus (
    ..
    , CPSCancelled
    , CPSFailed
    , CPSInProgress
    , CPSPending
    , CPSSuccess
    , CPSTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CommandPluginStatus = CommandPluginStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern CPSCancelled :: CommandPluginStatus
pattern CPSCancelled = CommandPluginStatus' "Cancelled"

pattern CPSFailed :: CommandPluginStatus
pattern CPSFailed = CommandPluginStatus' "Failed"

pattern CPSInProgress :: CommandPluginStatus
pattern CPSInProgress = CommandPluginStatus' "InProgress"

pattern CPSPending :: CommandPluginStatus
pattern CPSPending = CommandPluginStatus' "Pending"

pattern CPSSuccess :: CommandPluginStatus
pattern CPSSuccess = CommandPluginStatus' "Success"

pattern CPSTimedOut :: CommandPluginStatus
pattern CPSTimedOut = CommandPluginStatus' "TimedOut"

{-# COMPLETE
  CPSCancelled,
  CPSFailed,
  CPSInProgress,
  CPSPending,
  CPSSuccess,
  CPSTimedOut,
  CommandPluginStatus' #-}

instance FromText CommandPluginStatus where
    parser = (CommandPluginStatus' . mk) <$> takeText

instance ToText CommandPluginStatus where
    toText (CommandPluginStatus' ci) = original ci

-- | Represents an enum of /known/ $CommandPluginStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CommandPluginStatus where
    toEnum i = case i of
        0 -> CPSCancelled
        1 -> CPSFailed
        2 -> CPSInProgress
        3 -> CPSPending
        4 -> CPSSuccess
        5 -> CPSTimedOut
        _ -> (error . showText) $ "Unknown index for CommandPluginStatus: " <> toText i
    fromEnum x = case x of
        CPSCancelled -> 0
        CPSFailed -> 1
        CPSInProgress -> 2
        CPSPending -> 3
        CPSSuccess -> 4
        CPSTimedOut -> 5
        CommandPluginStatus' name -> (error . showText) $ "Unknown CommandPluginStatus: " <> original name

-- | Represents the bounds of /known/ $CommandPluginStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CommandPluginStatus where
    minBound = CPSCancelled
    maxBound = CPSTimedOut

instance Hashable     CommandPluginStatus
instance NFData       CommandPluginStatus
instance ToByteString CommandPluginStatus
instance ToQuery      CommandPluginStatus
instance ToHeader     CommandPluginStatus

instance FromJSON CommandPluginStatus where
    parseJSON = parseJSONText "CommandPluginStatus"
