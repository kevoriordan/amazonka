{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UpdateActionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.UpdateActionStatus (
  UpdateActionStatus (
    ..
    , Complete
    , InProgress
    , NotApplied
    , Stopped
    , Stopping
    , WaitingToStart
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UpdateActionStatus = UpdateActionStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Complete :: UpdateActionStatus
pattern Complete = UpdateActionStatus' "complete"

pattern InProgress :: UpdateActionStatus
pattern InProgress = UpdateActionStatus' "in-progress"

pattern NotApplied :: UpdateActionStatus
pattern NotApplied = UpdateActionStatus' "not-applied"

pattern Stopped :: UpdateActionStatus
pattern Stopped = UpdateActionStatus' "stopped"

pattern Stopping :: UpdateActionStatus
pattern Stopping = UpdateActionStatus' "stopping"

pattern WaitingToStart :: UpdateActionStatus
pattern WaitingToStart = UpdateActionStatus' "waiting-to-start"

{-# COMPLETE
  Complete,
  InProgress,
  NotApplied,
  Stopped,
  Stopping,
  WaitingToStart,
  UpdateActionStatus' #-}

instance FromText UpdateActionStatus where
    parser = (UpdateActionStatus' . mk) <$> takeText

instance ToText UpdateActionStatus where
    toText (UpdateActionStatus' ci) = original ci

-- | Represents an enum of /known/ $UpdateActionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UpdateActionStatus where
    toEnum i = case i of
        0 -> Complete
        1 -> InProgress
        2 -> NotApplied
        3 -> Stopped
        4 -> Stopping
        5 -> WaitingToStart
        _ -> (error . showText) $ "Unknown index for UpdateActionStatus: " <> toText i
    fromEnum x = case x of
        Complete -> 0
        InProgress -> 1
        NotApplied -> 2
        Stopped -> 3
        Stopping -> 4
        WaitingToStart -> 5
        UpdateActionStatus' name -> (error . showText) $ "Unknown UpdateActionStatus: " <> original name

-- | Represents the bounds of /known/ $UpdateActionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UpdateActionStatus where
    minBound = Complete
    maxBound = WaitingToStart

instance Hashable     UpdateActionStatus
instance NFData       UpdateActionStatus
instance ToByteString UpdateActionStatus
instance ToQuery      UpdateActionStatus
instance ToHeader     UpdateActionStatus

instance FromXML UpdateActionStatus where
    parseXML = parseXMLText "UpdateActionStatus"
