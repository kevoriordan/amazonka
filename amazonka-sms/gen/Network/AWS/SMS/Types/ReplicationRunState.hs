{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationRunState (
  ReplicationRunState (
    ..
    , RRSActive
    , RRSCompleted
    , RRSDeleted
    , RRSDeleting
    , RRSFailed
    , RRSMissed
    , RRSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicationRunState = ReplicationRunState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern RRSActive :: ReplicationRunState
pattern RRSActive = ReplicationRunState' "ACTIVE"

pattern RRSCompleted :: ReplicationRunState
pattern RRSCompleted = ReplicationRunState' "COMPLETED"

pattern RRSDeleted :: ReplicationRunState
pattern RRSDeleted = ReplicationRunState' "DELETED"

pattern RRSDeleting :: ReplicationRunState
pattern RRSDeleting = ReplicationRunState' "DELETING"

pattern RRSFailed :: ReplicationRunState
pattern RRSFailed = ReplicationRunState' "FAILED"

pattern RRSMissed :: ReplicationRunState
pattern RRSMissed = ReplicationRunState' "MISSED"

pattern RRSPending :: ReplicationRunState
pattern RRSPending = ReplicationRunState' "PENDING"

{-# COMPLETE
  RRSActive,
  RRSCompleted,
  RRSDeleted,
  RRSDeleting,
  RRSFailed,
  RRSMissed,
  RRSPending,
  ReplicationRunState' #-}

instance FromText ReplicationRunState where
    parser = (ReplicationRunState' . mk) <$> takeText

instance ToText ReplicationRunState where
    toText (ReplicationRunState' ci) = original ci

-- | Represents an enum of /known/ $ReplicationRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationRunState where
    toEnum i = case i of
        0 -> RRSActive
        1 -> RRSCompleted
        2 -> RRSDeleted
        3 -> RRSDeleting
        4 -> RRSFailed
        5 -> RRSMissed
        6 -> RRSPending
        _ -> (error . showText) $ "Unknown index for ReplicationRunState: " <> toText i
    fromEnum x = case x of
        RRSActive -> 0
        RRSCompleted -> 1
        RRSDeleted -> 2
        RRSDeleting -> 3
        RRSFailed -> 4
        RRSMissed -> 5
        RRSPending -> 6
        ReplicationRunState' name -> (error . showText) $ "Unknown ReplicationRunState: " <> original name

-- | Represents the bounds of /known/ $ReplicationRunState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationRunState where
    minBound = RRSActive
    maxBound = RRSPending

instance Hashable     ReplicationRunState
instance NFData       ReplicationRunState
instance ToByteString ReplicationRunState
instance ToQuery      ReplicationRunState
instance ToHeader     ReplicationRunState

instance FromJSON ReplicationRunState where
    parseJSON = parseJSONText "ReplicationRunState"
