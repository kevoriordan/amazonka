{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationJobState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationJobState (
  ReplicationJobState (
    ..
    , RJSActive
    , RJSCompleted
    , RJSDeleted
    , RJSDeleting
    , RJSFailed
    , RJSFailing
    , RJSPausedOnFailure
    , RJSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicationJobState = ReplicationJobState' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern RJSActive :: ReplicationJobState
pattern RJSActive = ReplicationJobState' "ACTIVE"

pattern RJSCompleted :: ReplicationJobState
pattern RJSCompleted = ReplicationJobState' "COMPLETED"

pattern RJSDeleted :: ReplicationJobState
pattern RJSDeleted = ReplicationJobState' "DELETED"

pattern RJSDeleting :: ReplicationJobState
pattern RJSDeleting = ReplicationJobState' "DELETING"

pattern RJSFailed :: ReplicationJobState
pattern RJSFailed = ReplicationJobState' "FAILED"

pattern RJSFailing :: ReplicationJobState
pattern RJSFailing = ReplicationJobState' "FAILING"

pattern RJSPausedOnFailure :: ReplicationJobState
pattern RJSPausedOnFailure = ReplicationJobState' "PAUSED_ON_FAILURE"

pattern RJSPending :: ReplicationJobState
pattern RJSPending = ReplicationJobState' "PENDING"

{-# COMPLETE
  RJSActive,
  RJSCompleted,
  RJSDeleted,
  RJSDeleting,
  RJSFailed,
  RJSFailing,
  RJSPausedOnFailure,
  RJSPending,
  ReplicationJobState' #-}

instance FromText ReplicationJobState where
    parser = (ReplicationJobState' . mk) <$> takeText

instance ToText ReplicationJobState where
    toText (ReplicationJobState' ci) = original ci

-- | Represents an enum of /known/ $ReplicationJobState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationJobState where
    toEnum i = case i of
        0 -> RJSActive
        1 -> RJSCompleted
        2 -> RJSDeleted
        3 -> RJSDeleting
        4 -> RJSFailed
        5 -> RJSFailing
        6 -> RJSPausedOnFailure
        7 -> RJSPending
        _ -> (error . showText) $ "Unknown index for ReplicationJobState: " <> toText i
    fromEnum x = case x of
        RJSActive -> 0
        RJSCompleted -> 1
        RJSDeleted -> 2
        RJSDeleting -> 3
        RJSFailed -> 4
        RJSFailing -> 5
        RJSPausedOnFailure -> 6
        RJSPending -> 7
        ReplicationJobState' name -> (error . showText) $ "Unknown ReplicationJobState: " <> original name

-- | Represents the bounds of /known/ $ReplicationJobState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationJobState where
    minBound = RJSActive
    maxBound = RJSPending

instance Hashable     ReplicationJobState
instance NFData       ReplicationJobState
instance ToByteString ReplicationJobState
instance ToQuery      ReplicationJobState
instance ToHeader     ReplicationJobState

instance FromJSON ReplicationJobState where
    parseJSON = parseJSONText "ReplicationJobState"
