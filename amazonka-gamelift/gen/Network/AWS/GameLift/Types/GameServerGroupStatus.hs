{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroupStatus (
  GameServerGroupStatus (
    ..
    , GSGSActivating
    , GSGSActive
    , GSGSDeleteScheduled
    , GSGSDeleted
    , GSGSDeleting
    , GSGSError'
    , GSGSNew
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameServerGroupStatus = GameServerGroupStatus' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern GSGSActivating :: GameServerGroupStatus
pattern GSGSActivating = GameServerGroupStatus' "ACTIVATING"

pattern GSGSActive :: GameServerGroupStatus
pattern GSGSActive = GameServerGroupStatus' "ACTIVE"

pattern GSGSDeleteScheduled :: GameServerGroupStatus
pattern GSGSDeleteScheduled = GameServerGroupStatus' "DELETE_SCHEDULED"

pattern GSGSDeleted :: GameServerGroupStatus
pattern GSGSDeleted = GameServerGroupStatus' "DELETED"

pattern GSGSDeleting :: GameServerGroupStatus
pattern GSGSDeleting = GameServerGroupStatus' "DELETING"

pattern GSGSError' :: GameServerGroupStatus
pattern GSGSError' = GameServerGroupStatus' "ERROR"

pattern GSGSNew :: GameServerGroupStatus
pattern GSGSNew = GameServerGroupStatus' "NEW"

{-# COMPLETE
  GSGSActivating,
  GSGSActive,
  GSGSDeleteScheduled,
  GSGSDeleted,
  GSGSDeleting,
  GSGSError',
  GSGSNew,
  GameServerGroupStatus' #-}

instance FromText GameServerGroupStatus where
    parser = (GameServerGroupStatus' . mk) <$> takeText

instance ToText GameServerGroupStatus where
    toText (GameServerGroupStatus' ci) = original ci

-- | Represents an enum of /known/ $GameServerGroupStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameServerGroupStatus where
    toEnum i = case i of
        0 -> GSGSActivating
        1 -> GSGSActive
        2 -> GSGSDeleteScheduled
        3 -> GSGSDeleted
        4 -> GSGSDeleting
        5 -> GSGSError'
        6 -> GSGSNew
        _ -> (error . showText) $ "Unknown index for GameServerGroupStatus: " <> toText i
    fromEnum x = case x of
        GSGSActivating -> 0
        GSGSActive -> 1
        GSGSDeleteScheduled -> 2
        GSGSDeleted -> 3
        GSGSDeleting -> 4
        GSGSError' -> 5
        GSGSNew -> 6
        GameServerGroupStatus' name -> (error . showText) $ "Unknown GameServerGroupStatus: " <> original name

-- | Represents the bounds of /known/ $GameServerGroupStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameServerGroupStatus where
    minBound = GSGSActivating
    maxBound = GSGSNew

instance Hashable     GameServerGroupStatus
instance NFData       GameServerGroupStatus
instance ToByteString GameServerGroupStatus
instance ToQuery      GameServerGroupStatus
instance ToHeader     GameServerGroupStatus

instance FromJSON GameServerGroupStatus where
    parseJSON = parseJSONText "GameServerGroupStatus"
