{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceState (
  WorkspaceState (
    ..
    , WSAdminMaintenance
    , WSAvailable
    , WSError'
    , WSImpaired
    , WSMaintenance
    , WSPending
    , WSRebooting
    , WSRebuilding
    , WSRestoring
    , WSStarting
    , WSStopped
    , WSStopping
    , WSSuspended
    , WSTerminated
    , WSTerminating
    , WSUnhealthy
    , WSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkspaceState = WorkspaceState' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern WSAdminMaintenance :: WorkspaceState
pattern WSAdminMaintenance = WorkspaceState' "ADMIN_MAINTENANCE"

pattern WSAvailable :: WorkspaceState
pattern WSAvailable = WorkspaceState' "AVAILABLE"

pattern WSError' :: WorkspaceState
pattern WSError' = WorkspaceState' "ERROR"

pattern WSImpaired :: WorkspaceState
pattern WSImpaired = WorkspaceState' "IMPAIRED"

pattern WSMaintenance :: WorkspaceState
pattern WSMaintenance = WorkspaceState' "MAINTENANCE"

pattern WSPending :: WorkspaceState
pattern WSPending = WorkspaceState' "PENDING"

pattern WSRebooting :: WorkspaceState
pattern WSRebooting = WorkspaceState' "REBOOTING"

pattern WSRebuilding :: WorkspaceState
pattern WSRebuilding = WorkspaceState' "REBUILDING"

pattern WSRestoring :: WorkspaceState
pattern WSRestoring = WorkspaceState' "RESTORING"

pattern WSStarting :: WorkspaceState
pattern WSStarting = WorkspaceState' "STARTING"

pattern WSStopped :: WorkspaceState
pattern WSStopped = WorkspaceState' "STOPPED"

pattern WSStopping :: WorkspaceState
pattern WSStopping = WorkspaceState' "STOPPING"

pattern WSSuspended :: WorkspaceState
pattern WSSuspended = WorkspaceState' "SUSPENDED"

pattern WSTerminated :: WorkspaceState
pattern WSTerminated = WorkspaceState' "TERMINATED"

pattern WSTerminating :: WorkspaceState
pattern WSTerminating = WorkspaceState' "TERMINATING"

pattern WSUnhealthy :: WorkspaceState
pattern WSUnhealthy = WorkspaceState' "UNHEALTHY"

pattern WSUpdating :: WorkspaceState
pattern WSUpdating = WorkspaceState' "UPDATING"

{-# COMPLETE
  WSAdminMaintenance,
  WSAvailable,
  WSError',
  WSImpaired,
  WSMaintenance,
  WSPending,
  WSRebooting,
  WSRebuilding,
  WSRestoring,
  WSStarting,
  WSStopped,
  WSStopping,
  WSSuspended,
  WSTerminated,
  WSTerminating,
  WSUnhealthy,
  WSUpdating,
  WorkspaceState' #-}

instance FromText WorkspaceState where
    parser = (WorkspaceState' . mk) <$> takeText

instance ToText WorkspaceState where
    toText (WorkspaceState' ci) = original ci

-- | Represents an enum of /known/ $WorkspaceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkspaceState where
    toEnum i = case i of
        0 -> WSAdminMaintenance
        1 -> WSAvailable
        2 -> WSError'
        3 -> WSImpaired
        4 -> WSMaintenance
        5 -> WSPending
        6 -> WSRebooting
        7 -> WSRebuilding
        8 -> WSRestoring
        9 -> WSStarting
        10 -> WSStopped
        11 -> WSStopping
        12 -> WSSuspended
        13 -> WSTerminated
        14 -> WSTerminating
        15 -> WSUnhealthy
        16 -> WSUpdating
        _ -> (error . showText) $ "Unknown index for WorkspaceState: " <> toText i
    fromEnum x = case x of
        WSAdminMaintenance -> 0
        WSAvailable -> 1
        WSError' -> 2
        WSImpaired -> 3
        WSMaintenance -> 4
        WSPending -> 5
        WSRebooting -> 6
        WSRebuilding -> 7
        WSRestoring -> 8
        WSStarting -> 9
        WSStopped -> 10
        WSStopping -> 11
        WSSuspended -> 12
        WSTerminated -> 13
        WSTerminating -> 14
        WSUnhealthy -> 15
        WSUpdating -> 16
        WorkspaceState' name -> (error . showText) $ "Unknown WorkspaceState: " <> original name

-- | Represents the bounds of /known/ $WorkspaceState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkspaceState where
    minBound = WSAdminMaintenance
    maxBound = WSUpdating

instance Hashable     WorkspaceState
instance NFData       WorkspaceState
instance ToByteString WorkspaceState
instance ToQuery      WorkspaceState
instance ToHeader     WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"
