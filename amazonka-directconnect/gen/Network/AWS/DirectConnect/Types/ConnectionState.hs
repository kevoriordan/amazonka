{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.ConnectionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.ConnectionState (
  ConnectionState (
    ..
    , CSAvailable
    , CSDeleted
    , CSDeleting
    , CSDown
    , CSOrdering
    , CSPending
    , CSRejected
    , CSRequested
    , CSUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionState = ConnectionState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CSAvailable :: ConnectionState
pattern CSAvailable = ConnectionState' "available"

pattern CSDeleted :: ConnectionState
pattern CSDeleted = ConnectionState' "deleted"

pattern CSDeleting :: ConnectionState
pattern CSDeleting = ConnectionState' "deleting"

pattern CSDown :: ConnectionState
pattern CSDown = ConnectionState' "down"

pattern CSOrdering :: ConnectionState
pattern CSOrdering = ConnectionState' "ordering"

pattern CSPending :: ConnectionState
pattern CSPending = ConnectionState' "pending"

pattern CSRejected :: ConnectionState
pattern CSRejected = ConnectionState' "rejected"

pattern CSRequested :: ConnectionState
pattern CSRequested = ConnectionState' "requested"

pattern CSUnknown :: ConnectionState
pattern CSUnknown = ConnectionState' "unknown"

{-# COMPLETE
  CSAvailable,
  CSDeleted,
  CSDeleting,
  CSDown,
  CSOrdering,
  CSPending,
  CSRejected,
  CSRequested,
  CSUnknown,
  ConnectionState' #-}

instance FromText ConnectionState where
    parser = (ConnectionState' . mk) <$> takeText

instance ToText ConnectionState where
    toText (ConnectionState' ci) = original ci

-- | Represents an enum of /known/ $ConnectionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionState where
    toEnum i = case i of
        0 -> CSAvailable
        1 -> CSDeleted
        2 -> CSDeleting
        3 -> CSDown
        4 -> CSOrdering
        5 -> CSPending
        6 -> CSRejected
        7 -> CSRequested
        8 -> CSUnknown
        _ -> (error . showText) $ "Unknown index for ConnectionState: " <> toText i
    fromEnum x = case x of
        CSAvailable -> 0
        CSDeleted -> 1
        CSDeleting -> 2
        CSDown -> 3
        CSOrdering -> 4
        CSPending -> 5
        CSRejected -> 6
        CSRequested -> 7
        CSUnknown -> 8
        ConnectionState' name -> (error . showText) $ "Unknown ConnectionState: " <> original name

-- | Represents the bounds of /known/ $ConnectionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionState where
    minBound = CSAvailable
    maxBound = CSUnknown

instance Hashable     ConnectionState
instance NFData       ConnectionState
instance ToByteString ConnectionState
instance ToQuery      ConnectionState
instance ToHeader     ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"
