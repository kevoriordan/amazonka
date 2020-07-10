{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ConnectionState (
  ConnectionState (
    ..
    , Connected
    , Disconnected
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionState = ConnectionState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Connected :: ConnectionState
pattern Connected = ConnectionState' "CONNECTED"

pattern Disconnected :: ConnectionState
pattern Disconnected = ConnectionState' "DISCONNECTED"

pattern Unknown :: ConnectionState
pattern Unknown = ConnectionState' "UNKNOWN"

{-# COMPLETE
  Connected,
  Disconnected,
  Unknown,
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
        0 -> Connected
        1 -> Disconnected
        2 -> Unknown
        _ -> (error . showText) $ "Unknown index for ConnectionState: " <> toText i
    fromEnum x = case x of
        Connected -> 0
        Disconnected -> 1
        Unknown -> 2
        ConnectionState' name -> (error . showText) $ "Unknown ConnectionState: " <> original name

-- | Represents the bounds of /known/ $ConnectionState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionState where
    minBound = Connected
    maxBound = Unknown

instance Hashable     ConnectionState
instance NFData       ConnectionState
instance ToByteString ConnectionState
instance ToQuery      ConnectionState
instance ToHeader     ConnectionState

instance FromJSON ConnectionState where
    parseJSON = parseJSONText "ConnectionState"
