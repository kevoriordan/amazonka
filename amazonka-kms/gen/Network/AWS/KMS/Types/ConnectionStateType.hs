{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionStateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.ConnectionStateType (
  ConnectionStateType (
    ..
    , Connected
    , Connecting
    , Disconnected
    , Disconnecting
    , Failed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionStateType = ConnectionStateType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Connected :: ConnectionStateType
pattern Connected = ConnectionStateType' "CONNECTED"

pattern Connecting :: ConnectionStateType
pattern Connecting = ConnectionStateType' "CONNECTING"

pattern Disconnected :: ConnectionStateType
pattern Disconnected = ConnectionStateType' "DISCONNECTED"

pattern Disconnecting :: ConnectionStateType
pattern Disconnecting = ConnectionStateType' "DISCONNECTING"

pattern Failed :: ConnectionStateType
pattern Failed = ConnectionStateType' "FAILED"

{-# COMPLETE
  Connected,
  Connecting,
  Disconnected,
  Disconnecting,
  Failed,
  ConnectionStateType' #-}

instance FromText ConnectionStateType where
    parser = (ConnectionStateType' . mk) <$> takeText

instance ToText ConnectionStateType where
    toText (ConnectionStateType' ci) = original ci

-- | Represents an enum of /known/ $ConnectionStateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionStateType where
    toEnum i = case i of
        0 -> Connected
        1 -> Connecting
        2 -> Disconnected
        3 -> Disconnecting
        4 -> Failed
        _ -> (error . showText) $ "Unknown index for ConnectionStateType: " <> toText i
    fromEnum x = case x of
        Connected -> 0
        Connecting -> 1
        Disconnected -> 2
        Disconnecting -> 3
        Failed -> 4
        ConnectionStateType' name -> (error . showText) $ "Unknown ConnectionStateType: " <> original name

-- | Represents the bounds of /known/ $ConnectionStateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionStateType where
    minBound = Connected
    maxBound = Failed

instance Hashable     ConnectionStateType
instance NFData       ConnectionStateType
instance ToByteString ConnectionStateType
instance ToQuery      ConnectionStateType
instance ToHeader     ConnectionStateType

instance FromJSON ConnectionStateType where
    parseJSON = parseJSONText "ConnectionStateType"
