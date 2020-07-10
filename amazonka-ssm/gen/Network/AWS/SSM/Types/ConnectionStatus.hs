{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ConnectionStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ConnectionStatus (
  ConnectionStatus (
    ..
    , Connected
    , NotConnected
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionStatus = ConnectionStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Connected :: ConnectionStatus
pattern Connected = ConnectionStatus' "Connected"

pattern NotConnected :: ConnectionStatus
pattern NotConnected = ConnectionStatus' "NotConnected"

{-# COMPLETE
  Connected,
  NotConnected,
  ConnectionStatus' #-}

instance FromText ConnectionStatus where
    parser = (ConnectionStatus' . mk) <$> takeText

instance ToText ConnectionStatus where
    toText (ConnectionStatus' ci) = original ci

-- | Represents an enum of /known/ $ConnectionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionStatus where
    toEnum i = case i of
        0 -> Connected
        1 -> NotConnected
        _ -> (error . showText) $ "Unknown index for ConnectionStatus: " <> toText i
    fromEnum x = case x of
        Connected -> 0
        NotConnected -> 1
        ConnectionStatus' name -> (error . showText) $ "Unknown ConnectionStatus: " <> original name

-- | Represents the bounds of /known/ $ConnectionStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionStatus where
    minBound = Connected
    maxBound = NotConnected

instance Hashable     ConnectionStatus
instance NFData       ConnectionStatus
instance ToByteString ConnectionStatus
instance ToQuery      ConnectionStatus
instance ToHeader     ConnectionStatus

instance FromJSON ConnectionStatus where
    parseJSON = parseJSONText "ConnectionStatus"
