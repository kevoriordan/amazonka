{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Protocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.Protocol (
  Protocol (
    ..
    , HTTP
    , Mqtt
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Protocol = Protocol' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern HTTP :: Protocol
pattern HTTP = Protocol' "HTTP"

pattern Mqtt :: Protocol
pattern Mqtt = Protocol' "MQTT"

{-# COMPLETE
  HTTP,
  Mqtt,
  Protocol' #-}

instance FromText Protocol where
    parser = (Protocol' . mk) <$> takeText

instance ToText Protocol where
    toText (Protocol' ci) = original ci

-- | Represents an enum of /known/ $Protocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Protocol where
    toEnum i = case i of
        0 -> HTTP
        1 -> Mqtt
        _ -> (error . showText) $ "Unknown index for Protocol: " <> toText i
    fromEnum x = case x of
        HTTP -> 0
        Mqtt -> 1
        Protocol' name -> (error . showText) $ "Unknown Protocol: " <> original name

-- | Represents the bounds of /known/ $Protocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Protocol where
    minBound = HTTP
    maxBound = Mqtt

instance Hashable     Protocol
instance NFData       Protocol
instance ToByteString Protocol
instance ToQuery      Protocol
instance ToHeader     Protocol

instance ToJSON Protocol where
    toJSON = toJSONText

instance FromJSON Protocol where
    parseJSON = parseJSONText "Protocol"
