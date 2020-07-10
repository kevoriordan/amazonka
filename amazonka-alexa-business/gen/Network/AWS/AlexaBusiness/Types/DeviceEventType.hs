{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceEventType (
  DeviceEventType (
    ..
    , ConnectionStatus
    , DeviceStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceEventType = DeviceEventType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ConnectionStatus :: DeviceEventType
pattern ConnectionStatus = DeviceEventType' "CONNECTION_STATUS"

pattern DeviceStatus :: DeviceEventType
pattern DeviceStatus = DeviceEventType' "DEVICE_STATUS"

{-# COMPLETE
  ConnectionStatus,
  DeviceStatus,
  DeviceEventType' #-}

instance FromText DeviceEventType where
    parser = (DeviceEventType' . mk) <$> takeText

instance ToText DeviceEventType where
    toText (DeviceEventType' ci) = original ci

-- | Represents an enum of /known/ $DeviceEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceEventType where
    toEnum i = case i of
        0 -> ConnectionStatus
        1 -> DeviceStatus
        _ -> (error . showText) $ "Unknown index for DeviceEventType: " <> toText i
    fromEnum x = case x of
        ConnectionStatus -> 0
        DeviceStatus -> 1
        DeviceEventType' name -> (error . showText) $ "Unknown DeviceEventType: " <> original name

-- | Represents the bounds of /known/ $DeviceEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceEventType where
    minBound = ConnectionStatus
    maxBound = DeviceStatus

instance Hashable     DeviceEventType
instance NFData       DeviceEventType
instance ToByteString DeviceEventType
instance ToQuery      DeviceEventType
instance ToHeader     DeviceEventType

instance ToJSON DeviceEventType where
    toJSON = toJSONText

instance FromJSON DeviceEventType where
    parseJSON = parseJSONText "DeviceEventType"
