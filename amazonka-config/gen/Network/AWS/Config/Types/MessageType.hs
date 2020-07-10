{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MessageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.MessageType (
  MessageType (
    ..
    , ConfigurationItemChangeNotification
    , ConfigurationSnapshotDeliveryCompleted
    , OversizedConfigurationItemChangeNotification
    , ScheduledNotification
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MessageType = MessageType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern ConfigurationItemChangeNotification :: MessageType
pattern ConfigurationItemChangeNotification = MessageType' "ConfigurationItemChangeNotification"

pattern ConfigurationSnapshotDeliveryCompleted :: MessageType
pattern ConfigurationSnapshotDeliveryCompleted = MessageType' "ConfigurationSnapshotDeliveryCompleted"

pattern OversizedConfigurationItemChangeNotification :: MessageType
pattern OversizedConfigurationItemChangeNotification = MessageType' "OversizedConfigurationItemChangeNotification"

pattern ScheduledNotification :: MessageType
pattern ScheduledNotification = MessageType' "ScheduledNotification"

{-# COMPLETE
  ConfigurationItemChangeNotification,
  ConfigurationSnapshotDeliveryCompleted,
  OversizedConfigurationItemChangeNotification,
  ScheduledNotification,
  MessageType' #-}

instance FromText MessageType where
    parser = (MessageType' . mk) <$> takeText

instance ToText MessageType where
    toText (MessageType' ci) = original ci

-- | Represents an enum of /known/ $MessageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MessageType where
    toEnum i = case i of
        0 -> ConfigurationItemChangeNotification
        1 -> ConfigurationSnapshotDeliveryCompleted
        2 -> OversizedConfigurationItemChangeNotification
        3 -> ScheduledNotification
        _ -> (error . showText) $ "Unknown index for MessageType: " <> toText i
    fromEnum x = case x of
        ConfigurationItemChangeNotification -> 0
        ConfigurationSnapshotDeliveryCompleted -> 1
        OversizedConfigurationItemChangeNotification -> 2
        ScheduledNotification -> 3
        MessageType' name -> (error . showText) $ "Unknown MessageType: " <> original name

-- | Represents the bounds of /known/ $MessageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MessageType where
    minBound = ConfigurationItemChangeNotification
    maxBound = ScheduledNotification

instance Hashable     MessageType
instance NFData       MessageType
instance ToByteString MessageType
instance ToQuery      MessageType
instance ToHeader     MessageType

instance ToJSON MessageType where
    toJSON = toJSONText

instance FromJSON MessageType where
    parseJSON = parseJSONText "MessageType"
