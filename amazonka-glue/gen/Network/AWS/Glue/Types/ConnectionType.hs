{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ConnectionType (
  ConnectionType (
    ..
    , Jdbc
    , Kafka
    , Mongodb
    , Sftp
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionType = ConnectionType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Jdbc :: ConnectionType
pattern Jdbc = ConnectionType' "JDBC"

pattern Kafka :: ConnectionType
pattern Kafka = ConnectionType' "KAFKA"

pattern Mongodb :: ConnectionType
pattern Mongodb = ConnectionType' "MONGODB"

pattern Sftp :: ConnectionType
pattern Sftp = ConnectionType' "SFTP"

{-# COMPLETE
  Jdbc,
  Kafka,
  Mongodb,
  Sftp,
  ConnectionType' #-}

instance FromText ConnectionType where
    parser = (ConnectionType' . mk) <$> takeText

instance ToText ConnectionType where
    toText (ConnectionType' ci) = original ci

-- | Represents an enum of /known/ $ConnectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionType where
    toEnum i = case i of
        0 -> Jdbc
        1 -> Kafka
        2 -> Mongodb
        3 -> Sftp
        _ -> (error . showText) $ "Unknown index for ConnectionType: " <> toText i
    fromEnum x = case x of
        Jdbc -> 0
        Kafka -> 1
        Mongodb -> 2
        Sftp -> 3
        ConnectionType' name -> (error . showText) $ "Unknown ConnectionType: " <> original name

-- | Represents the bounds of /known/ $ConnectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionType where
    minBound = Jdbc
    maxBound = Sftp

instance Hashable     ConnectionType
instance NFData       ConnectionType
instance ToByteString ConnectionType
instance ToQuery      ConnectionType
instance ToHeader     ConnectionType

instance ToJSON ConnectionType where
    toJSON = toJSONText

instance FromJSON ConnectionType where
    parseJSON = parseJSONText "ConnectionType"
