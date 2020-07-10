{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPropertyKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ConnectionPropertyKey (
  ConnectionPropertyKey (
    ..
    , ConfigFiles
    , ConnectionURL
    , CustomJdbcCert
    , CustomJdbcCertString
    , EncryptedPassword
    , Host
    , InstanceId
    , JdbcConnectionURL
    , JdbcDriverClassName
    , JdbcDriverJARURI
    , JdbcEnforceSSL
    , JdbcEngine
    , JdbcEngineVersion
    , KafkaBootstrapServers
    , Password
    , Port
    , SkipCustomJdbcCertValidation
    , Username
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConnectionPropertyKey = ConnectionPropertyKey' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ConfigFiles :: ConnectionPropertyKey
pattern ConfigFiles = ConnectionPropertyKey' "CONFIG_FILES"

pattern ConnectionURL :: ConnectionPropertyKey
pattern ConnectionURL = ConnectionPropertyKey' "CONNECTION_URL"

pattern CustomJdbcCert :: ConnectionPropertyKey
pattern CustomJdbcCert = ConnectionPropertyKey' "CUSTOM_JDBC_CERT"

pattern CustomJdbcCertString :: ConnectionPropertyKey
pattern CustomJdbcCertString = ConnectionPropertyKey' "CUSTOM_JDBC_CERT_STRING"

pattern EncryptedPassword :: ConnectionPropertyKey
pattern EncryptedPassword = ConnectionPropertyKey' "ENCRYPTED_PASSWORD"

pattern Host :: ConnectionPropertyKey
pattern Host = ConnectionPropertyKey' "HOST"

pattern InstanceId :: ConnectionPropertyKey
pattern InstanceId = ConnectionPropertyKey' "INSTANCE_ID"

pattern JdbcConnectionURL :: ConnectionPropertyKey
pattern JdbcConnectionURL = ConnectionPropertyKey' "JDBC_CONNECTION_URL"

pattern JdbcDriverClassName :: ConnectionPropertyKey
pattern JdbcDriverClassName = ConnectionPropertyKey' "JDBC_DRIVER_CLASS_NAME"

pattern JdbcDriverJARURI :: ConnectionPropertyKey
pattern JdbcDriverJARURI = ConnectionPropertyKey' "JDBC_DRIVER_JAR_URI"

pattern JdbcEnforceSSL :: ConnectionPropertyKey
pattern JdbcEnforceSSL = ConnectionPropertyKey' "JDBC_ENFORCE_SSL"

pattern JdbcEngine :: ConnectionPropertyKey
pattern JdbcEngine = ConnectionPropertyKey' "JDBC_ENGINE"

pattern JdbcEngineVersion :: ConnectionPropertyKey
pattern JdbcEngineVersion = ConnectionPropertyKey' "JDBC_ENGINE_VERSION"

pattern KafkaBootstrapServers :: ConnectionPropertyKey
pattern KafkaBootstrapServers = ConnectionPropertyKey' "KAFKA_BOOTSTRAP_SERVERS"

pattern Password :: ConnectionPropertyKey
pattern Password = ConnectionPropertyKey' "PASSWORD"

pattern Port :: ConnectionPropertyKey
pattern Port = ConnectionPropertyKey' "PORT"

pattern SkipCustomJdbcCertValidation :: ConnectionPropertyKey
pattern SkipCustomJdbcCertValidation = ConnectionPropertyKey' "SKIP_CUSTOM_JDBC_CERT_VALIDATION"

pattern Username :: ConnectionPropertyKey
pattern Username = ConnectionPropertyKey' "USERNAME"

{-# COMPLETE
  ConfigFiles,
  ConnectionURL,
  CustomJdbcCert,
  CustomJdbcCertString,
  EncryptedPassword,
  Host,
  InstanceId,
  JdbcConnectionURL,
  JdbcDriverClassName,
  JdbcDriverJARURI,
  JdbcEnforceSSL,
  JdbcEngine,
  JdbcEngineVersion,
  KafkaBootstrapServers,
  Password,
  Port,
  SkipCustomJdbcCertValidation,
  Username,
  ConnectionPropertyKey' #-}

instance FromText ConnectionPropertyKey where
    parser = (ConnectionPropertyKey' . mk) <$> takeText

instance ToText ConnectionPropertyKey where
    toText (ConnectionPropertyKey' ci) = original ci

-- | Represents an enum of /known/ $ConnectionPropertyKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConnectionPropertyKey where
    toEnum i = case i of
        0 -> ConfigFiles
        1 -> ConnectionURL
        2 -> CustomJdbcCert
        3 -> CustomJdbcCertString
        4 -> EncryptedPassword
        5 -> Host
        6 -> InstanceId
        7 -> JdbcConnectionURL
        8 -> JdbcDriverClassName
        9 -> JdbcDriverJARURI
        10 -> JdbcEnforceSSL
        11 -> JdbcEngine
        12 -> JdbcEngineVersion
        13 -> KafkaBootstrapServers
        14 -> Password
        15 -> Port
        16 -> SkipCustomJdbcCertValidation
        17 -> Username
        _ -> (error . showText) $ "Unknown index for ConnectionPropertyKey: " <> toText i
    fromEnum x = case x of
        ConfigFiles -> 0
        ConnectionURL -> 1
        CustomJdbcCert -> 2
        CustomJdbcCertString -> 3
        EncryptedPassword -> 4
        Host -> 5
        InstanceId -> 6
        JdbcConnectionURL -> 7
        JdbcDriverClassName -> 8
        JdbcDriverJARURI -> 9
        JdbcEnforceSSL -> 10
        JdbcEngine -> 11
        JdbcEngineVersion -> 12
        KafkaBootstrapServers -> 13
        Password -> 14
        Port -> 15
        SkipCustomJdbcCertValidation -> 16
        Username -> 17
        ConnectionPropertyKey' name -> (error . showText) $ "Unknown ConnectionPropertyKey: " <> original name

-- | Represents the bounds of /known/ $ConnectionPropertyKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConnectionPropertyKey where
    minBound = ConfigFiles
    maxBound = Username

instance Hashable     ConnectionPropertyKey
instance NFData       ConnectionPropertyKey
instance ToByteString ConnectionPropertyKey
instance ToQuery      ConnectionPropertyKey
instance ToHeader     ConnectionPropertyKey

instance ToJSON ConnectionPropertyKey where
    toJSON = toJSONText

instance FromJSON ConnectionPropertyKey where
    parseJSON = parseJSONText "ConnectionPropertyKey"
