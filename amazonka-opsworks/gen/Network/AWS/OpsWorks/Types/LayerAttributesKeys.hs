{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerAttributesKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LayerAttributesKeys (
  LayerAttributesKeys (
    ..
    , BundlerVersion
    , EcsClusterARN
    , EnableHaproxyStats
    , GangliaPassword
    , GangliaURL
    , GangliaUser
    , HaproxyHealthCheckMethod
    , HaproxyHealthCheckURL
    , HaproxyStatsPassword
    , HaproxyStatsURL
    , HaproxyStatsUser
    , JVM
    , JVMOptions
    , JVMVersion
    , JavaAppServer
    , JavaAppServerVersion
    , ManageBundler
    , MemcachedMemory
    , MysqlRootPassword
    , MysqlRootPasswordUbiquitous
    , NodejsVersion
    , PassengerVersion
    , RailsStack
    , RubyVersion
    , RubygemsVersion
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LayerAttributesKeys = LayerAttributesKeys' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern BundlerVersion :: LayerAttributesKeys
pattern BundlerVersion = LayerAttributesKeys' "BundlerVersion"

pattern EcsClusterARN :: LayerAttributesKeys
pattern EcsClusterARN = LayerAttributesKeys' "EcsClusterArn"

pattern EnableHaproxyStats :: LayerAttributesKeys
pattern EnableHaproxyStats = LayerAttributesKeys' "EnableHaproxyStats"

pattern GangliaPassword :: LayerAttributesKeys
pattern GangliaPassword = LayerAttributesKeys' "GangliaPassword"

pattern GangliaURL :: LayerAttributesKeys
pattern GangliaURL = LayerAttributesKeys' "GangliaUrl"

pattern GangliaUser :: LayerAttributesKeys
pattern GangliaUser = LayerAttributesKeys' "GangliaUser"

pattern HaproxyHealthCheckMethod :: LayerAttributesKeys
pattern HaproxyHealthCheckMethod = LayerAttributesKeys' "HaproxyHealthCheckMethod"

pattern HaproxyHealthCheckURL :: LayerAttributesKeys
pattern HaproxyHealthCheckURL = LayerAttributesKeys' "HaproxyHealthCheckUrl"

pattern HaproxyStatsPassword :: LayerAttributesKeys
pattern HaproxyStatsPassword = LayerAttributesKeys' "HaproxyStatsPassword"

pattern HaproxyStatsURL :: LayerAttributesKeys
pattern HaproxyStatsURL = LayerAttributesKeys' "HaproxyStatsUrl"

pattern HaproxyStatsUser :: LayerAttributesKeys
pattern HaproxyStatsUser = LayerAttributesKeys' "HaproxyStatsUser"

pattern JVM :: LayerAttributesKeys
pattern JVM = LayerAttributesKeys' "Jvm"

pattern JVMOptions :: LayerAttributesKeys
pattern JVMOptions = LayerAttributesKeys' "JvmOptions"

pattern JVMVersion :: LayerAttributesKeys
pattern JVMVersion = LayerAttributesKeys' "JvmVersion"

pattern JavaAppServer :: LayerAttributesKeys
pattern JavaAppServer = LayerAttributesKeys' "JavaAppServer"

pattern JavaAppServerVersion :: LayerAttributesKeys
pattern JavaAppServerVersion = LayerAttributesKeys' "JavaAppServerVersion"

pattern ManageBundler :: LayerAttributesKeys
pattern ManageBundler = LayerAttributesKeys' "ManageBundler"

pattern MemcachedMemory :: LayerAttributesKeys
pattern MemcachedMemory = LayerAttributesKeys' "MemcachedMemory"

pattern MysqlRootPassword :: LayerAttributesKeys
pattern MysqlRootPassword = LayerAttributesKeys' "MysqlRootPassword"

pattern MysqlRootPasswordUbiquitous :: LayerAttributesKeys
pattern MysqlRootPasswordUbiquitous = LayerAttributesKeys' "MysqlRootPasswordUbiquitous"

pattern NodejsVersion :: LayerAttributesKeys
pattern NodejsVersion = LayerAttributesKeys' "NodejsVersion"

pattern PassengerVersion :: LayerAttributesKeys
pattern PassengerVersion = LayerAttributesKeys' "PassengerVersion"

pattern RailsStack :: LayerAttributesKeys
pattern RailsStack = LayerAttributesKeys' "RailsStack"

pattern RubyVersion :: LayerAttributesKeys
pattern RubyVersion = LayerAttributesKeys' "RubyVersion"

pattern RubygemsVersion :: LayerAttributesKeys
pattern RubygemsVersion = LayerAttributesKeys' "RubygemsVersion"

{-# COMPLETE
  BundlerVersion,
  EcsClusterARN,
  EnableHaproxyStats,
  GangliaPassword,
  GangliaURL,
  GangliaUser,
  HaproxyHealthCheckMethod,
  HaproxyHealthCheckURL,
  HaproxyStatsPassword,
  HaproxyStatsURL,
  HaproxyStatsUser,
  JVM,
  JVMOptions,
  JVMVersion,
  JavaAppServer,
  JavaAppServerVersion,
  ManageBundler,
  MemcachedMemory,
  MysqlRootPassword,
  MysqlRootPasswordUbiquitous,
  NodejsVersion,
  PassengerVersion,
  RailsStack,
  RubyVersion,
  RubygemsVersion,
  LayerAttributesKeys' #-}

instance FromText LayerAttributesKeys where
    parser = (LayerAttributesKeys' . mk) <$> takeText

instance ToText LayerAttributesKeys where
    toText (LayerAttributesKeys' ci) = original ci

-- | Represents an enum of /known/ $LayerAttributesKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LayerAttributesKeys where
    toEnum i = case i of
        0 -> BundlerVersion
        1 -> EcsClusterARN
        2 -> EnableHaproxyStats
        3 -> GangliaPassword
        4 -> GangliaURL
        5 -> GangliaUser
        6 -> HaproxyHealthCheckMethod
        7 -> HaproxyHealthCheckURL
        8 -> HaproxyStatsPassword
        9 -> HaproxyStatsURL
        10 -> HaproxyStatsUser
        11 -> JVM
        12 -> JVMOptions
        13 -> JVMVersion
        14 -> JavaAppServer
        15 -> JavaAppServerVersion
        16 -> ManageBundler
        17 -> MemcachedMemory
        18 -> MysqlRootPassword
        19 -> MysqlRootPasswordUbiquitous
        20 -> NodejsVersion
        21 -> PassengerVersion
        22 -> RailsStack
        23 -> RubyVersion
        24 -> RubygemsVersion
        _ -> (error . showText) $ "Unknown index for LayerAttributesKeys: " <> toText i
    fromEnum x = case x of
        BundlerVersion -> 0
        EcsClusterARN -> 1
        EnableHaproxyStats -> 2
        GangliaPassword -> 3
        GangliaURL -> 4
        GangliaUser -> 5
        HaproxyHealthCheckMethod -> 6
        HaproxyHealthCheckURL -> 7
        HaproxyStatsPassword -> 8
        HaproxyStatsURL -> 9
        HaproxyStatsUser -> 10
        JVM -> 11
        JVMOptions -> 12
        JVMVersion -> 13
        JavaAppServer -> 14
        JavaAppServerVersion -> 15
        ManageBundler -> 16
        MemcachedMemory -> 17
        MysqlRootPassword -> 18
        MysqlRootPasswordUbiquitous -> 19
        NodejsVersion -> 20
        PassengerVersion -> 21
        RailsStack -> 22
        RubyVersion -> 23
        RubygemsVersion -> 24
        LayerAttributesKeys' name -> (error . showText) $ "Unknown LayerAttributesKeys: " <> original name

-- | Represents the bounds of /known/ $LayerAttributesKeys.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LayerAttributesKeys where
    minBound = BundlerVersion
    maxBound = RubygemsVersion

instance Hashable     LayerAttributesKeys
instance NFData       LayerAttributesKeys
instance ToByteString LayerAttributesKeys
instance ToQuery      LayerAttributesKeys
instance ToHeader     LayerAttributesKeys

instance ToJSON LayerAttributesKeys where
    toJSON = toJSONText

instance FromJSON LayerAttributesKeys where
    parseJSON = parseJSONText "LayerAttributesKeys"
