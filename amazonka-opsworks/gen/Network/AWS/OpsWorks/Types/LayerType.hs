{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LayerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.LayerType (
  LayerType (
    ..
    , AWSFlowRuby
    , Custom
    , DBMaster
    , EcsCluster
    , JavaApp
    , LB
    , Memcached
    , MonitoringMaster
    , NodejsApp
    , PHPApp
    , RailsApp
    , Web
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LayerType = LayerType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AWSFlowRuby :: LayerType
pattern AWSFlowRuby = LayerType' "aws-flow-ruby"

pattern Custom :: LayerType
pattern Custom = LayerType' "custom"

pattern DBMaster :: LayerType
pattern DBMaster = LayerType' "db-master"

pattern EcsCluster :: LayerType
pattern EcsCluster = LayerType' "ecs-cluster"

pattern JavaApp :: LayerType
pattern JavaApp = LayerType' "java-app"

pattern LB :: LayerType
pattern LB = LayerType' "lb"

pattern Memcached :: LayerType
pattern Memcached = LayerType' "memcached"

pattern MonitoringMaster :: LayerType
pattern MonitoringMaster = LayerType' "monitoring-master"

pattern NodejsApp :: LayerType
pattern NodejsApp = LayerType' "nodejs-app"

pattern PHPApp :: LayerType
pattern PHPApp = LayerType' "php-app"

pattern RailsApp :: LayerType
pattern RailsApp = LayerType' "rails-app"

pattern Web :: LayerType
pattern Web = LayerType' "web"

{-# COMPLETE
  AWSFlowRuby,
  Custom,
  DBMaster,
  EcsCluster,
  JavaApp,
  LB,
  Memcached,
  MonitoringMaster,
  NodejsApp,
  PHPApp,
  RailsApp,
  Web,
  LayerType' #-}

instance FromText LayerType where
    parser = (LayerType' . mk) <$> takeText

instance ToText LayerType where
    toText (LayerType' ci) = original ci

-- | Represents an enum of /known/ $LayerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LayerType where
    toEnum i = case i of
        0 -> AWSFlowRuby
        1 -> Custom
        2 -> DBMaster
        3 -> EcsCluster
        4 -> JavaApp
        5 -> LB
        6 -> Memcached
        7 -> MonitoringMaster
        8 -> NodejsApp
        9 -> PHPApp
        10 -> RailsApp
        11 -> Web
        _ -> (error . showText) $ "Unknown index for LayerType: " <> toText i
    fromEnum x = case x of
        AWSFlowRuby -> 0
        Custom -> 1
        DBMaster -> 2
        EcsCluster -> 3
        JavaApp -> 4
        LB -> 5
        Memcached -> 6
        MonitoringMaster -> 7
        NodejsApp -> 8
        PHPApp -> 9
        RailsApp -> 10
        Web -> 11
        LayerType' name -> (error . showText) $ "Unknown LayerType: " <> original name

-- | Represents the bounds of /known/ $LayerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LayerType where
    minBound = AWSFlowRuby
    maxBound = Web

instance Hashable     LayerType
instance NFData       LayerType
instance ToByteString LayerType
instance ToQuery      LayerType
instance ToHeader     LayerType

instance ToJSON LayerType where
    toJSON = toJSONText

instance FromJSON LayerType where
    parseJSON = parseJSONText "LayerType"
