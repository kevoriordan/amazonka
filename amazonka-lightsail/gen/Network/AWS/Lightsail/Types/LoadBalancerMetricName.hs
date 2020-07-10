{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerMetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerMetricName (
  LoadBalancerMetricName (
    ..
    , ClientTLSNegotiationErrorCount
    , HTTPCodeInstance2XXCount
    , HTTPCodeInstance3XXCount
    , HTTPCodeInstance4XXCount
    , HTTPCodeInstance5XXCount
    , HTTPCodeLb4XXCount
    , HTTPCodeLb5XXCount
    , HealthyHostCount
    , InstanceResponseTime
    , RejectedConnectionCount
    , RequestCount
    , UnhealthyHostCount
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerMetricName = LoadBalancerMetricName' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern ClientTLSNegotiationErrorCount :: LoadBalancerMetricName
pattern ClientTLSNegotiationErrorCount = LoadBalancerMetricName' "ClientTLSNegotiationErrorCount"

pattern HTTPCodeInstance2XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance2XXCount = LoadBalancerMetricName' "HTTPCode_Instance_2XX_Count"

pattern HTTPCodeInstance3XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance3XXCount = LoadBalancerMetricName' "HTTPCode_Instance_3XX_Count"

pattern HTTPCodeInstance4XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance4XXCount = LoadBalancerMetricName' "HTTPCode_Instance_4XX_Count"

pattern HTTPCodeInstance5XXCount :: LoadBalancerMetricName
pattern HTTPCodeInstance5XXCount = LoadBalancerMetricName' "HTTPCode_Instance_5XX_Count"

pattern HTTPCodeLb4XXCount :: LoadBalancerMetricName
pattern HTTPCodeLb4XXCount = LoadBalancerMetricName' "HTTPCode_LB_4XX_Count"

pattern HTTPCodeLb5XXCount :: LoadBalancerMetricName
pattern HTTPCodeLb5XXCount = LoadBalancerMetricName' "HTTPCode_LB_5XX_Count"

pattern HealthyHostCount :: LoadBalancerMetricName
pattern HealthyHostCount = LoadBalancerMetricName' "HealthyHostCount"

pattern InstanceResponseTime :: LoadBalancerMetricName
pattern InstanceResponseTime = LoadBalancerMetricName' "InstanceResponseTime"

pattern RejectedConnectionCount :: LoadBalancerMetricName
pattern RejectedConnectionCount = LoadBalancerMetricName' "RejectedConnectionCount"

pattern RequestCount :: LoadBalancerMetricName
pattern RequestCount = LoadBalancerMetricName' "RequestCount"

pattern UnhealthyHostCount :: LoadBalancerMetricName
pattern UnhealthyHostCount = LoadBalancerMetricName' "UnhealthyHostCount"

{-# COMPLETE
  ClientTLSNegotiationErrorCount,
  HTTPCodeInstance2XXCount,
  HTTPCodeInstance3XXCount,
  HTTPCodeInstance4XXCount,
  HTTPCodeInstance5XXCount,
  HTTPCodeLb4XXCount,
  HTTPCodeLb5XXCount,
  HealthyHostCount,
  InstanceResponseTime,
  RejectedConnectionCount,
  RequestCount,
  UnhealthyHostCount,
  LoadBalancerMetricName' #-}

instance FromText LoadBalancerMetricName where
    parser = (LoadBalancerMetricName' . mk) <$> takeText

instance ToText LoadBalancerMetricName where
    toText (LoadBalancerMetricName' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerMetricName where
    toEnum i = case i of
        0 -> ClientTLSNegotiationErrorCount
        1 -> HTTPCodeInstance2XXCount
        2 -> HTTPCodeInstance3XXCount
        3 -> HTTPCodeInstance4XXCount
        4 -> HTTPCodeInstance5XXCount
        5 -> HTTPCodeLb4XXCount
        6 -> HTTPCodeLb5XXCount
        7 -> HealthyHostCount
        8 -> InstanceResponseTime
        9 -> RejectedConnectionCount
        10 -> RequestCount
        11 -> UnhealthyHostCount
        _ -> (error . showText) $ "Unknown index for LoadBalancerMetricName: " <> toText i
    fromEnum x = case x of
        ClientTLSNegotiationErrorCount -> 0
        HTTPCodeInstance2XXCount -> 1
        HTTPCodeInstance3XXCount -> 2
        HTTPCodeInstance4XXCount -> 3
        HTTPCodeInstance5XXCount -> 4
        HTTPCodeLb4XXCount -> 5
        HTTPCodeLb5XXCount -> 6
        HealthyHostCount -> 7
        InstanceResponseTime -> 8
        RejectedConnectionCount -> 9
        RequestCount -> 10
        UnhealthyHostCount -> 11
        LoadBalancerMetricName' name -> (error . showText) $ "Unknown LoadBalancerMetricName: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerMetricName where
    minBound = ClientTLSNegotiationErrorCount
    maxBound = UnhealthyHostCount

instance Hashable     LoadBalancerMetricName
instance NFData       LoadBalancerMetricName
instance ToByteString LoadBalancerMetricName
instance ToQuery      LoadBalancerMetricName
instance ToHeader     LoadBalancerMetricName

instance ToJSON LoadBalancerMetricName where
    toJSON = toJSONText

instance FromJSON LoadBalancerMetricName where
    parseJSON = parseJSONText "LoadBalancerMetricName"
