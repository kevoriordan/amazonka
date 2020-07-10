{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.MetricName (
  MetricName (
    ..
    , MNCPUUtilization
    , MNClientTLSNegotiationErrorCount
    , MNDatabaseConnections
    , MNDiskQueueDepth
    , MNFreeStorageSpace
    , MNHTTPCodeInstance2XXCount
    , MNHTTPCodeInstance3XXCount
    , MNHTTPCodeInstance4XXCount
    , MNHTTPCodeInstance5XXCount
    , MNHTTPCodeLb4XXCount
    , MNHTTPCodeLb5XXCount
    , MNHealthyHostCount
    , MNInstanceResponseTime
    , MNNetworkIn
    , MNNetworkOut
    , MNNetworkReceiveThroughput
    , MNNetworkTransmitThroughput
    , MNRejectedConnectionCount
    , MNRequestCount
    , MNStatusCheckFailed
    , MNStatusCheckFailedInstance
    , MNStatusCheckFailedSystem
    , MNUnhealthyHostCount
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricName = MetricName' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern MNCPUUtilization :: MetricName
pattern MNCPUUtilization = MetricName' "CPUUtilization"

pattern MNClientTLSNegotiationErrorCount :: MetricName
pattern MNClientTLSNegotiationErrorCount = MetricName' "ClientTLSNegotiationErrorCount"

pattern MNDatabaseConnections :: MetricName
pattern MNDatabaseConnections = MetricName' "DatabaseConnections"

pattern MNDiskQueueDepth :: MetricName
pattern MNDiskQueueDepth = MetricName' "DiskQueueDepth"

pattern MNFreeStorageSpace :: MetricName
pattern MNFreeStorageSpace = MetricName' "FreeStorageSpace"

pattern MNHTTPCodeInstance2XXCount :: MetricName
pattern MNHTTPCodeInstance2XXCount = MetricName' "HTTPCode_Instance_2XX_Count"

pattern MNHTTPCodeInstance3XXCount :: MetricName
pattern MNHTTPCodeInstance3XXCount = MetricName' "HTTPCode_Instance_3XX_Count"

pattern MNHTTPCodeInstance4XXCount :: MetricName
pattern MNHTTPCodeInstance4XXCount = MetricName' "HTTPCode_Instance_4XX_Count"

pattern MNHTTPCodeInstance5XXCount :: MetricName
pattern MNHTTPCodeInstance5XXCount = MetricName' "HTTPCode_Instance_5XX_Count"

pattern MNHTTPCodeLb4XXCount :: MetricName
pattern MNHTTPCodeLb4XXCount = MetricName' "HTTPCode_LB_4XX_Count"

pattern MNHTTPCodeLb5XXCount :: MetricName
pattern MNHTTPCodeLb5XXCount = MetricName' "HTTPCode_LB_5XX_Count"

pattern MNHealthyHostCount :: MetricName
pattern MNHealthyHostCount = MetricName' "HealthyHostCount"

pattern MNInstanceResponseTime :: MetricName
pattern MNInstanceResponseTime = MetricName' "InstanceResponseTime"

pattern MNNetworkIn :: MetricName
pattern MNNetworkIn = MetricName' "NetworkIn"

pattern MNNetworkOut :: MetricName
pattern MNNetworkOut = MetricName' "NetworkOut"

pattern MNNetworkReceiveThroughput :: MetricName
pattern MNNetworkReceiveThroughput = MetricName' "NetworkReceiveThroughput"

pattern MNNetworkTransmitThroughput :: MetricName
pattern MNNetworkTransmitThroughput = MetricName' "NetworkTransmitThroughput"

pattern MNRejectedConnectionCount :: MetricName
pattern MNRejectedConnectionCount = MetricName' "RejectedConnectionCount"

pattern MNRequestCount :: MetricName
pattern MNRequestCount = MetricName' "RequestCount"

pattern MNStatusCheckFailed :: MetricName
pattern MNStatusCheckFailed = MetricName' "StatusCheckFailed"

pattern MNStatusCheckFailedInstance :: MetricName
pattern MNStatusCheckFailedInstance = MetricName' "StatusCheckFailed_Instance"

pattern MNStatusCheckFailedSystem :: MetricName
pattern MNStatusCheckFailedSystem = MetricName' "StatusCheckFailed_System"

pattern MNUnhealthyHostCount :: MetricName
pattern MNUnhealthyHostCount = MetricName' "UnhealthyHostCount"

{-# COMPLETE
  MNCPUUtilization,
  MNClientTLSNegotiationErrorCount,
  MNDatabaseConnections,
  MNDiskQueueDepth,
  MNFreeStorageSpace,
  MNHTTPCodeInstance2XXCount,
  MNHTTPCodeInstance3XXCount,
  MNHTTPCodeInstance4XXCount,
  MNHTTPCodeInstance5XXCount,
  MNHTTPCodeLb4XXCount,
  MNHTTPCodeLb5XXCount,
  MNHealthyHostCount,
  MNInstanceResponseTime,
  MNNetworkIn,
  MNNetworkOut,
  MNNetworkReceiveThroughput,
  MNNetworkTransmitThroughput,
  MNRejectedConnectionCount,
  MNRequestCount,
  MNStatusCheckFailed,
  MNStatusCheckFailedInstance,
  MNStatusCheckFailedSystem,
  MNUnhealthyHostCount,
  MetricName' #-}

instance FromText MetricName where
    parser = (MetricName' . mk) <$> takeText

instance ToText MetricName where
    toText (MetricName' ci) = original ci

-- | Represents an enum of /known/ $MetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricName where
    toEnum i = case i of
        0 -> MNCPUUtilization
        1 -> MNClientTLSNegotiationErrorCount
        2 -> MNDatabaseConnections
        3 -> MNDiskQueueDepth
        4 -> MNFreeStorageSpace
        5 -> MNHTTPCodeInstance2XXCount
        6 -> MNHTTPCodeInstance3XXCount
        7 -> MNHTTPCodeInstance4XXCount
        8 -> MNHTTPCodeInstance5XXCount
        9 -> MNHTTPCodeLb4XXCount
        10 -> MNHTTPCodeLb5XXCount
        11 -> MNHealthyHostCount
        12 -> MNInstanceResponseTime
        13 -> MNNetworkIn
        14 -> MNNetworkOut
        15 -> MNNetworkReceiveThroughput
        16 -> MNNetworkTransmitThroughput
        17 -> MNRejectedConnectionCount
        18 -> MNRequestCount
        19 -> MNStatusCheckFailed
        20 -> MNStatusCheckFailedInstance
        21 -> MNStatusCheckFailedSystem
        22 -> MNUnhealthyHostCount
        _ -> (error . showText) $ "Unknown index for MetricName: " <> toText i
    fromEnum x = case x of
        MNCPUUtilization -> 0
        MNClientTLSNegotiationErrorCount -> 1
        MNDatabaseConnections -> 2
        MNDiskQueueDepth -> 3
        MNFreeStorageSpace -> 4
        MNHTTPCodeInstance2XXCount -> 5
        MNHTTPCodeInstance3XXCount -> 6
        MNHTTPCodeInstance4XXCount -> 7
        MNHTTPCodeInstance5XXCount -> 8
        MNHTTPCodeLb4XXCount -> 9
        MNHTTPCodeLb5XXCount -> 10
        MNHealthyHostCount -> 11
        MNInstanceResponseTime -> 12
        MNNetworkIn -> 13
        MNNetworkOut -> 14
        MNNetworkReceiveThroughput -> 15
        MNNetworkTransmitThroughput -> 16
        MNRejectedConnectionCount -> 17
        MNRequestCount -> 18
        MNStatusCheckFailed -> 19
        MNStatusCheckFailedInstance -> 20
        MNStatusCheckFailedSystem -> 21
        MNUnhealthyHostCount -> 22
        MetricName' name -> (error . showText) $ "Unknown MetricName: " <> original name

-- | Represents the bounds of /known/ $MetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricName where
    minBound = MNCPUUtilization
    maxBound = MNUnhealthyHostCount

instance Hashable     MetricName
instance NFData       MetricName
instance ToByteString MetricName
instance ToQuery      MetricName
instance ToHeader     MetricName

instance ToJSON MetricName where
    toJSON = toJSONText

instance FromJSON MetricName where
    parseJSON = parseJSONText "MetricName"
