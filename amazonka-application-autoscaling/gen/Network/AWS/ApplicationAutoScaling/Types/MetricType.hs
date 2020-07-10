{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.MetricType (
  MetricType (
    ..
    , ALBRequestCountPerTarget
    , AppStreamAverageCapacityUtilization
    , CassandraReadCapacityUtilization
    , CassandraWriteCapacityUtilization
    , ComprehendInferenceUtilization
    , DynamoDBReadCapacityUtilization
    , DynamoDBWriteCapacityUtilization
    , EC2SpotFleetRequestAverageCPUUtilization
    , EC2SpotFleetRequestAverageNetworkIn
    , EC2SpotFleetRequestAverageNetworkOut
    , ECSServiceAverageCPUUtilization
    , ECSServiceAverageMemoryUtilization
    , LambdaProvisionedConcurrencyUtilization
    , RDSReaderAverageCPUUtilization
    , RDSReaderAverageDatabaseConnections
    , SageMakerVariantInvocationsPerInstance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricType = MetricType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ALBRequestCountPerTarget :: MetricType
pattern ALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

pattern AppStreamAverageCapacityUtilization :: MetricType
pattern AppStreamAverageCapacityUtilization = MetricType' "AppStreamAverageCapacityUtilization"

pattern CassandraReadCapacityUtilization :: MetricType
pattern CassandraReadCapacityUtilization = MetricType' "CassandraReadCapacityUtilization"

pattern CassandraWriteCapacityUtilization :: MetricType
pattern CassandraWriteCapacityUtilization = MetricType' "CassandraWriteCapacityUtilization"

pattern ComprehendInferenceUtilization :: MetricType
pattern ComprehendInferenceUtilization = MetricType' "ComprehendInferenceUtilization"

pattern DynamoDBReadCapacityUtilization :: MetricType
pattern DynamoDBReadCapacityUtilization = MetricType' "DynamoDBReadCapacityUtilization"

pattern DynamoDBWriteCapacityUtilization :: MetricType
pattern DynamoDBWriteCapacityUtilization = MetricType' "DynamoDBWriteCapacityUtilization"

pattern EC2SpotFleetRequestAverageCPUUtilization :: MetricType
pattern EC2SpotFleetRequestAverageCPUUtilization = MetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern EC2SpotFleetRequestAverageNetworkIn :: MetricType
pattern EC2SpotFleetRequestAverageNetworkIn = MetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern EC2SpotFleetRequestAverageNetworkOut :: MetricType
pattern EC2SpotFleetRequestAverageNetworkOut = MetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern ECSServiceAverageCPUUtilization :: MetricType
pattern ECSServiceAverageCPUUtilization = MetricType' "ECSServiceAverageCPUUtilization"

pattern ECSServiceAverageMemoryUtilization :: MetricType
pattern ECSServiceAverageMemoryUtilization = MetricType' "ECSServiceAverageMemoryUtilization"

pattern LambdaProvisionedConcurrencyUtilization :: MetricType
pattern LambdaProvisionedConcurrencyUtilization = MetricType' "LambdaProvisionedConcurrencyUtilization"

pattern RDSReaderAverageCPUUtilization :: MetricType
pattern RDSReaderAverageCPUUtilization = MetricType' "RDSReaderAverageCPUUtilization"

pattern RDSReaderAverageDatabaseConnections :: MetricType
pattern RDSReaderAverageDatabaseConnections = MetricType' "RDSReaderAverageDatabaseConnections"

pattern SageMakerVariantInvocationsPerInstance :: MetricType
pattern SageMakerVariantInvocationsPerInstance = MetricType' "SageMakerVariantInvocationsPerInstance"

{-# COMPLETE
  ALBRequestCountPerTarget,
  AppStreamAverageCapacityUtilization,
  CassandraReadCapacityUtilization,
  CassandraWriteCapacityUtilization,
  ComprehendInferenceUtilization,
  DynamoDBReadCapacityUtilization,
  DynamoDBWriteCapacityUtilization,
  EC2SpotFleetRequestAverageCPUUtilization,
  EC2SpotFleetRequestAverageNetworkIn,
  EC2SpotFleetRequestAverageNetworkOut,
  ECSServiceAverageCPUUtilization,
  ECSServiceAverageMemoryUtilization,
  LambdaProvisionedConcurrencyUtilization,
  RDSReaderAverageCPUUtilization,
  RDSReaderAverageDatabaseConnections,
  SageMakerVariantInvocationsPerInstance,
  MetricType' #-}

instance FromText MetricType where
    parser = (MetricType' . mk) <$> takeText

instance ToText MetricType where
    toText (MetricType' ci) = original ci

-- | Represents an enum of /known/ $MetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricType where
    toEnum i = case i of
        0 -> ALBRequestCountPerTarget
        1 -> AppStreamAverageCapacityUtilization
        2 -> CassandraReadCapacityUtilization
        3 -> CassandraWriteCapacityUtilization
        4 -> ComprehendInferenceUtilization
        5 -> DynamoDBReadCapacityUtilization
        6 -> DynamoDBWriteCapacityUtilization
        7 -> EC2SpotFleetRequestAverageCPUUtilization
        8 -> EC2SpotFleetRequestAverageNetworkIn
        9 -> EC2SpotFleetRequestAverageNetworkOut
        10 -> ECSServiceAverageCPUUtilization
        11 -> ECSServiceAverageMemoryUtilization
        12 -> LambdaProvisionedConcurrencyUtilization
        13 -> RDSReaderAverageCPUUtilization
        14 -> RDSReaderAverageDatabaseConnections
        15 -> SageMakerVariantInvocationsPerInstance
        _ -> (error . showText) $ "Unknown index for MetricType: " <> toText i
    fromEnum x = case x of
        ALBRequestCountPerTarget -> 0
        AppStreamAverageCapacityUtilization -> 1
        CassandraReadCapacityUtilization -> 2
        CassandraWriteCapacityUtilization -> 3
        ComprehendInferenceUtilization -> 4
        DynamoDBReadCapacityUtilization -> 5
        DynamoDBWriteCapacityUtilization -> 6
        EC2SpotFleetRequestAverageCPUUtilization -> 7
        EC2SpotFleetRequestAverageNetworkIn -> 8
        EC2SpotFleetRequestAverageNetworkOut -> 9
        ECSServiceAverageCPUUtilization -> 10
        ECSServiceAverageMemoryUtilization -> 11
        LambdaProvisionedConcurrencyUtilization -> 12
        RDSReaderAverageCPUUtilization -> 13
        RDSReaderAverageDatabaseConnections -> 14
        SageMakerVariantInvocationsPerInstance -> 15
        MetricType' name -> (error . showText) $ "Unknown MetricType: " <> original name

-- | Represents the bounds of /known/ $MetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricType where
    minBound = ALBRequestCountPerTarget
    maxBound = SageMakerVariantInvocationsPerInstance

instance Hashable     MetricType
instance NFData       MetricType
instance ToByteString MetricType
instance ToQuery      MetricType
instance ToHeader     MetricType

instance ToJSON MetricType where
    toJSON = toJSONText

instance FromJSON MetricType where
    parseJSON = parseJSONText "MetricType"
