{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingMetricType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingMetricType (
  ScalingMetricType (
    ..
    , ALBRequestCountPerTarget
    , ASGAverageCPUUtilization
    , ASGAverageNetworkIn
    , ASGAverageNetworkOut
    , DynamoDBReadCapacityUtilization
    , DynamoDBWriteCapacityUtilization
    , EC2SpotFleetRequestAverageCPUUtilization
    , EC2SpotFleetRequestAverageNetworkIn
    , EC2SpotFleetRequestAverageNetworkOut
    , ECSServiceAverageCPUUtilization
    , ECSServiceAverageMemoryUtilization
    , RDSReaderAverageCPUUtilization
    , RDSReaderAverageDatabaseConnections
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingMetricType = ScalingMetricType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ALBRequestCountPerTarget :: ScalingMetricType
pattern ALBRequestCountPerTarget = ScalingMetricType' "ALBRequestCountPerTarget"

pattern ASGAverageCPUUtilization :: ScalingMetricType
pattern ASGAverageCPUUtilization = ScalingMetricType' "ASGAverageCPUUtilization"

pattern ASGAverageNetworkIn :: ScalingMetricType
pattern ASGAverageNetworkIn = ScalingMetricType' "ASGAverageNetworkIn"

pattern ASGAverageNetworkOut :: ScalingMetricType
pattern ASGAverageNetworkOut = ScalingMetricType' "ASGAverageNetworkOut"

pattern DynamoDBReadCapacityUtilization :: ScalingMetricType
pattern DynamoDBReadCapacityUtilization = ScalingMetricType' "DynamoDBReadCapacityUtilization"

pattern DynamoDBWriteCapacityUtilization :: ScalingMetricType
pattern DynamoDBWriteCapacityUtilization = ScalingMetricType' "DynamoDBWriteCapacityUtilization"

pattern EC2SpotFleetRequestAverageCPUUtilization :: ScalingMetricType
pattern EC2SpotFleetRequestAverageCPUUtilization = ScalingMetricType' "EC2SpotFleetRequestAverageCPUUtilization"

pattern EC2SpotFleetRequestAverageNetworkIn :: ScalingMetricType
pattern EC2SpotFleetRequestAverageNetworkIn = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkIn"

pattern EC2SpotFleetRequestAverageNetworkOut :: ScalingMetricType
pattern EC2SpotFleetRequestAverageNetworkOut = ScalingMetricType' "EC2SpotFleetRequestAverageNetworkOut"

pattern ECSServiceAverageCPUUtilization :: ScalingMetricType
pattern ECSServiceAverageCPUUtilization = ScalingMetricType' "ECSServiceAverageCPUUtilization"

pattern ECSServiceAverageMemoryUtilization :: ScalingMetricType
pattern ECSServiceAverageMemoryUtilization = ScalingMetricType' "ECSServiceAverageMemoryUtilization"

pattern RDSReaderAverageCPUUtilization :: ScalingMetricType
pattern RDSReaderAverageCPUUtilization = ScalingMetricType' "RDSReaderAverageCPUUtilization"

pattern RDSReaderAverageDatabaseConnections :: ScalingMetricType
pattern RDSReaderAverageDatabaseConnections = ScalingMetricType' "RDSReaderAverageDatabaseConnections"

{-# COMPLETE
  ALBRequestCountPerTarget,
  ASGAverageCPUUtilization,
  ASGAverageNetworkIn,
  ASGAverageNetworkOut,
  DynamoDBReadCapacityUtilization,
  DynamoDBWriteCapacityUtilization,
  EC2SpotFleetRequestAverageCPUUtilization,
  EC2SpotFleetRequestAverageNetworkIn,
  EC2SpotFleetRequestAverageNetworkOut,
  ECSServiceAverageCPUUtilization,
  ECSServiceAverageMemoryUtilization,
  RDSReaderAverageCPUUtilization,
  RDSReaderAverageDatabaseConnections,
  ScalingMetricType' #-}

instance FromText ScalingMetricType where
    parser = (ScalingMetricType' . mk) <$> takeText

instance ToText ScalingMetricType where
    toText (ScalingMetricType' ci) = original ci

-- | Represents an enum of /known/ $ScalingMetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingMetricType where
    toEnum i = case i of
        0 -> ALBRequestCountPerTarget
        1 -> ASGAverageCPUUtilization
        2 -> ASGAverageNetworkIn
        3 -> ASGAverageNetworkOut
        4 -> DynamoDBReadCapacityUtilization
        5 -> DynamoDBWriteCapacityUtilization
        6 -> EC2SpotFleetRequestAverageCPUUtilization
        7 -> EC2SpotFleetRequestAverageNetworkIn
        8 -> EC2SpotFleetRequestAverageNetworkOut
        9 -> ECSServiceAverageCPUUtilization
        10 -> ECSServiceAverageMemoryUtilization
        11 -> RDSReaderAverageCPUUtilization
        12 -> RDSReaderAverageDatabaseConnections
        _ -> (error . showText) $ "Unknown index for ScalingMetricType: " <> toText i
    fromEnum x = case x of
        ALBRequestCountPerTarget -> 0
        ASGAverageCPUUtilization -> 1
        ASGAverageNetworkIn -> 2
        ASGAverageNetworkOut -> 3
        DynamoDBReadCapacityUtilization -> 4
        DynamoDBWriteCapacityUtilization -> 5
        EC2SpotFleetRequestAverageCPUUtilization -> 6
        EC2SpotFleetRequestAverageNetworkIn -> 7
        EC2SpotFleetRequestAverageNetworkOut -> 8
        ECSServiceAverageCPUUtilization -> 9
        ECSServiceAverageMemoryUtilization -> 10
        RDSReaderAverageCPUUtilization -> 11
        RDSReaderAverageDatabaseConnections -> 12
        ScalingMetricType' name -> (error . showText) $ "Unknown ScalingMetricType: " <> original name

-- | Represents the bounds of /known/ $ScalingMetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingMetricType where
    minBound = ALBRequestCountPerTarget
    maxBound = RDSReaderAverageDatabaseConnections

instance Hashable     ScalingMetricType
instance NFData       ScalingMetricType
instance ToByteString ScalingMetricType
instance ToQuery      ScalingMetricType
instance ToHeader     ScalingMetricType

instance ToJSON ScalingMetricType where
    toJSON = toJSONText

instance FromJSON ScalingMetricType where
    parseJSON = parseJSONText "ScalingMetricType"
