{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.RelationalDatabaseMetricName (
  RelationalDatabaseMetricName (
    ..
    , CPUUtilization
    , DatabaseConnections
    , DiskQueueDepth
    , FreeStorageSpace
    , NetworkReceiveThroughput
    , NetworkTransmitThroughput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RelationalDatabaseMetricName = RelationalDatabaseMetricName' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern CPUUtilization :: RelationalDatabaseMetricName
pattern CPUUtilization = RelationalDatabaseMetricName' "CPUUtilization"

pattern DatabaseConnections :: RelationalDatabaseMetricName
pattern DatabaseConnections = RelationalDatabaseMetricName' "DatabaseConnections"

pattern DiskQueueDepth :: RelationalDatabaseMetricName
pattern DiskQueueDepth = RelationalDatabaseMetricName' "DiskQueueDepth"

pattern FreeStorageSpace :: RelationalDatabaseMetricName
pattern FreeStorageSpace = RelationalDatabaseMetricName' "FreeStorageSpace"

pattern NetworkReceiveThroughput :: RelationalDatabaseMetricName
pattern NetworkReceiveThroughput = RelationalDatabaseMetricName' "NetworkReceiveThroughput"

pattern NetworkTransmitThroughput :: RelationalDatabaseMetricName
pattern NetworkTransmitThroughput = RelationalDatabaseMetricName' "NetworkTransmitThroughput"

{-# COMPLETE
  CPUUtilization,
  DatabaseConnections,
  DiskQueueDepth,
  FreeStorageSpace,
  NetworkReceiveThroughput,
  NetworkTransmitThroughput,
  RelationalDatabaseMetricName' #-}

instance FromText RelationalDatabaseMetricName where
    parser = (RelationalDatabaseMetricName' . mk) <$> takeText

instance ToText RelationalDatabaseMetricName where
    toText (RelationalDatabaseMetricName' ci) = original ci

-- | Represents an enum of /known/ $RelationalDatabaseMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RelationalDatabaseMetricName where
    toEnum i = case i of
        0 -> CPUUtilization
        1 -> DatabaseConnections
        2 -> DiskQueueDepth
        3 -> FreeStorageSpace
        4 -> NetworkReceiveThroughput
        5 -> NetworkTransmitThroughput
        _ -> (error . showText) $ "Unknown index for RelationalDatabaseMetricName: " <> toText i
    fromEnum x = case x of
        CPUUtilization -> 0
        DatabaseConnections -> 1
        DiskQueueDepth -> 2
        FreeStorageSpace -> 3
        NetworkReceiveThroughput -> 4
        NetworkTransmitThroughput -> 5
        RelationalDatabaseMetricName' name -> (error . showText) $ "Unknown RelationalDatabaseMetricName: " <> original name

-- | Represents the bounds of /known/ $RelationalDatabaseMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RelationalDatabaseMetricName where
    minBound = CPUUtilization
    maxBound = NetworkTransmitThroughput

instance Hashable     RelationalDatabaseMetricName
instance NFData       RelationalDatabaseMetricName
instance ToByteString RelationalDatabaseMetricName
instance ToQuery      RelationalDatabaseMetricName
instance ToHeader     RelationalDatabaseMetricName

instance ToJSON RelationalDatabaseMetricName where
    toJSON = toJSONText

instance FromJSON RelationalDatabaseMetricName where
    parseJSON = parseJSONText "RelationalDatabaseMetricName"
