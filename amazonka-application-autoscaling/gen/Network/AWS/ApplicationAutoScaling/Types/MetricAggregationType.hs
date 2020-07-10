{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.MetricAggregationType (
  MetricAggregationType (
    ..
    , MATAverage
    , MATMaximum
    , MATMinimum
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricAggregationType = MetricAggregationType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern MATAverage :: MetricAggregationType
pattern MATAverage = MetricAggregationType' "Average"

pattern MATMaximum :: MetricAggregationType
pattern MATMaximum = MetricAggregationType' "Maximum"

pattern MATMinimum :: MetricAggregationType
pattern MATMinimum = MetricAggregationType' "Minimum"

{-# COMPLETE
  MATAverage,
  MATMaximum,
  MATMinimum,
  MetricAggregationType' #-}

instance FromText MetricAggregationType where
    parser = (MetricAggregationType' . mk) <$> takeText

instance ToText MetricAggregationType where
    toText (MetricAggregationType' ci) = original ci

-- | Represents an enum of /known/ $MetricAggregationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricAggregationType where
    toEnum i = case i of
        0 -> MATAverage
        1 -> MATMaximum
        2 -> MATMinimum
        _ -> (error . showText) $ "Unknown index for MetricAggregationType: " <> toText i
    fromEnum x = case x of
        MATAverage -> 0
        MATMaximum -> 1
        MATMinimum -> 2
        MetricAggregationType' name -> (error . showText) $ "Unknown MetricAggregationType: " <> original name

-- | Represents the bounds of /known/ $MetricAggregationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricAggregationType where
    minBound = MATAverage
    maxBound = MATMinimum

instance Hashable     MetricAggregationType
instance NFData       MetricAggregationType
instance ToByteString MetricAggregationType
instance ToQuery      MetricAggregationType
instance ToHeader     MetricAggregationType

instance ToJSON MetricAggregationType where
    toJSON = toJSONText

instance FromJSON MetricAggregationType where
    parseJSON = parseJSONText "MetricAggregationType"
