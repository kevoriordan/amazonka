{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricStatistic
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.MetricStatistic (
  MetricStatistic (
    ..
    , Average
    , Maximum
    , Minimum
    , SampleCount
    , Sum
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MetricStatistic = MetricStatistic' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Average :: MetricStatistic
pattern Average = MetricStatistic' "Average"

pattern Maximum :: MetricStatistic
pattern Maximum = MetricStatistic' "Maximum"

pattern Minimum :: MetricStatistic
pattern Minimum = MetricStatistic' "Minimum"

pattern SampleCount :: MetricStatistic
pattern SampleCount = MetricStatistic' "SampleCount"

pattern Sum :: MetricStatistic
pattern Sum = MetricStatistic' "Sum"

{-# COMPLETE
  Average,
  Maximum,
  Minimum,
  SampleCount,
  Sum,
  MetricStatistic' #-}

instance FromText MetricStatistic where
    parser = (MetricStatistic' . mk) <$> takeText

instance ToText MetricStatistic where
    toText (MetricStatistic' ci) = original ci

-- | Represents an enum of /known/ $MetricStatistic.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MetricStatistic where
    toEnum i = case i of
        0 -> Average
        1 -> Maximum
        2 -> Minimum
        3 -> SampleCount
        4 -> Sum
        _ -> (error . showText) $ "Unknown index for MetricStatistic: " <> toText i
    fromEnum x = case x of
        Average -> 0
        Maximum -> 1
        Minimum -> 2
        SampleCount -> 3
        Sum -> 4
        MetricStatistic' name -> (error . showText) $ "Unknown MetricStatistic: " <> original name

-- | Represents the bounds of /known/ $MetricStatistic.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MetricStatistic where
    minBound = Average
    maxBound = Sum

instance Hashable     MetricStatistic
instance NFData       MetricStatistic
instance ToByteString MetricStatistic
instance ToQuery      MetricStatistic
instance ToHeader     MetricStatistic

instance FromXML MetricStatistic where
    parseXML = parseXMLText "MetricStatistic"
