{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Metric
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Metric (
  Metric (
    ..
    , AmortizedCost
    , BlendedCost
    , NetAmortizedCost
    , NetUnblendedCost
    , NormalizedUsageAmount
    , UnblendedCost
    , UsageQuantity
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Metric = Metric' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern AmortizedCost :: Metric
pattern AmortizedCost = Metric' "AMORTIZED_COST"

pattern BlendedCost :: Metric
pattern BlendedCost = Metric' "BLENDED_COST"

pattern NetAmortizedCost :: Metric
pattern NetAmortizedCost = Metric' "NET_AMORTIZED_COST"

pattern NetUnblendedCost :: Metric
pattern NetUnblendedCost = Metric' "NET_UNBLENDED_COST"

pattern NormalizedUsageAmount :: Metric
pattern NormalizedUsageAmount = Metric' "NORMALIZED_USAGE_AMOUNT"

pattern UnblendedCost :: Metric
pattern UnblendedCost = Metric' "UNBLENDED_COST"

pattern UsageQuantity :: Metric
pattern UsageQuantity = Metric' "USAGE_QUANTITY"

{-# COMPLETE
  AmortizedCost,
  BlendedCost,
  NetAmortizedCost,
  NetUnblendedCost,
  NormalizedUsageAmount,
  UnblendedCost,
  UsageQuantity,
  Metric' #-}

instance FromText Metric where
    parser = (Metric' . mk) <$> takeText

instance ToText Metric where
    toText (Metric' ci) = original ci

-- | Represents an enum of /known/ $Metric.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Metric where
    toEnum i = case i of
        0 -> AmortizedCost
        1 -> BlendedCost
        2 -> NetAmortizedCost
        3 -> NetUnblendedCost
        4 -> NormalizedUsageAmount
        5 -> UnblendedCost
        6 -> UsageQuantity
        _ -> (error . showText) $ "Unknown index for Metric: " <> toText i
    fromEnum x = case x of
        AmortizedCost -> 0
        BlendedCost -> 1
        NetAmortizedCost -> 2
        NetUnblendedCost -> 3
        NormalizedUsageAmount -> 4
        UnblendedCost -> 5
        UsageQuantity -> 6
        Metric' name -> (error . showText) $ "Unknown Metric: " <> original name

-- | Represents the bounds of /known/ $Metric.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Metric where
    minBound = AmortizedCost
    maxBound = UsageQuantity

instance Hashable     Metric
instance NFData       Metric
instance ToByteString Metric
instance ToQuery      Metric
instance ToHeader     Metric

instance ToJSON Metric where
    toJSON = toJSONText
