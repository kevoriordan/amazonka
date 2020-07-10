{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.PaymentOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.PaymentOption (
  PaymentOption (
    ..
    , AllUpfront
    , HeavyUtilization
    , LightUtilization
    , MediumUtilization
    , NoUpfront
    , PartialUpfront
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PaymentOption = PaymentOption' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern AllUpfront :: PaymentOption
pattern AllUpfront = PaymentOption' "ALL_UPFRONT"

pattern HeavyUtilization :: PaymentOption
pattern HeavyUtilization = PaymentOption' "HEAVY_UTILIZATION"

pattern LightUtilization :: PaymentOption
pattern LightUtilization = PaymentOption' "LIGHT_UTILIZATION"

pattern MediumUtilization :: PaymentOption
pattern MediumUtilization = PaymentOption' "MEDIUM_UTILIZATION"

pattern NoUpfront :: PaymentOption
pattern NoUpfront = PaymentOption' "NO_UPFRONT"

pattern PartialUpfront :: PaymentOption
pattern PartialUpfront = PaymentOption' "PARTIAL_UPFRONT"

{-# COMPLETE
  AllUpfront,
  HeavyUtilization,
  LightUtilization,
  MediumUtilization,
  NoUpfront,
  PartialUpfront,
  PaymentOption' #-}

instance FromText PaymentOption where
    parser = (PaymentOption' . mk) <$> takeText

instance ToText PaymentOption where
    toText (PaymentOption' ci) = original ci

-- | Represents an enum of /known/ $PaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PaymentOption where
    toEnum i = case i of
        0 -> AllUpfront
        1 -> HeavyUtilization
        2 -> LightUtilization
        3 -> MediumUtilization
        4 -> NoUpfront
        5 -> PartialUpfront
        _ -> (error . showText) $ "Unknown index for PaymentOption: " <> toText i
    fromEnum x = case x of
        AllUpfront -> 0
        HeavyUtilization -> 1
        LightUtilization -> 2
        MediumUtilization -> 3
        NoUpfront -> 4
        PartialUpfront -> 5
        PaymentOption' name -> (error . showText) $ "Unknown PaymentOption: " <> original name

-- | Represents the bounds of /known/ $PaymentOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PaymentOption where
    minBound = AllUpfront
    maxBound = PartialUpfront

instance Hashable     PaymentOption
instance NFData       PaymentOption
instance ToByteString PaymentOption
instance ToQuery      PaymentOption
instance ToHeader     PaymentOption

instance ToJSON PaymentOption where
    toJSON = toJSONText

instance FromJSON PaymentOption where
    parseJSON = parseJSONText "PaymentOption"
