{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType (
  ProductionVariantAcceleratorType (
    ..
    , PVATMl_EIA1_Large
    , PVATMl_EIA1_Medium
    , PVATMl_EIA1_XLarge
    , PVATMl_EIA2_Large
    , PVATMl_EIA2_Medium
    , PVATMl_EIA2_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProductionVariantAcceleratorType = ProductionVariantAcceleratorType' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern PVATMl_EIA1_Large :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_Large = ProductionVariantAcceleratorType' "ml.eia1.large"

pattern PVATMl_EIA1_Medium :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_Medium = ProductionVariantAcceleratorType' "ml.eia1.medium"

pattern PVATMl_EIA1_XLarge :: ProductionVariantAcceleratorType
pattern PVATMl_EIA1_XLarge = ProductionVariantAcceleratorType' "ml.eia1.xlarge"

pattern PVATMl_EIA2_Large :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_Large = ProductionVariantAcceleratorType' "ml.eia2.large"

pattern PVATMl_EIA2_Medium :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_Medium = ProductionVariantAcceleratorType' "ml.eia2.medium"

pattern PVATMl_EIA2_XLarge :: ProductionVariantAcceleratorType
pattern PVATMl_EIA2_XLarge = ProductionVariantAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  PVATMl_EIA1_Large,
  PVATMl_EIA1_Medium,
  PVATMl_EIA1_XLarge,
  PVATMl_EIA2_Large,
  PVATMl_EIA2_Medium,
  PVATMl_EIA2_XLarge,
  ProductionVariantAcceleratorType' #-}

instance FromText ProductionVariantAcceleratorType where
    parser = (ProductionVariantAcceleratorType' . mk) <$> takeText

instance ToText ProductionVariantAcceleratorType where
    toText (ProductionVariantAcceleratorType' ci) = original ci

-- | Represents an enum of /known/ $ProductionVariantAcceleratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProductionVariantAcceleratorType where
    toEnum i = case i of
        0 -> PVATMl_EIA1_Large
        1 -> PVATMl_EIA1_Medium
        2 -> PVATMl_EIA1_XLarge
        3 -> PVATMl_EIA2_Large
        4 -> PVATMl_EIA2_Medium
        5 -> PVATMl_EIA2_XLarge
        _ -> (error . showText) $ "Unknown index for ProductionVariantAcceleratorType: " <> toText i
    fromEnum x = case x of
        PVATMl_EIA1_Large -> 0
        PVATMl_EIA1_Medium -> 1
        PVATMl_EIA1_XLarge -> 2
        PVATMl_EIA2_Large -> 3
        PVATMl_EIA2_Medium -> 4
        PVATMl_EIA2_XLarge -> 5
        ProductionVariantAcceleratorType' name -> (error . showText) $ "Unknown ProductionVariantAcceleratorType: " <> original name

-- | Represents the bounds of /known/ $ProductionVariantAcceleratorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProductionVariantAcceleratorType where
    minBound = PVATMl_EIA1_Large
    maxBound = PVATMl_EIA2_XLarge

instance Hashable     ProductionVariantAcceleratorType
instance NFData       ProductionVariantAcceleratorType
instance ToByteString ProductionVariantAcceleratorType
instance ToQuery      ProductionVariantAcceleratorType
instance ToHeader     ProductionVariantAcceleratorType

instance ToJSON ProductionVariantAcceleratorType where
    toJSON = toJSONText

instance FromJSON ProductionVariantAcceleratorType where
    parseJSON = parseJSONText "ProductionVariantAcceleratorType"
