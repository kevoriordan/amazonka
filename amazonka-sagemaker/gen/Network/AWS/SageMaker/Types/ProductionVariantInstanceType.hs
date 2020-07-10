{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProductionVariantInstanceType (
  ProductionVariantInstanceType (
    ..
    , PVITMl_C4_2XLarge
    , PVITMl_C4_4XLarge
    , PVITMl_C4_8XLarge
    , PVITMl_C4_Large
    , PVITMl_C4_XLarge
    , PVITMl_C5_18XLarge
    , PVITMl_C5_2XLarge
    , PVITMl_C5_4XLarge
    , PVITMl_C5_9XLarge
    , PVITMl_C5_Large
    , PVITMl_C5_XLarge
    , PVITMl_C5d_18XLarge
    , PVITMl_C5d_2XLarge
    , PVITMl_C5d_4XLarge
    , PVITMl_C5d_9XLarge
    , PVITMl_C5d_Large
    , PVITMl_C5d_XLarge
    , PVITMl_G4dn_12XLarge
    , PVITMl_G4dn_16XLarge
    , PVITMl_G4dn_2XLarge
    , PVITMl_G4dn_4XLarge
    , PVITMl_G4dn_8XLarge
    , PVITMl_G4dn_XLarge
    , PVITMl_INF1_24XLarge
    , PVITMl_INF1_2XLarge
    , PVITMl_INF1_6XLarge
    , PVITMl_INF1_XLarge
    , PVITMl_M4_10XLarge
    , PVITMl_M4_16XLarge
    , PVITMl_M4_2XLarge
    , PVITMl_M4_4XLarge
    , PVITMl_M4_XLarge
    , PVITMl_M5_12XLarge
    , PVITMl_M5_24XLarge
    , PVITMl_M5_2XLarge
    , PVITMl_M5_4XLarge
    , PVITMl_M5_Large
    , PVITMl_M5_XLarge
    , PVITMl_M5d_12XLarge
    , PVITMl_M5d_24XLarge
    , PVITMl_M5d_2XLarge
    , PVITMl_M5d_4XLarge
    , PVITMl_M5d_Large
    , PVITMl_M5d_XLarge
    , PVITMl_P2_16XLarge
    , PVITMl_P2_8XLarge
    , PVITMl_P2_XLarge
    , PVITMl_P3_16XLarge
    , PVITMl_P3_2XLarge
    , PVITMl_P3_8XLarge
    , PVITMl_R5_12XLarge
    , PVITMl_R5_24XLarge
    , PVITMl_R5_2XLarge
    , PVITMl_R5_4XLarge
    , PVITMl_R5_Large
    , PVITMl_R5_XLarge
    , PVITMl_R5d_12XLarge
    , PVITMl_R5d_24XLarge
    , PVITMl_R5d_2XLarge
    , PVITMl_R5d_4XLarge
    , PVITMl_R5d_Large
    , PVITMl_R5d_XLarge
    , PVITMl_T2_2XLarge
    , PVITMl_T2_Large
    , PVITMl_T2_Medium
    , PVITMl_T2_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProductionVariantInstanceType = ProductionVariantInstanceType' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern PVITMl_C4_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_2XLarge = ProductionVariantInstanceType' "ml.c4.2xlarge"

pattern PVITMl_C4_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_4XLarge = ProductionVariantInstanceType' "ml.c4.4xlarge"

pattern PVITMl_C4_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_8XLarge = ProductionVariantInstanceType' "ml.c4.8xlarge"

pattern PVITMl_C4_Large :: ProductionVariantInstanceType
pattern PVITMl_C4_Large = ProductionVariantInstanceType' "ml.c4.large"

pattern PVITMl_C4_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C4_XLarge = ProductionVariantInstanceType' "ml.c4.xlarge"

pattern PVITMl_C5_18XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_18XLarge = ProductionVariantInstanceType' "ml.c5.18xlarge"

pattern PVITMl_C5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_2XLarge = ProductionVariantInstanceType' "ml.c5.2xlarge"

pattern PVITMl_C5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_4XLarge = ProductionVariantInstanceType' "ml.c5.4xlarge"

pattern PVITMl_C5_9XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_9XLarge = ProductionVariantInstanceType' "ml.c5.9xlarge"

pattern PVITMl_C5_Large :: ProductionVariantInstanceType
pattern PVITMl_C5_Large = ProductionVariantInstanceType' "ml.c5.large"

pattern PVITMl_C5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5_XLarge = ProductionVariantInstanceType' "ml.c5.xlarge"

pattern PVITMl_C5d_18XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_18XLarge = ProductionVariantInstanceType' "ml.c5d.18xlarge"

pattern PVITMl_C5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_2XLarge = ProductionVariantInstanceType' "ml.c5d.2xlarge"

pattern PVITMl_C5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_4XLarge = ProductionVariantInstanceType' "ml.c5d.4xlarge"

pattern PVITMl_C5d_9XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_9XLarge = ProductionVariantInstanceType' "ml.c5d.9xlarge"

pattern PVITMl_C5d_Large :: ProductionVariantInstanceType
pattern PVITMl_C5d_Large = ProductionVariantInstanceType' "ml.c5d.large"

pattern PVITMl_C5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_C5d_XLarge = ProductionVariantInstanceType' "ml.c5d.xlarge"

pattern PVITMl_G4dn_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_12XLarge = ProductionVariantInstanceType' "ml.g4dn.12xlarge"

pattern PVITMl_G4dn_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_16XLarge = ProductionVariantInstanceType' "ml.g4dn.16xlarge"

pattern PVITMl_G4dn_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_2XLarge = ProductionVariantInstanceType' "ml.g4dn.2xlarge"

pattern PVITMl_G4dn_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_4XLarge = ProductionVariantInstanceType' "ml.g4dn.4xlarge"

pattern PVITMl_G4dn_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_8XLarge = ProductionVariantInstanceType' "ml.g4dn.8xlarge"

pattern PVITMl_G4dn_XLarge :: ProductionVariantInstanceType
pattern PVITMl_G4dn_XLarge = ProductionVariantInstanceType' "ml.g4dn.xlarge"

pattern PVITMl_INF1_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_24XLarge = ProductionVariantInstanceType' "ml.inf1.24xlarge"

pattern PVITMl_INF1_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_2XLarge = ProductionVariantInstanceType' "ml.inf1.2xlarge"

pattern PVITMl_INF1_6XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_6XLarge = ProductionVariantInstanceType' "ml.inf1.6xlarge"

pattern PVITMl_INF1_XLarge :: ProductionVariantInstanceType
pattern PVITMl_INF1_XLarge = ProductionVariantInstanceType' "ml.inf1.xlarge"

pattern PVITMl_M4_10XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_10XLarge = ProductionVariantInstanceType' "ml.m4.10xlarge"

pattern PVITMl_M4_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_16XLarge = ProductionVariantInstanceType' "ml.m4.16xlarge"

pattern PVITMl_M4_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_2XLarge = ProductionVariantInstanceType' "ml.m4.2xlarge"

pattern PVITMl_M4_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_4XLarge = ProductionVariantInstanceType' "ml.m4.4xlarge"

pattern PVITMl_M4_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M4_XLarge = ProductionVariantInstanceType' "ml.m4.xlarge"

pattern PVITMl_M5_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_12XLarge = ProductionVariantInstanceType' "ml.m5.12xlarge"

pattern PVITMl_M5_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_24XLarge = ProductionVariantInstanceType' "ml.m5.24xlarge"

pattern PVITMl_M5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_2XLarge = ProductionVariantInstanceType' "ml.m5.2xlarge"

pattern PVITMl_M5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_4XLarge = ProductionVariantInstanceType' "ml.m5.4xlarge"

pattern PVITMl_M5_Large :: ProductionVariantInstanceType
pattern PVITMl_M5_Large = ProductionVariantInstanceType' "ml.m5.large"

pattern PVITMl_M5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5_XLarge = ProductionVariantInstanceType' "ml.m5.xlarge"

pattern PVITMl_M5d_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_12XLarge = ProductionVariantInstanceType' "ml.m5d.12xlarge"

pattern PVITMl_M5d_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_24XLarge = ProductionVariantInstanceType' "ml.m5d.24xlarge"

pattern PVITMl_M5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_2XLarge = ProductionVariantInstanceType' "ml.m5d.2xlarge"

pattern PVITMl_M5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_4XLarge = ProductionVariantInstanceType' "ml.m5d.4xlarge"

pattern PVITMl_M5d_Large :: ProductionVariantInstanceType
pattern PVITMl_M5d_Large = ProductionVariantInstanceType' "ml.m5d.large"

pattern PVITMl_M5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_M5d_XLarge = ProductionVariantInstanceType' "ml.m5d.xlarge"

pattern PVITMl_P2_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_16XLarge = ProductionVariantInstanceType' "ml.p2.16xlarge"

pattern PVITMl_P2_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_8XLarge = ProductionVariantInstanceType' "ml.p2.8xlarge"

pattern PVITMl_P2_XLarge :: ProductionVariantInstanceType
pattern PVITMl_P2_XLarge = ProductionVariantInstanceType' "ml.p2.xlarge"

pattern PVITMl_P3_16XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_16XLarge = ProductionVariantInstanceType' "ml.p3.16xlarge"

pattern PVITMl_P3_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_2XLarge = ProductionVariantInstanceType' "ml.p3.2xlarge"

pattern PVITMl_P3_8XLarge :: ProductionVariantInstanceType
pattern PVITMl_P3_8XLarge = ProductionVariantInstanceType' "ml.p3.8xlarge"

pattern PVITMl_R5_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_12XLarge = ProductionVariantInstanceType' "ml.r5.12xlarge"

pattern PVITMl_R5_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_24XLarge = ProductionVariantInstanceType' "ml.r5.24xlarge"

pattern PVITMl_R5_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_2XLarge = ProductionVariantInstanceType' "ml.r5.2xlarge"

pattern PVITMl_R5_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_4XLarge = ProductionVariantInstanceType' "ml.r5.4xlarge"

pattern PVITMl_R5_Large :: ProductionVariantInstanceType
pattern PVITMl_R5_Large = ProductionVariantInstanceType' "ml.r5.large"

pattern PVITMl_R5_XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5_XLarge = ProductionVariantInstanceType' "ml.r5.xlarge"

pattern PVITMl_R5d_12XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_12XLarge = ProductionVariantInstanceType' "ml.r5d.12xlarge"

pattern PVITMl_R5d_24XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_24XLarge = ProductionVariantInstanceType' "ml.r5d.24xlarge"

pattern PVITMl_R5d_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_2XLarge = ProductionVariantInstanceType' "ml.r5d.2xlarge"

pattern PVITMl_R5d_4XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_4XLarge = ProductionVariantInstanceType' "ml.r5d.4xlarge"

pattern PVITMl_R5d_Large :: ProductionVariantInstanceType
pattern PVITMl_R5d_Large = ProductionVariantInstanceType' "ml.r5d.large"

pattern PVITMl_R5d_XLarge :: ProductionVariantInstanceType
pattern PVITMl_R5d_XLarge = ProductionVariantInstanceType' "ml.r5d.xlarge"

pattern PVITMl_T2_2XLarge :: ProductionVariantInstanceType
pattern PVITMl_T2_2XLarge = ProductionVariantInstanceType' "ml.t2.2xlarge"

pattern PVITMl_T2_Large :: ProductionVariantInstanceType
pattern PVITMl_T2_Large = ProductionVariantInstanceType' "ml.t2.large"

pattern PVITMl_T2_Medium :: ProductionVariantInstanceType
pattern PVITMl_T2_Medium = ProductionVariantInstanceType' "ml.t2.medium"

pattern PVITMl_T2_XLarge :: ProductionVariantInstanceType
pattern PVITMl_T2_XLarge = ProductionVariantInstanceType' "ml.t2.xlarge"

{-# COMPLETE
  PVITMl_C4_2XLarge,
  PVITMl_C4_4XLarge,
  PVITMl_C4_8XLarge,
  PVITMl_C4_Large,
  PVITMl_C4_XLarge,
  PVITMl_C5_18XLarge,
  PVITMl_C5_2XLarge,
  PVITMl_C5_4XLarge,
  PVITMl_C5_9XLarge,
  PVITMl_C5_Large,
  PVITMl_C5_XLarge,
  PVITMl_C5d_18XLarge,
  PVITMl_C5d_2XLarge,
  PVITMl_C5d_4XLarge,
  PVITMl_C5d_9XLarge,
  PVITMl_C5d_Large,
  PVITMl_C5d_XLarge,
  PVITMl_G4dn_12XLarge,
  PVITMl_G4dn_16XLarge,
  PVITMl_G4dn_2XLarge,
  PVITMl_G4dn_4XLarge,
  PVITMl_G4dn_8XLarge,
  PVITMl_G4dn_XLarge,
  PVITMl_INF1_24XLarge,
  PVITMl_INF1_2XLarge,
  PVITMl_INF1_6XLarge,
  PVITMl_INF1_XLarge,
  PVITMl_M4_10XLarge,
  PVITMl_M4_16XLarge,
  PVITMl_M4_2XLarge,
  PVITMl_M4_4XLarge,
  PVITMl_M4_XLarge,
  PVITMl_M5_12XLarge,
  PVITMl_M5_24XLarge,
  PVITMl_M5_2XLarge,
  PVITMl_M5_4XLarge,
  PVITMl_M5_Large,
  PVITMl_M5_XLarge,
  PVITMl_M5d_12XLarge,
  PVITMl_M5d_24XLarge,
  PVITMl_M5d_2XLarge,
  PVITMl_M5d_4XLarge,
  PVITMl_M5d_Large,
  PVITMl_M5d_XLarge,
  PVITMl_P2_16XLarge,
  PVITMl_P2_8XLarge,
  PVITMl_P2_XLarge,
  PVITMl_P3_16XLarge,
  PVITMl_P3_2XLarge,
  PVITMl_P3_8XLarge,
  PVITMl_R5_12XLarge,
  PVITMl_R5_24XLarge,
  PVITMl_R5_2XLarge,
  PVITMl_R5_4XLarge,
  PVITMl_R5_Large,
  PVITMl_R5_XLarge,
  PVITMl_R5d_12XLarge,
  PVITMl_R5d_24XLarge,
  PVITMl_R5d_2XLarge,
  PVITMl_R5d_4XLarge,
  PVITMl_R5d_Large,
  PVITMl_R5d_XLarge,
  PVITMl_T2_2XLarge,
  PVITMl_T2_Large,
  PVITMl_T2_Medium,
  PVITMl_T2_XLarge,
  ProductionVariantInstanceType' #-}

instance FromText ProductionVariantInstanceType where
    parser = (ProductionVariantInstanceType' . mk) <$> takeText

instance ToText ProductionVariantInstanceType where
    toText (ProductionVariantInstanceType' ci) = original ci

-- | Represents an enum of /known/ $ProductionVariantInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProductionVariantInstanceType where
    toEnum i = case i of
        0 -> PVITMl_C4_2XLarge
        1 -> PVITMl_C4_4XLarge
        2 -> PVITMl_C4_8XLarge
        3 -> PVITMl_C4_Large
        4 -> PVITMl_C4_XLarge
        5 -> PVITMl_C5_18XLarge
        6 -> PVITMl_C5_2XLarge
        7 -> PVITMl_C5_4XLarge
        8 -> PVITMl_C5_9XLarge
        9 -> PVITMl_C5_Large
        10 -> PVITMl_C5_XLarge
        11 -> PVITMl_C5d_18XLarge
        12 -> PVITMl_C5d_2XLarge
        13 -> PVITMl_C5d_4XLarge
        14 -> PVITMl_C5d_9XLarge
        15 -> PVITMl_C5d_Large
        16 -> PVITMl_C5d_XLarge
        17 -> PVITMl_G4dn_12XLarge
        18 -> PVITMl_G4dn_16XLarge
        19 -> PVITMl_G4dn_2XLarge
        20 -> PVITMl_G4dn_4XLarge
        21 -> PVITMl_G4dn_8XLarge
        22 -> PVITMl_G4dn_XLarge
        23 -> PVITMl_INF1_24XLarge
        24 -> PVITMl_INF1_2XLarge
        25 -> PVITMl_INF1_6XLarge
        26 -> PVITMl_INF1_XLarge
        27 -> PVITMl_M4_10XLarge
        28 -> PVITMl_M4_16XLarge
        29 -> PVITMl_M4_2XLarge
        30 -> PVITMl_M4_4XLarge
        31 -> PVITMl_M4_XLarge
        32 -> PVITMl_M5_12XLarge
        33 -> PVITMl_M5_24XLarge
        34 -> PVITMl_M5_2XLarge
        35 -> PVITMl_M5_4XLarge
        36 -> PVITMl_M5_Large
        37 -> PVITMl_M5_XLarge
        38 -> PVITMl_M5d_12XLarge
        39 -> PVITMl_M5d_24XLarge
        40 -> PVITMl_M5d_2XLarge
        41 -> PVITMl_M5d_4XLarge
        42 -> PVITMl_M5d_Large
        43 -> PVITMl_M5d_XLarge
        44 -> PVITMl_P2_16XLarge
        45 -> PVITMl_P2_8XLarge
        46 -> PVITMl_P2_XLarge
        47 -> PVITMl_P3_16XLarge
        48 -> PVITMl_P3_2XLarge
        49 -> PVITMl_P3_8XLarge
        50 -> PVITMl_R5_12XLarge
        51 -> PVITMl_R5_24XLarge
        52 -> PVITMl_R5_2XLarge
        53 -> PVITMl_R5_4XLarge
        54 -> PVITMl_R5_Large
        55 -> PVITMl_R5_XLarge
        56 -> PVITMl_R5d_12XLarge
        57 -> PVITMl_R5d_24XLarge
        58 -> PVITMl_R5d_2XLarge
        59 -> PVITMl_R5d_4XLarge
        60 -> PVITMl_R5d_Large
        61 -> PVITMl_R5d_XLarge
        62 -> PVITMl_T2_2XLarge
        63 -> PVITMl_T2_Large
        64 -> PVITMl_T2_Medium
        65 -> PVITMl_T2_XLarge
        _ -> (error . showText) $ "Unknown index for ProductionVariantInstanceType: " <> toText i
    fromEnum x = case x of
        PVITMl_C4_2XLarge -> 0
        PVITMl_C4_4XLarge -> 1
        PVITMl_C4_8XLarge -> 2
        PVITMl_C4_Large -> 3
        PVITMl_C4_XLarge -> 4
        PVITMl_C5_18XLarge -> 5
        PVITMl_C5_2XLarge -> 6
        PVITMl_C5_4XLarge -> 7
        PVITMl_C5_9XLarge -> 8
        PVITMl_C5_Large -> 9
        PVITMl_C5_XLarge -> 10
        PVITMl_C5d_18XLarge -> 11
        PVITMl_C5d_2XLarge -> 12
        PVITMl_C5d_4XLarge -> 13
        PVITMl_C5d_9XLarge -> 14
        PVITMl_C5d_Large -> 15
        PVITMl_C5d_XLarge -> 16
        PVITMl_G4dn_12XLarge -> 17
        PVITMl_G4dn_16XLarge -> 18
        PVITMl_G4dn_2XLarge -> 19
        PVITMl_G4dn_4XLarge -> 20
        PVITMl_G4dn_8XLarge -> 21
        PVITMl_G4dn_XLarge -> 22
        PVITMl_INF1_24XLarge -> 23
        PVITMl_INF1_2XLarge -> 24
        PVITMl_INF1_6XLarge -> 25
        PVITMl_INF1_XLarge -> 26
        PVITMl_M4_10XLarge -> 27
        PVITMl_M4_16XLarge -> 28
        PVITMl_M4_2XLarge -> 29
        PVITMl_M4_4XLarge -> 30
        PVITMl_M4_XLarge -> 31
        PVITMl_M5_12XLarge -> 32
        PVITMl_M5_24XLarge -> 33
        PVITMl_M5_2XLarge -> 34
        PVITMl_M5_4XLarge -> 35
        PVITMl_M5_Large -> 36
        PVITMl_M5_XLarge -> 37
        PVITMl_M5d_12XLarge -> 38
        PVITMl_M5d_24XLarge -> 39
        PVITMl_M5d_2XLarge -> 40
        PVITMl_M5d_4XLarge -> 41
        PVITMl_M5d_Large -> 42
        PVITMl_M5d_XLarge -> 43
        PVITMl_P2_16XLarge -> 44
        PVITMl_P2_8XLarge -> 45
        PVITMl_P2_XLarge -> 46
        PVITMl_P3_16XLarge -> 47
        PVITMl_P3_2XLarge -> 48
        PVITMl_P3_8XLarge -> 49
        PVITMl_R5_12XLarge -> 50
        PVITMl_R5_24XLarge -> 51
        PVITMl_R5_2XLarge -> 52
        PVITMl_R5_4XLarge -> 53
        PVITMl_R5_Large -> 54
        PVITMl_R5_XLarge -> 55
        PVITMl_R5d_12XLarge -> 56
        PVITMl_R5d_24XLarge -> 57
        PVITMl_R5d_2XLarge -> 58
        PVITMl_R5d_4XLarge -> 59
        PVITMl_R5d_Large -> 60
        PVITMl_R5d_XLarge -> 61
        PVITMl_T2_2XLarge -> 62
        PVITMl_T2_Large -> 63
        PVITMl_T2_Medium -> 64
        PVITMl_T2_XLarge -> 65
        ProductionVariantInstanceType' name -> (error . showText) $ "Unknown ProductionVariantInstanceType: " <> original name

-- | Represents the bounds of /known/ $ProductionVariantInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProductionVariantInstanceType where
    minBound = PVITMl_C4_2XLarge
    maxBound = PVITMl_T2_XLarge

instance Hashable     ProductionVariantInstanceType
instance NFData       ProductionVariantInstanceType
instance ToByteString ProductionVariantInstanceType
instance ToQuery      ProductionVariantInstanceType
instance ToHeader     ProductionVariantInstanceType

instance ToJSON ProductionVariantInstanceType where
    toJSON = toJSONText

instance FromJSON ProductionVariantInstanceType where
    parseJSON = parseJSONText "ProductionVariantInstanceType"
