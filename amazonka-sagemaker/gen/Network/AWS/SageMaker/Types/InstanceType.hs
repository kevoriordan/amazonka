{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.InstanceType (
  InstanceType (
    ..
    , ITMl_C4_2XLarge
    , ITMl_C4_4XLarge
    , ITMl_C4_8XLarge
    , ITMl_C4_XLarge
    , ITMl_C5_18XLarge
    , ITMl_C5_2XLarge
    , ITMl_C5_4XLarge
    , ITMl_C5_9XLarge
    , ITMl_C5_XLarge
    , ITMl_C5d_18XLarge
    , ITMl_C5d_2XLarge
    , ITMl_C5d_4XLarge
    , ITMl_C5d_9XLarge
    , ITMl_C5d_XLarge
    , ITMl_M4_10XLarge
    , ITMl_M4_16XLarge
    , ITMl_M4_2XLarge
    , ITMl_M4_4XLarge
    , ITMl_M4_XLarge
    , ITMl_M5_12XLarge
    , ITMl_M5_24XLarge
    , ITMl_M5_2XLarge
    , ITMl_M5_4XLarge
    , ITMl_M5_XLarge
    , ITMl_P2_16XLarge
    , ITMl_P2_8XLarge
    , ITMl_P2_XLarge
    , ITMl_P3_16XLarge
    , ITMl_P3_2XLarge
    , ITMl_P3_8XLarge
    , ITMl_T2_2XLarge
    , ITMl_T2_Large
    , ITMl_T2_Medium
    , ITMl_T2_XLarge
    , ITMl_T3_2XLarge
    , ITMl_T3_Large
    , ITMl_T3_Medium
    , ITMl_T3_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceType = InstanceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ITMl_C4_2XLarge :: InstanceType
pattern ITMl_C4_2XLarge = InstanceType' "ml.c4.2xlarge"

pattern ITMl_C4_4XLarge :: InstanceType
pattern ITMl_C4_4XLarge = InstanceType' "ml.c4.4xlarge"

pattern ITMl_C4_8XLarge :: InstanceType
pattern ITMl_C4_8XLarge = InstanceType' "ml.c4.8xlarge"

pattern ITMl_C4_XLarge :: InstanceType
pattern ITMl_C4_XLarge = InstanceType' "ml.c4.xlarge"

pattern ITMl_C5_18XLarge :: InstanceType
pattern ITMl_C5_18XLarge = InstanceType' "ml.c5.18xlarge"

pattern ITMl_C5_2XLarge :: InstanceType
pattern ITMl_C5_2XLarge = InstanceType' "ml.c5.2xlarge"

pattern ITMl_C5_4XLarge :: InstanceType
pattern ITMl_C5_4XLarge = InstanceType' "ml.c5.4xlarge"

pattern ITMl_C5_9XLarge :: InstanceType
pattern ITMl_C5_9XLarge = InstanceType' "ml.c5.9xlarge"

pattern ITMl_C5_XLarge :: InstanceType
pattern ITMl_C5_XLarge = InstanceType' "ml.c5.xlarge"

pattern ITMl_C5d_18XLarge :: InstanceType
pattern ITMl_C5d_18XLarge = InstanceType' "ml.c5d.18xlarge"

pattern ITMl_C5d_2XLarge :: InstanceType
pattern ITMl_C5d_2XLarge = InstanceType' "ml.c5d.2xlarge"

pattern ITMl_C5d_4XLarge :: InstanceType
pattern ITMl_C5d_4XLarge = InstanceType' "ml.c5d.4xlarge"

pattern ITMl_C5d_9XLarge :: InstanceType
pattern ITMl_C5d_9XLarge = InstanceType' "ml.c5d.9xlarge"

pattern ITMl_C5d_XLarge :: InstanceType
pattern ITMl_C5d_XLarge = InstanceType' "ml.c5d.xlarge"

pattern ITMl_M4_10XLarge :: InstanceType
pattern ITMl_M4_10XLarge = InstanceType' "ml.m4.10xlarge"

pattern ITMl_M4_16XLarge :: InstanceType
pattern ITMl_M4_16XLarge = InstanceType' "ml.m4.16xlarge"

pattern ITMl_M4_2XLarge :: InstanceType
pattern ITMl_M4_2XLarge = InstanceType' "ml.m4.2xlarge"

pattern ITMl_M4_4XLarge :: InstanceType
pattern ITMl_M4_4XLarge = InstanceType' "ml.m4.4xlarge"

pattern ITMl_M4_XLarge :: InstanceType
pattern ITMl_M4_XLarge = InstanceType' "ml.m4.xlarge"

pattern ITMl_M5_12XLarge :: InstanceType
pattern ITMl_M5_12XLarge = InstanceType' "ml.m5.12xlarge"

pattern ITMl_M5_24XLarge :: InstanceType
pattern ITMl_M5_24XLarge = InstanceType' "ml.m5.24xlarge"

pattern ITMl_M5_2XLarge :: InstanceType
pattern ITMl_M5_2XLarge = InstanceType' "ml.m5.2xlarge"

pattern ITMl_M5_4XLarge :: InstanceType
pattern ITMl_M5_4XLarge = InstanceType' "ml.m5.4xlarge"

pattern ITMl_M5_XLarge :: InstanceType
pattern ITMl_M5_XLarge = InstanceType' "ml.m5.xlarge"

pattern ITMl_P2_16XLarge :: InstanceType
pattern ITMl_P2_16XLarge = InstanceType' "ml.p2.16xlarge"

pattern ITMl_P2_8XLarge :: InstanceType
pattern ITMl_P2_8XLarge = InstanceType' "ml.p2.8xlarge"

pattern ITMl_P2_XLarge :: InstanceType
pattern ITMl_P2_XLarge = InstanceType' "ml.p2.xlarge"

pattern ITMl_P3_16XLarge :: InstanceType
pattern ITMl_P3_16XLarge = InstanceType' "ml.p3.16xlarge"

pattern ITMl_P3_2XLarge :: InstanceType
pattern ITMl_P3_2XLarge = InstanceType' "ml.p3.2xlarge"

pattern ITMl_P3_8XLarge :: InstanceType
pattern ITMl_P3_8XLarge = InstanceType' "ml.p3.8xlarge"

pattern ITMl_T2_2XLarge :: InstanceType
pattern ITMl_T2_2XLarge = InstanceType' "ml.t2.2xlarge"

pattern ITMl_T2_Large :: InstanceType
pattern ITMl_T2_Large = InstanceType' "ml.t2.large"

pattern ITMl_T2_Medium :: InstanceType
pattern ITMl_T2_Medium = InstanceType' "ml.t2.medium"

pattern ITMl_T2_XLarge :: InstanceType
pattern ITMl_T2_XLarge = InstanceType' "ml.t2.xlarge"

pattern ITMl_T3_2XLarge :: InstanceType
pattern ITMl_T3_2XLarge = InstanceType' "ml.t3.2xlarge"

pattern ITMl_T3_Large :: InstanceType
pattern ITMl_T3_Large = InstanceType' "ml.t3.large"

pattern ITMl_T3_Medium :: InstanceType
pattern ITMl_T3_Medium = InstanceType' "ml.t3.medium"

pattern ITMl_T3_XLarge :: InstanceType
pattern ITMl_T3_XLarge = InstanceType' "ml.t3.xlarge"

{-# COMPLETE
  ITMl_C4_2XLarge,
  ITMl_C4_4XLarge,
  ITMl_C4_8XLarge,
  ITMl_C4_XLarge,
  ITMl_C5_18XLarge,
  ITMl_C5_2XLarge,
  ITMl_C5_4XLarge,
  ITMl_C5_9XLarge,
  ITMl_C5_XLarge,
  ITMl_C5d_18XLarge,
  ITMl_C5d_2XLarge,
  ITMl_C5d_4XLarge,
  ITMl_C5d_9XLarge,
  ITMl_C5d_XLarge,
  ITMl_M4_10XLarge,
  ITMl_M4_16XLarge,
  ITMl_M4_2XLarge,
  ITMl_M4_4XLarge,
  ITMl_M4_XLarge,
  ITMl_M5_12XLarge,
  ITMl_M5_24XLarge,
  ITMl_M5_2XLarge,
  ITMl_M5_4XLarge,
  ITMl_M5_XLarge,
  ITMl_P2_16XLarge,
  ITMl_P2_8XLarge,
  ITMl_P2_XLarge,
  ITMl_P3_16XLarge,
  ITMl_P3_2XLarge,
  ITMl_P3_8XLarge,
  ITMl_T2_2XLarge,
  ITMl_T2_Large,
  ITMl_T2_Medium,
  ITMl_T2_XLarge,
  ITMl_T3_2XLarge,
  ITMl_T3_Large,
  ITMl_T3_Medium,
  ITMl_T3_XLarge,
  InstanceType' #-}

instance FromText InstanceType where
    parser = (InstanceType' . mk) <$> takeText

instance ToText InstanceType where
    toText (InstanceType' ci) = original ci

-- | Represents an enum of /known/ $InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceType where
    toEnum i = case i of
        0 -> ITMl_C4_2XLarge
        1 -> ITMl_C4_4XLarge
        2 -> ITMl_C4_8XLarge
        3 -> ITMl_C4_XLarge
        4 -> ITMl_C5_18XLarge
        5 -> ITMl_C5_2XLarge
        6 -> ITMl_C5_4XLarge
        7 -> ITMl_C5_9XLarge
        8 -> ITMl_C5_XLarge
        9 -> ITMl_C5d_18XLarge
        10 -> ITMl_C5d_2XLarge
        11 -> ITMl_C5d_4XLarge
        12 -> ITMl_C5d_9XLarge
        13 -> ITMl_C5d_XLarge
        14 -> ITMl_M4_10XLarge
        15 -> ITMl_M4_16XLarge
        16 -> ITMl_M4_2XLarge
        17 -> ITMl_M4_4XLarge
        18 -> ITMl_M4_XLarge
        19 -> ITMl_M5_12XLarge
        20 -> ITMl_M5_24XLarge
        21 -> ITMl_M5_2XLarge
        22 -> ITMl_M5_4XLarge
        23 -> ITMl_M5_XLarge
        24 -> ITMl_P2_16XLarge
        25 -> ITMl_P2_8XLarge
        26 -> ITMl_P2_XLarge
        27 -> ITMl_P3_16XLarge
        28 -> ITMl_P3_2XLarge
        29 -> ITMl_P3_8XLarge
        30 -> ITMl_T2_2XLarge
        31 -> ITMl_T2_Large
        32 -> ITMl_T2_Medium
        33 -> ITMl_T2_XLarge
        34 -> ITMl_T3_2XLarge
        35 -> ITMl_T3_Large
        36 -> ITMl_T3_Medium
        37 -> ITMl_T3_XLarge
        _ -> (error . showText) $ "Unknown index for InstanceType: " <> toText i
    fromEnum x = case x of
        ITMl_C4_2XLarge -> 0
        ITMl_C4_4XLarge -> 1
        ITMl_C4_8XLarge -> 2
        ITMl_C4_XLarge -> 3
        ITMl_C5_18XLarge -> 4
        ITMl_C5_2XLarge -> 5
        ITMl_C5_4XLarge -> 6
        ITMl_C5_9XLarge -> 7
        ITMl_C5_XLarge -> 8
        ITMl_C5d_18XLarge -> 9
        ITMl_C5d_2XLarge -> 10
        ITMl_C5d_4XLarge -> 11
        ITMl_C5d_9XLarge -> 12
        ITMl_C5d_XLarge -> 13
        ITMl_M4_10XLarge -> 14
        ITMl_M4_16XLarge -> 15
        ITMl_M4_2XLarge -> 16
        ITMl_M4_4XLarge -> 17
        ITMl_M4_XLarge -> 18
        ITMl_M5_12XLarge -> 19
        ITMl_M5_24XLarge -> 20
        ITMl_M5_2XLarge -> 21
        ITMl_M5_4XLarge -> 22
        ITMl_M5_XLarge -> 23
        ITMl_P2_16XLarge -> 24
        ITMl_P2_8XLarge -> 25
        ITMl_P2_XLarge -> 26
        ITMl_P3_16XLarge -> 27
        ITMl_P3_2XLarge -> 28
        ITMl_P3_8XLarge -> 29
        ITMl_T2_2XLarge -> 30
        ITMl_T2_Large -> 31
        ITMl_T2_Medium -> 32
        ITMl_T2_XLarge -> 33
        ITMl_T3_2XLarge -> 34
        ITMl_T3_Large -> 35
        ITMl_T3_Medium -> 36
        ITMl_T3_XLarge -> 37
        InstanceType' name -> (error . showText) $ "Unknown InstanceType: " <> original name

-- | Represents the bounds of /known/ $InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceType where
    minBound = ITMl_C4_2XLarge
    maxBound = ITMl_T3_XLarge

instance Hashable     InstanceType
instance NFData       InstanceType
instance ToByteString InstanceType
instance ToQuery      InstanceType
instance ToHeader     InstanceType

instance ToJSON InstanceType where
    toJSON = toJSONText

instance FromJSON InstanceType where
    parseJSON = parseJSONText "InstanceType"
