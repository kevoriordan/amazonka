{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingInstanceType (
  ProcessingInstanceType (
    ..
    , PITMl_C4_2XLarge
    , PITMl_C4_4XLarge
    , PITMl_C4_8XLarge
    , PITMl_C4_XLarge
    , PITMl_C5_18XLarge
    , PITMl_C5_2XLarge
    , PITMl_C5_4XLarge
    , PITMl_C5_9XLarge
    , PITMl_C5_XLarge
    , PITMl_M4_10XLarge
    , PITMl_M4_16XLarge
    , PITMl_M4_2XLarge
    , PITMl_M4_4XLarge
    , PITMl_M4_XLarge
    , PITMl_M5_12XLarge
    , PITMl_M5_24XLarge
    , PITMl_M5_2XLarge
    , PITMl_M5_4XLarge
    , PITMl_M5_Large
    , PITMl_M5_XLarge
    , PITMl_P2_16XLarge
    , PITMl_P2_8XLarge
    , PITMl_P2_XLarge
    , PITMl_P3_16XLarge
    , PITMl_P3_2XLarge
    , PITMl_P3_8XLarge
    , PITMl_R5_12XLarge
    , PITMl_R5_16XLarge
    , PITMl_R5_24XLarge
    , PITMl_R5_2XLarge
    , PITMl_R5_4XLarge
    , PITMl_R5_8XLarge
    , PITMl_R5_Large
    , PITMl_R5_XLarge
    , PITMl_T3_2XLarge
    , PITMl_T3_Large
    , PITMl_T3_Medium
    , PITMl_T3_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingInstanceType = ProcessingInstanceType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern PITMl_C4_2XLarge :: ProcessingInstanceType
pattern PITMl_C4_2XLarge = ProcessingInstanceType' "ml.c4.2xlarge"

pattern PITMl_C4_4XLarge :: ProcessingInstanceType
pattern PITMl_C4_4XLarge = ProcessingInstanceType' "ml.c4.4xlarge"

pattern PITMl_C4_8XLarge :: ProcessingInstanceType
pattern PITMl_C4_8XLarge = ProcessingInstanceType' "ml.c4.8xlarge"

pattern PITMl_C4_XLarge :: ProcessingInstanceType
pattern PITMl_C4_XLarge = ProcessingInstanceType' "ml.c4.xlarge"

pattern PITMl_C5_18XLarge :: ProcessingInstanceType
pattern PITMl_C5_18XLarge = ProcessingInstanceType' "ml.c5.18xlarge"

pattern PITMl_C5_2XLarge :: ProcessingInstanceType
pattern PITMl_C5_2XLarge = ProcessingInstanceType' "ml.c5.2xlarge"

pattern PITMl_C5_4XLarge :: ProcessingInstanceType
pattern PITMl_C5_4XLarge = ProcessingInstanceType' "ml.c5.4xlarge"

pattern PITMl_C5_9XLarge :: ProcessingInstanceType
pattern PITMl_C5_9XLarge = ProcessingInstanceType' "ml.c5.9xlarge"

pattern PITMl_C5_XLarge :: ProcessingInstanceType
pattern PITMl_C5_XLarge = ProcessingInstanceType' "ml.c5.xlarge"

pattern PITMl_M4_10XLarge :: ProcessingInstanceType
pattern PITMl_M4_10XLarge = ProcessingInstanceType' "ml.m4.10xlarge"

pattern PITMl_M4_16XLarge :: ProcessingInstanceType
pattern PITMl_M4_16XLarge = ProcessingInstanceType' "ml.m4.16xlarge"

pattern PITMl_M4_2XLarge :: ProcessingInstanceType
pattern PITMl_M4_2XLarge = ProcessingInstanceType' "ml.m4.2xlarge"

pattern PITMl_M4_4XLarge :: ProcessingInstanceType
pattern PITMl_M4_4XLarge = ProcessingInstanceType' "ml.m4.4xlarge"

pattern PITMl_M4_XLarge :: ProcessingInstanceType
pattern PITMl_M4_XLarge = ProcessingInstanceType' "ml.m4.xlarge"

pattern PITMl_M5_12XLarge :: ProcessingInstanceType
pattern PITMl_M5_12XLarge = ProcessingInstanceType' "ml.m5.12xlarge"

pattern PITMl_M5_24XLarge :: ProcessingInstanceType
pattern PITMl_M5_24XLarge = ProcessingInstanceType' "ml.m5.24xlarge"

pattern PITMl_M5_2XLarge :: ProcessingInstanceType
pattern PITMl_M5_2XLarge = ProcessingInstanceType' "ml.m5.2xlarge"

pattern PITMl_M5_4XLarge :: ProcessingInstanceType
pattern PITMl_M5_4XLarge = ProcessingInstanceType' "ml.m5.4xlarge"

pattern PITMl_M5_Large :: ProcessingInstanceType
pattern PITMl_M5_Large = ProcessingInstanceType' "ml.m5.large"

pattern PITMl_M5_XLarge :: ProcessingInstanceType
pattern PITMl_M5_XLarge = ProcessingInstanceType' "ml.m5.xlarge"

pattern PITMl_P2_16XLarge :: ProcessingInstanceType
pattern PITMl_P2_16XLarge = ProcessingInstanceType' "ml.p2.16xlarge"

pattern PITMl_P2_8XLarge :: ProcessingInstanceType
pattern PITMl_P2_8XLarge = ProcessingInstanceType' "ml.p2.8xlarge"

pattern PITMl_P2_XLarge :: ProcessingInstanceType
pattern PITMl_P2_XLarge = ProcessingInstanceType' "ml.p2.xlarge"

pattern PITMl_P3_16XLarge :: ProcessingInstanceType
pattern PITMl_P3_16XLarge = ProcessingInstanceType' "ml.p3.16xlarge"

pattern PITMl_P3_2XLarge :: ProcessingInstanceType
pattern PITMl_P3_2XLarge = ProcessingInstanceType' "ml.p3.2xlarge"

pattern PITMl_P3_8XLarge :: ProcessingInstanceType
pattern PITMl_P3_8XLarge = ProcessingInstanceType' "ml.p3.8xlarge"

pattern PITMl_R5_12XLarge :: ProcessingInstanceType
pattern PITMl_R5_12XLarge = ProcessingInstanceType' "ml.r5.12xlarge"

pattern PITMl_R5_16XLarge :: ProcessingInstanceType
pattern PITMl_R5_16XLarge = ProcessingInstanceType' "ml.r5.16xlarge"

pattern PITMl_R5_24XLarge :: ProcessingInstanceType
pattern PITMl_R5_24XLarge = ProcessingInstanceType' "ml.r5.24xlarge"

pattern PITMl_R5_2XLarge :: ProcessingInstanceType
pattern PITMl_R5_2XLarge = ProcessingInstanceType' "ml.r5.2xlarge"

pattern PITMl_R5_4XLarge :: ProcessingInstanceType
pattern PITMl_R5_4XLarge = ProcessingInstanceType' "ml.r5.4xlarge"

pattern PITMl_R5_8XLarge :: ProcessingInstanceType
pattern PITMl_R5_8XLarge = ProcessingInstanceType' "ml.r5.8xlarge"

pattern PITMl_R5_Large :: ProcessingInstanceType
pattern PITMl_R5_Large = ProcessingInstanceType' "ml.r5.large"

pattern PITMl_R5_XLarge :: ProcessingInstanceType
pattern PITMl_R5_XLarge = ProcessingInstanceType' "ml.r5.xlarge"

pattern PITMl_T3_2XLarge :: ProcessingInstanceType
pattern PITMl_T3_2XLarge = ProcessingInstanceType' "ml.t3.2xlarge"

pattern PITMl_T3_Large :: ProcessingInstanceType
pattern PITMl_T3_Large = ProcessingInstanceType' "ml.t3.large"

pattern PITMl_T3_Medium :: ProcessingInstanceType
pattern PITMl_T3_Medium = ProcessingInstanceType' "ml.t3.medium"

pattern PITMl_T3_XLarge :: ProcessingInstanceType
pattern PITMl_T3_XLarge = ProcessingInstanceType' "ml.t3.xlarge"

{-# COMPLETE
  PITMl_C4_2XLarge,
  PITMl_C4_4XLarge,
  PITMl_C4_8XLarge,
  PITMl_C4_XLarge,
  PITMl_C5_18XLarge,
  PITMl_C5_2XLarge,
  PITMl_C5_4XLarge,
  PITMl_C5_9XLarge,
  PITMl_C5_XLarge,
  PITMl_M4_10XLarge,
  PITMl_M4_16XLarge,
  PITMl_M4_2XLarge,
  PITMl_M4_4XLarge,
  PITMl_M4_XLarge,
  PITMl_M5_12XLarge,
  PITMl_M5_24XLarge,
  PITMl_M5_2XLarge,
  PITMl_M5_4XLarge,
  PITMl_M5_Large,
  PITMl_M5_XLarge,
  PITMl_P2_16XLarge,
  PITMl_P2_8XLarge,
  PITMl_P2_XLarge,
  PITMl_P3_16XLarge,
  PITMl_P3_2XLarge,
  PITMl_P3_8XLarge,
  PITMl_R5_12XLarge,
  PITMl_R5_16XLarge,
  PITMl_R5_24XLarge,
  PITMl_R5_2XLarge,
  PITMl_R5_4XLarge,
  PITMl_R5_8XLarge,
  PITMl_R5_Large,
  PITMl_R5_XLarge,
  PITMl_T3_2XLarge,
  PITMl_T3_Large,
  PITMl_T3_Medium,
  PITMl_T3_XLarge,
  ProcessingInstanceType' #-}

instance FromText ProcessingInstanceType where
    parser = (ProcessingInstanceType' . mk) <$> takeText

instance ToText ProcessingInstanceType where
    toText (ProcessingInstanceType' ci) = original ci

-- | Represents an enum of /known/ $ProcessingInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingInstanceType where
    toEnum i = case i of
        0 -> PITMl_C4_2XLarge
        1 -> PITMl_C4_4XLarge
        2 -> PITMl_C4_8XLarge
        3 -> PITMl_C4_XLarge
        4 -> PITMl_C5_18XLarge
        5 -> PITMl_C5_2XLarge
        6 -> PITMl_C5_4XLarge
        7 -> PITMl_C5_9XLarge
        8 -> PITMl_C5_XLarge
        9 -> PITMl_M4_10XLarge
        10 -> PITMl_M4_16XLarge
        11 -> PITMl_M4_2XLarge
        12 -> PITMl_M4_4XLarge
        13 -> PITMl_M4_XLarge
        14 -> PITMl_M5_12XLarge
        15 -> PITMl_M5_24XLarge
        16 -> PITMl_M5_2XLarge
        17 -> PITMl_M5_4XLarge
        18 -> PITMl_M5_Large
        19 -> PITMl_M5_XLarge
        20 -> PITMl_P2_16XLarge
        21 -> PITMl_P2_8XLarge
        22 -> PITMl_P2_XLarge
        23 -> PITMl_P3_16XLarge
        24 -> PITMl_P3_2XLarge
        25 -> PITMl_P3_8XLarge
        26 -> PITMl_R5_12XLarge
        27 -> PITMl_R5_16XLarge
        28 -> PITMl_R5_24XLarge
        29 -> PITMl_R5_2XLarge
        30 -> PITMl_R5_4XLarge
        31 -> PITMl_R5_8XLarge
        32 -> PITMl_R5_Large
        33 -> PITMl_R5_XLarge
        34 -> PITMl_T3_2XLarge
        35 -> PITMl_T3_Large
        36 -> PITMl_T3_Medium
        37 -> PITMl_T3_XLarge
        _ -> (error . showText) $ "Unknown index for ProcessingInstanceType: " <> toText i
    fromEnum x = case x of
        PITMl_C4_2XLarge -> 0
        PITMl_C4_4XLarge -> 1
        PITMl_C4_8XLarge -> 2
        PITMl_C4_XLarge -> 3
        PITMl_C5_18XLarge -> 4
        PITMl_C5_2XLarge -> 5
        PITMl_C5_4XLarge -> 6
        PITMl_C5_9XLarge -> 7
        PITMl_C5_XLarge -> 8
        PITMl_M4_10XLarge -> 9
        PITMl_M4_16XLarge -> 10
        PITMl_M4_2XLarge -> 11
        PITMl_M4_4XLarge -> 12
        PITMl_M4_XLarge -> 13
        PITMl_M5_12XLarge -> 14
        PITMl_M5_24XLarge -> 15
        PITMl_M5_2XLarge -> 16
        PITMl_M5_4XLarge -> 17
        PITMl_M5_Large -> 18
        PITMl_M5_XLarge -> 19
        PITMl_P2_16XLarge -> 20
        PITMl_P2_8XLarge -> 21
        PITMl_P2_XLarge -> 22
        PITMl_P3_16XLarge -> 23
        PITMl_P3_2XLarge -> 24
        PITMl_P3_8XLarge -> 25
        PITMl_R5_12XLarge -> 26
        PITMl_R5_16XLarge -> 27
        PITMl_R5_24XLarge -> 28
        PITMl_R5_2XLarge -> 29
        PITMl_R5_4XLarge -> 30
        PITMl_R5_8XLarge -> 31
        PITMl_R5_Large -> 32
        PITMl_R5_XLarge -> 33
        PITMl_T3_2XLarge -> 34
        PITMl_T3_Large -> 35
        PITMl_T3_Medium -> 36
        PITMl_T3_XLarge -> 37
        ProcessingInstanceType' name -> (error . showText) $ "Unknown ProcessingInstanceType: " <> original name

-- | Represents the bounds of /known/ $ProcessingInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingInstanceType where
    minBound = PITMl_C4_2XLarge
    maxBound = PITMl_T3_XLarge

instance Hashable     ProcessingInstanceType
instance NFData       ProcessingInstanceType
instance ToByteString ProcessingInstanceType
instance ToQuery      ProcessingInstanceType
instance ToHeader     ProcessingInstanceType

instance ToJSON ProcessingInstanceType where
    toJSON = toJSONText

instance FromJSON ProcessingInstanceType where
    parseJSON = parseJSONText "ProcessingInstanceType"
