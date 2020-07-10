{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.EC2InstanceType (
  EC2InstanceType (
    ..
    , EITC3_2XLarge
    , EITC3_4XLarge
    , EITC3_8XLarge
    , EITC3_Large
    , EITC3_XLarge
    , EITC4_2XLarge
    , EITC4_4XLarge
    , EITC4_8XLarge
    , EITC4_Large
    , EITC4_XLarge
    , EITC5_12XLarge
    , EITC5_18XLarge
    , EITC5_24XLarge
    , EITC5_2XLarge
    , EITC5_4XLarge
    , EITC5_9XLarge
    , EITC5_Large
    , EITC5_XLarge
    , EITM3_2XLarge
    , EITM3_Large
    , EITM3_Medium
    , EITM3_XLarge
    , EITM4_10XLarge
    , EITM4_2XLarge
    , EITM4_4XLarge
    , EITM4_Large
    , EITM4_XLarge
    , EITM5_12XLarge
    , EITM5_16XLarge
    , EITM5_24XLarge
    , EITM5_2XLarge
    , EITM5_4XLarge
    , EITM5_8XLarge
    , EITM5_Large
    , EITM5_XLarge
    , EITR3_2XLarge
    , EITR3_4XLarge
    , EITR3_8XLarge
    , EITR3_Large
    , EITR3_XLarge
    , EITR4_16XLarge
    , EITR4_2XLarge
    , EITR4_4XLarge
    , EITR4_8XLarge
    , EITR4_Large
    , EITR4_XLarge
    , EITR5_12XLarge
    , EITR5_16XLarge
    , EITR5_24XLarge
    , EITR5_2XLarge
    , EITR5_4XLarge
    , EITR5_8XLarge
    , EITR5_Large
    , EITR5_XLarge
    , EITT2_Large
    , EITT2_Medium
    , EITT2_Micro
    , EITT2_Small
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EC2InstanceType = EC2InstanceType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern EITC3_2XLarge :: EC2InstanceType
pattern EITC3_2XLarge = EC2InstanceType' "c3.2xlarge"

pattern EITC3_4XLarge :: EC2InstanceType
pattern EITC3_4XLarge = EC2InstanceType' "c3.4xlarge"

pattern EITC3_8XLarge :: EC2InstanceType
pattern EITC3_8XLarge = EC2InstanceType' "c3.8xlarge"

pattern EITC3_Large :: EC2InstanceType
pattern EITC3_Large = EC2InstanceType' "c3.large"

pattern EITC3_XLarge :: EC2InstanceType
pattern EITC3_XLarge = EC2InstanceType' "c3.xlarge"

pattern EITC4_2XLarge :: EC2InstanceType
pattern EITC4_2XLarge = EC2InstanceType' "c4.2xlarge"

pattern EITC4_4XLarge :: EC2InstanceType
pattern EITC4_4XLarge = EC2InstanceType' "c4.4xlarge"

pattern EITC4_8XLarge :: EC2InstanceType
pattern EITC4_8XLarge = EC2InstanceType' "c4.8xlarge"

pattern EITC4_Large :: EC2InstanceType
pattern EITC4_Large = EC2InstanceType' "c4.large"

pattern EITC4_XLarge :: EC2InstanceType
pattern EITC4_XLarge = EC2InstanceType' "c4.xlarge"

pattern EITC5_12XLarge :: EC2InstanceType
pattern EITC5_12XLarge = EC2InstanceType' "c5.12xlarge"

pattern EITC5_18XLarge :: EC2InstanceType
pattern EITC5_18XLarge = EC2InstanceType' "c5.18xlarge"

pattern EITC5_24XLarge :: EC2InstanceType
pattern EITC5_24XLarge = EC2InstanceType' "c5.24xlarge"

pattern EITC5_2XLarge :: EC2InstanceType
pattern EITC5_2XLarge = EC2InstanceType' "c5.2xlarge"

pattern EITC5_4XLarge :: EC2InstanceType
pattern EITC5_4XLarge = EC2InstanceType' "c5.4xlarge"

pattern EITC5_9XLarge :: EC2InstanceType
pattern EITC5_9XLarge = EC2InstanceType' "c5.9xlarge"

pattern EITC5_Large :: EC2InstanceType
pattern EITC5_Large = EC2InstanceType' "c5.large"

pattern EITC5_XLarge :: EC2InstanceType
pattern EITC5_XLarge = EC2InstanceType' "c5.xlarge"

pattern EITM3_2XLarge :: EC2InstanceType
pattern EITM3_2XLarge = EC2InstanceType' "m3.2xlarge"

pattern EITM3_Large :: EC2InstanceType
pattern EITM3_Large = EC2InstanceType' "m3.large"

pattern EITM3_Medium :: EC2InstanceType
pattern EITM3_Medium = EC2InstanceType' "m3.medium"

pattern EITM3_XLarge :: EC2InstanceType
pattern EITM3_XLarge = EC2InstanceType' "m3.xlarge"

pattern EITM4_10XLarge :: EC2InstanceType
pattern EITM4_10XLarge = EC2InstanceType' "m4.10xlarge"

pattern EITM4_2XLarge :: EC2InstanceType
pattern EITM4_2XLarge = EC2InstanceType' "m4.2xlarge"

pattern EITM4_4XLarge :: EC2InstanceType
pattern EITM4_4XLarge = EC2InstanceType' "m4.4xlarge"

pattern EITM4_Large :: EC2InstanceType
pattern EITM4_Large = EC2InstanceType' "m4.large"

pattern EITM4_XLarge :: EC2InstanceType
pattern EITM4_XLarge = EC2InstanceType' "m4.xlarge"

pattern EITM5_12XLarge :: EC2InstanceType
pattern EITM5_12XLarge = EC2InstanceType' "m5.12xlarge"

pattern EITM5_16XLarge :: EC2InstanceType
pattern EITM5_16XLarge = EC2InstanceType' "m5.16xlarge"

pattern EITM5_24XLarge :: EC2InstanceType
pattern EITM5_24XLarge = EC2InstanceType' "m5.24xlarge"

pattern EITM5_2XLarge :: EC2InstanceType
pattern EITM5_2XLarge = EC2InstanceType' "m5.2xlarge"

pattern EITM5_4XLarge :: EC2InstanceType
pattern EITM5_4XLarge = EC2InstanceType' "m5.4xlarge"

pattern EITM5_8XLarge :: EC2InstanceType
pattern EITM5_8XLarge = EC2InstanceType' "m5.8xlarge"

pattern EITM5_Large :: EC2InstanceType
pattern EITM5_Large = EC2InstanceType' "m5.large"

pattern EITM5_XLarge :: EC2InstanceType
pattern EITM5_XLarge = EC2InstanceType' "m5.xlarge"

pattern EITR3_2XLarge :: EC2InstanceType
pattern EITR3_2XLarge = EC2InstanceType' "r3.2xlarge"

pattern EITR3_4XLarge :: EC2InstanceType
pattern EITR3_4XLarge = EC2InstanceType' "r3.4xlarge"

pattern EITR3_8XLarge :: EC2InstanceType
pattern EITR3_8XLarge = EC2InstanceType' "r3.8xlarge"

pattern EITR3_Large :: EC2InstanceType
pattern EITR3_Large = EC2InstanceType' "r3.large"

pattern EITR3_XLarge :: EC2InstanceType
pattern EITR3_XLarge = EC2InstanceType' "r3.xlarge"

pattern EITR4_16XLarge :: EC2InstanceType
pattern EITR4_16XLarge = EC2InstanceType' "r4.16xlarge"

pattern EITR4_2XLarge :: EC2InstanceType
pattern EITR4_2XLarge = EC2InstanceType' "r4.2xlarge"

pattern EITR4_4XLarge :: EC2InstanceType
pattern EITR4_4XLarge = EC2InstanceType' "r4.4xlarge"

pattern EITR4_8XLarge :: EC2InstanceType
pattern EITR4_8XLarge = EC2InstanceType' "r4.8xlarge"

pattern EITR4_Large :: EC2InstanceType
pattern EITR4_Large = EC2InstanceType' "r4.large"

pattern EITR4_XLarge :: EC2InstanceType
pattern EITR4_XLarge = EC2InstanceType' "r4.xlarge"

pattern EITR5_12XLarge :: EC2InstanceType
pattern EITR5_12XLarge = EC2InstanceType' "r5.12xlarge"

pattern EITR5_16XLarge :: EC2InstanceType
pattern EITR5_16XLarge = EC2InstanceType' "r5.16xlarge"

pattern EITR5_24XLarge :: EC2InstanceType
pattern EITR5_24XLarge = EC2InstanceType' "r5.24xlarge"

pattern EITR5_2XLarge :: EC2InstanceType
pattern EITR5_2XLarge = EC2InstanceType' "r5.2xlarge"

pattern EITR5_4XLarge :: EC2InstanceType
pattern EITR5_4XLarge = EC2InstanceType' "r5.4xlarge"

pattern EITR5_8XLarge :: EC2InstanceType
pattern EITR5_8XLarge = EC2InstanceType' "r5.8xlarge"

pattern EITR5_Large :: EC2InstanceType
pattern EITR5_Large = EC2InstanceType' "r5.large"

pattern EITR5_XLarge :: EC2InstanceType
pattern EITR5_XLarge = EC2InstanceType' "r5.xlarge"

pattern EITT2_Large :: EC2InstanceType
pattern EITT2_Large = EC2InstanceType' "t2.large"

pattern EITT2_Medium :: EC2InstanceType
pattern EITT2_Medium = EC2InstanceType' "t2.medium"

pattern EITT2_Micro :: EC2InstanceType
pattern EITT2_Micro = EC2InstanceType' "t2.micro"

pattern EITT2_Small :: EC2InstanceType
pattern EITT2_Small = EC2InstanceType' "t2.small"

{-# COMPLETE
  EITC3_2XLarge,
  EITC3_4XLarge,
  EITC3_8XLarge,
  EITC3_Large,
  EITC3_XLarge,
  EITC4_2XLarge,
  EITC4_4XLarge,
  EITC4_8XLarge,
  EITC4_Large,
  EITC4_XLarge,
  EITC5_12XLarge,
  EITC5_18XLarge,
  EITC5_24XLarge,
  EITC5_2XLarge,
  EITC5_4XLarge,
  EITC5_9XLarge,
  EITC5_Large,
  EITC5_XLarge,
  EITM3_2XLarge,
  EITM3_Large,
  EITM3_Medium,
  EITM3_XLarge,
  EITM4_10XLarge,
  EITM4_2XLarge,
  EITM4_4XLarge,
  EITM4_Large,
  EITM4_XLarge,
  EITM5_12XLarge,
  EITM5_16XLarge,
  EITM5_24XLarge,
  EITM5_2XLarge,
  EITM5_4XLarge,
  EITM5_8XLarge,
  EITM5_Large,
  EITM5_XLarge,
  EITR3_2XLarge,
  EITR3_4XLarge,
  EITR3_8XLarge,
  EITR3_Large,
  EITR3_XLarge,
  EITR4_16XLarge,
  EITR4_2XLarge,
  EITR4_4XLarge,
  EITR4_8XLarge,
  EITR4_Large,
  EITR4_XLarge,
  EITR5_12XLarge,
  EITR5_16XLarge,
  EITR5_24XLarge,
  EITR5_2XLarge,
  EITR5_4XLarge,
  EITR5_8XLarge,
  EITR5_Large,
  EITR5_XLarge,
  EITT2_Large,
  EITT2_Medium,
  EITT2_Micro,
  EITT2_Small,
  EC2InstanceType' #-}

instance FromText EC2InstanceType where
    parser = (EC2InstanceType' . mk) <$> takeText

instance ToText EC2InstanceType where
    toText (EC2InstanceType' ci) = original ci

-- | Represents an enum of /known/ $EC2InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EC2InstanceType where
    toEnum i = case i of
        0 -> EITC3_2XLarge
        1 -> EITC3_4XLarge
        2 -> EITC3_8XLarge
        3 -> EITC3_Large
        4 -> EITC3_XLarge
        5 -> EITC4_2XLarge
        6 -> EITC4_4XLarge
        7 -> EITC4_8XLarge
        8 -> EITC4_Large
        9 -> EITC4_XLarge
        10 -> EITC5_12XLarge
        11 -> EITC5_18XLarge
        12 -> EITC5_24XLarge
        13 -> EITC5_2XLarge
        14 -> EITC5_4XLarge
        15 -> EITC5_9XLarge
        16 -> EITC5_Large
        17 -> EITC5_XLarge
        18 -> EITM3_2XLarge
        19 -> EITM3_Large
        20 -> EITM3_Medium
        21 -> EITM3_XLarge
        22 -> EITM4_10XLarge
        23 -> EITM4_2XLarge
        24 -> EITM4_4XLarge
        25 -> EITM4_Large
        26 -> EITM4_XLarge
        27 -> EITM5_12XLarge
        28 -> EITM5_16XLarge
        29 -> EITM5_24XLarge
        30 -> EITM5_2XLarge
        31 -> EITM5_4XLarge
        32 -> EITM5_8XLarge
        33 -> EITM5_Large
        34 -> EITM5_XLarge
        35 -> EITR3_2XLarge
        36 -> EITR3_4XLarge
        37 -> EITR3_8XLarge
        38 -> EITR3_Large
        39 -> EITR3_XLarge
        40 -> EITR4_16XLarge
        41 -> EITR4_2XLarge
        42 -> EITR4_4XLarge
        43 -> EITR4_8XLarge
        44 -> EITR4_Large
        45 -> EITR4_XLarge
        46 -> EITR5_12XLarge
        47 -> EITR5_16XLarge
        48 -> EITR5_24XLarge
        49 -> EITR5_2XLarge
        50 -> EITR5_4XLarge
        51 -> EITR5_8XLarge
        52 -> EITR5_Large
        53 -> EITR5_XLarge
        54 -> EITT2_Large
        55 -> EITT2_Medium
        56 -> EITT2_Micro
        57 -> EITT2_Small
        _ -> (error . showText) $ "Unknown index for EC2InstanceType: " <> toText i
    fromEnum x = case x of
        EITC3_2XLarge -> 0
        EITC3_4XLarge -> 1
        EITC3_8XLarge -> 2
        EITC3_Large -> 3
        EITC3_XLarge -> 4
        EITC4_2XLarge -> 5
        EITC4_4XLarge -> 6
        EITC4_8XLarge -> 7
        EITC4_Large -> 8
        EITC4_XLarge -> 9
        EITC5_12XLarge -> 10
        EITC5_18XLarge -> 11
        EITC5_24XLarge -> 12
        EITC5_2XLarge -> 13
        EITC5_4XLarge -> 14
        EITC5_9XLarge -> 15
        EITC5_Large -> 16
        EITC5_XLarge -> 17
        EITM3_2XLarge -> 18
        EITM3_Large -> 19
        EITM3_Medium -> 20
        EITM3_XLarge -> 21
        EITM4_10XLarge -> 22
        EITM4_2XLarge -> 23
        EITM4_4XLarge -> 24
        EITM4_Large -> 25
        EITM4_XLarge -> 26
        EITM5_12XLarge -> 27
        EITM5_16XLarge -> 28
        EITM5_24XLarge -> 29
        EITM5_2XLarge -> 30
        EITM5_4XLarge -> 31
        EITM5_8XLarge -> 32
        EITM5_Large -> 33
        EITM5_XLarge -> 34
        EITR3_2XLarge -> 35
        EITR3_4XLarge -> 36
        EITR3_8XLarge -> 37
        EITR3_Large -> 38
        EITR3_XLarge -> 39
        EITR4_16XLarge -> 40
        EITR4_2XLarge -> 41
        EITR4_4XLarge -> 42
        EITR4_8XLarge -> 43
        EITR4_Large -> 44
        EITR4_XLarge -> 45
        EITR5_12XLarge -> 46
        EITR5_16XLarge -> 47
        EITR5_24XLarge -> 48
        EITR5_2XLarge -> 49
        EITR5_4XLarge -> 50
        EITR5_8XLarge -> 51
        EITR5_Large -> 52
        EITR5_XLarge -> 53
        EITT2_Large -> 54
        EITT2_Medium -> 55
        EITT2_Micro -> 56
        EITT2_Small -> 57
        EC2InstanceType' name -> (error . showText) $ "Unknown EC2InstanceType: " <> original name

-- | Represents the bounds of /known/ $EC2InstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EC2InstanceType where
    minBound = EITC3_2XLarge
    maxBound = EITT2_Small

instance Hashable     EC2InstanceType
instance NFData       EC2InstanceType
instance ToByteString EC2InstanceType
instance ToQuery      EC2InstanceType
instance ToHeader     EC2InstanceType

instance ToJSON EC2InstanceType where
    toJSON = toJSONText

instance FromJSON EC2InstanceType where
    parseJSON = parseJSONText "EC2InstanceType"
