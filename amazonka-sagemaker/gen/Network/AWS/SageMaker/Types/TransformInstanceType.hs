{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TransformInstanceType (
  TransformInstanceType (
    ..
    , TMl_C4_2XLarge
    , TMl_C4_4XLarge
    , TMl_C4_8XLarge
    , TMl_C4_XLarge
    , TMl_C5_18XLarge
    , TMl_C5_2XLarge
    , TMl_C5_4XLarge
    , TMl_C5_9XLarge
    , TMl_C5_XLarge
    , TMl_M4_10XLarge
    , TMl_M4_16XLarge
    , TMl_M4_2XLarge
    , TMl_M4_4XLarge
    , TMl_M4_XLarge
    , TMl_M5_12XLarge
    , TMl_M5_24XLarge
    , TMl_M5_2XLarge
    , TMl_M5_4XLarge
    , TMl_M5_Large
    , TMl_M5_XLarge
    , TMl_P2_16XLarge
    , TMl_P2_8XLarge
    , TMl_P2_XLarge
    , TMl_P3_16XLarge
    , TMl_P3_2XLarge
    , TMl_P3_8XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransformInstanceType = TransformInstanceType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern TMl_C4_2XLarge :: TransformInstanceType
pattern TMl_C4_2XLarge = TransformInstanceType' "ml.c4.2xlarge"

pattern TMl_C4_4XLarge :: TransformInstanceType
pattern TMl_C4_4XLarge = TransformInstanceType' "ml.c4.4xlarge"

pattern TMl_C4_8XLarge :: TransformInstanceType
pattern TMl_C4_8XLarge = TransformInstanceType' "ml.c4.8xlarge"

pattern TMl_C4_XLarge :: TransformInstanceType
pattern TMl_C4_XLarge = TransformInstanceType' "ml.c4.xlarge"

pattern TMl_C5_18XLarge :: TransformInstanceType
pattern TMl_C5_18XLarge = TransformInstanceType' "ml.c5.18xlarge"

pattern TMl_C5_2XLarge :: TransformInstanceType
pattern TMl_C5_2XLarge = TransformInstanceType' "ml.c5.2xlarge"

pattern TMl_C5_4XLarge :: TransformInstanceType
pattern TMl_C5_4XLarge = TransformInstanceType' "ml.c5.4xlarge"

pattern TMl_C5_9XLarge :: TransformInstanceType
pattern TMl_C5_9XLarge = TransformInstanceType' "ml.c5.9xlarge"

pattern TMl_C5_XLarge :: TransformInstanceType
pattern TMl_C5_XLarge = TransformInstanceType' "ml.c5.xlarge"

pattern TMl_M4_10XLarge :: TransformInstanceType
pattern TMl_M4_10XLarge = TransformInstanceType' "ml.m4.10xlarge"

pattern TMl_M4_16XLarge :: TransformInstanceType
pattern TMl_M4_16XLarge = TransformInstanceType' "ml.m4.16xlarge"

pattern TMl_M4_2XLarge :: TransformInstanceType
pattern TMl_M4_2XLarge = TransformInstanceType' "ml.m4.2xlarge"

pattern TMl_M4_4XLarge :: TransformInstanceType
pattern TMl_M4_4XLarge = TransformInstanceType' "ml.m4.4xlarge"

pattern TMl_M4_XLarge :: TransformInstanceType
pattern TMl_M4_XLarge = TransformInstanceType' "ml.m4.xlarge"

pattern TMl_M5_12XLarge :: TransformInstanceType
pattern TMl_M5_12XLarge = TransformInstanceType' "ml.m5.12xlarge"

pattern TMl_M5_24XLarge :: TransformInstanceType
pattern TMl_M5_24XLarge = TransformInstanceType' "ml.m5.24xlarge"

pattern TMl_M5_2XLarge :: TransformInstanceType
pattern TMl_M5_2XLarge = TransformInstanceType' "ml.m5.2xlarge"

pattern TMl_M5_4XLarge :: TransformInstanceType
pattern TMl_M5_4XLarge = TransformInstanceType' "ml.m5.4xlarge"

pattern TMl_M5_Large :: TransformInstanceType
pattern TMl_M5_Large = TransformInstanceType' "ml.m5.large"

pattern TMl_M5_XLarge :: TransformInstanceType
pattern TMl_M5_XLarge = TransformInstanceType' "ml.m5.xlarge"

pattern TMl_P2_16XLarge :: TransformInstanceType
pattern TMl_P2_16XLarge = TransformInstanceType' "ml.p2.16xlarge"

pattern TMl_P2_8XLarge :: TransformInstanceType
pattern TMl_P2_8XLarge = TransformInstanceType' "ml.p2.8xlarge"

pattern TMl_P2_XLarge :: TransformInstanceType
pattern TMl_P2_XLarge = TransformInstanceType' "ml.p2.xlarge"

pattern TMl_P3_16XLarge :: TransformInstanceType
pattern TMl_P3_16XLarge = TransformInstanceType' "ml.p3.16xlarge"

pattern TMl_P3_2XLarge :: TransformInstanceType
pattern TMl_P3_2XLarge = TransformInstanceType' "ml.p3.2xlarge"

pattern TMl_P3_8XLarge :: TransformInstanceType
pattern TMl_P3_8XLarge = TransformInstanceType' "ml.p3.8xlarge"

{-# COMPLETE
  TMl_C4_2XLarge,
  TMl_C4_4XLarge,
  TMl_C4_8XLarge,
  TMl_C4_XLarge,
  TMl_C5_18XLarge,
  TMl_C5_2XLarge,
  TMl_C5_4XLarge,
  TMl_C5_9XLarge,
  TMl_C5_XLarge,
  TMl_M4_10XLarge,
  TMl_M4_16XLarge,
  TMl_M4_2XLarge,
  TMl_M4_4XLarge,
  TMl_M4_XLarge,
  TMl_M5_12XLarge,
  TMl_M5_24XLarge,
  TMl_M5_2XLarge,
  TMl_M5_4XLarge,
  TMl_M5_Large,
  TMl_M5_XLarge,
  TMl_P2_16XLarge,
  TMl_P2_8XLarge,
  TMl_P2_XLarge,
  TMl_P3_16XLarge,
  TMl_P3_2XLarge,
  TMl_P3_8XLarge,
  TransformInstanceType' #-}

instance FromText TransformInstanceType where
    parser = (TransformInstanceType' . mk) <$> takeText

instance ToText TransformInstanceType where
    toText (TransformInstanceType' ci) = original ci

-- | Represents an enum of /known/ $TransformInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransformInstanceType where
    toEnum i = case i of
        0 -> TMl_C4_2XLarge
        1 -> TMl_C4_4XLarge
        2 -> TMl_C4_8XLarge
        3 -> TMl_C4_XLarge
        4 -> TMl_C5_18XLarge
        5 -> TMl_C5_2XLarge
        6 -> TMl_C5_4XLarge
        7 -> TMl_C5_9XLarge
        8 -> TMl_C5_XLarge
        9 -> TMl_M4_10XLarge
        10 -> TMl_M4_16XLarge
        11 -> TMl_M4_2XLarge
        12 -> TMl_M4_4XLarge
        13 -> TMl_M4_XLarge
        14 -> TMl_M5_12XLarge
        15 -> TMl_M5_24XLarge
        16 -> TMl_M5_2XLarge
        17 -> TMl_M5_4XLarge
        18 -> TMl_M5_Large
        19 -> TMl_M5_XLarge
        20 -> TMl_P2_16XLarge
        21 -> TMl_P2_8XLarge
        22 -> TMl_P2_XLarge
        23 -> TMl_P3_16XLarge
        24 -> TMl_P3_2XLarge
        25 -> TMl_P3_8XLarge
        _ -> (error . showText) $ "Unknown index for TransformInstanceType: " <> toText i
    fromEnum x = case x of
        TMl_C4_2XLarge -> 0
        TMl_C4_4XLarge -> 1
        TMl_C4_8XLarge -> 2
        TMl_C4_XLarge -> 3
        TMl_C5_18XLarge -> 4
        TMl_C5_2XLarge -> 5
        TMl_C5_4XLarge -> 6
        TMl_C5_9XLarge -> 7
        TMl_C5_XLarge -> 8
        TMl_M4_10XLarge -> 9
        TMl_M4_16XLarge -> 10
        TMl_M4_2XLarge -> 11
        TMl_M4_4XLarge -> 12
        TMl_M4_XLarge -> 13
        TMl_M5_12XLarge -> 14
        TMl_M5_24XLarge -> 15
        TMl_M5_2XLarge -> 16
        TMl_M5_4XLarge -> 17
        TMl_M5_Large -> 18
        TMl_M5_XLarge -> 19
        TMl_P2_16XLarge -> 20
        TMl_P2_8XLarge -> 21
        TMl_P2_XLarge -> 22
        TMl_P3_16XLarge -> 23
        TMl_P3_2XLarge -> 24
        TMl_P3_8XLarge -> 25
        TransformInstanceType' name -> (error . showText) $ "Unknown TransformInstanceType: " <> original name

-- | Represents the bounds of /known/ $TransformInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransformInstanceType where
    minBound = TMl_C4_2XLarge
    maxBound = TMl_P3_8XLarge

instance Hashable     TransformInstanceType
instance NFData       TransformInstanceType
instance ToByteString TransformInstanceType
instance ToQuery      TransformInstanceType
instance ToHeader     TransformInstanceType

instance ToJSON TransformInstanceType where
    toJSON = toJSONText

instance FromJSON TransformInstanceType where
    parseJSON = parseJSONText "TransformInstanceType"
