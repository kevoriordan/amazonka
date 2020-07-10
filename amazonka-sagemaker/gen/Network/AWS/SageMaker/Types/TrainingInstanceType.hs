{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrainingInstanceType (
  TrainingInstanceType (
    ..
    , TITMl_C4_2XLarge
    , TITMl_C4_4XLarge
    , TITMl_C4_8XLarge
    , TITMl_C4_XLarge
    , TITMl_C5_18XLarge
    , TITMl_C5_2XLarge
    , TITMl_C5_4XLarge
    , TITMl_C5_9XLarge
    , TITMl_C5_XLarge
    , TITMl_C5n_18XLarge
    , TITMl_C5n_2XLarge
    , TITMl_C5n_4XLarge
    , TITMl_C5n_9XLarge
    , TITMl_C5n_XLarge
    , TITMl_G4dn_12XLarge
    , TITMl_G4dn_16XLarge
    , TITMl_G4dn_2XLarge
    , TITMl_G4dn_4XLarge
    , TITMl_G4dn_8XLarge
    , TITMl_G4dn_XLarge
    , TITMl_M4_10XLarge
    , TITMl_M4_16XLarge
    , TITMl_M4_2XLarge
    , TITMl_M4_4XLarge
    , TITMl_M4_XLarge
    , TITMl_M5_12XLarge
    , TITMl_M5_24XLarge
    , TITMl_M5_2XLarge
    , TITMl_M5_4XLarge
    , TITMl_M5_Large
    , TITMl_M5_XLarge
    , TITMl_P2_16XLarge
    , TITMl_P2_8XLarge
    , TITMl_P2_XLarge
    , TITMl_P3_16XLarge
    , TITMl_P3_2XLarge
    , TITMl_P3_8XLarge
    , TITMl_P3dn_24XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrainingInstanceType = TrainingInstanceType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern TITMl_C4_2XLarge :: TrainingInstanceType
pattern TITMl_C4_2XLarge = TrainingInstanceType' "ml.c4.2xlarge"

pattern TITMl_C4_4XLarge :: TrainingInstanceType
pattern TITMl_C4_4XLarge = TrainingInstanceType' "ml.c4.4xlarge"

pattern TITMl_C4_8XLarge :: TrainingInstanceType
pattern TITMl_C4_8XLarge = TrainingInstanceType' "ml.c4.8xlarge"

pattern TITMl_C4_XLarge :: TrainingInstanceType
pattern TITMl_C4_XLarge = TrainingInstanceType' "ml.c4.xlarge"

pattern TITMl_C5_18XLarge :: TrainingInstanceType
pattern TITMl_C5_18XLarge = TrainingInstanceType' "ml.c5.18xlarge"

pattern TITMl_C5_2XLarge :: TrainingInstanceType
pattern TITMl_C5_2XLarge = TrainingInstanceType' "ml.c5.2xlarge"

pattern TITMl_C5_4XLarge :: TrainingInstanceType
pattern TITMl_C5_4XLarge = TrainingInstanceType' "ml.c5.4xlarge"

pattern TITMl_C5_9XLarge :: TrainingInstanceType
pattern TITMl_C5_9XLarge = TrainingInstanceType' "ml.c5.9xlarge"

pattern TITMl_C5_XLarge :: TrainingInstanceType
pattern TITMl_C5_XLarge = TrainingInstanceType' "ml.c5.xlarge"

pattern TITMl_C5n_18XLarge :: TrainingInstanceType
pattern TITMl_C5n_18XLarge = TrainingInstanceType' "ml.c5n.18xlarge"

pattern TITMl_C5n_2XLarge :: TrainingInstanceType
pattern TITMl_C5n_2XLarge = TrainingInstanceType' "ml.c5n.2xlarge"

pattern TITMl_C5n_4XLarge :: TrainingInstanceType
pattern TITMl_C5n_4XLarge = TrainingInstanceType' "ml.c5n.4xlarge"

pattern TITMl_C5n_9XLarge :: TrainingInstanceType
pattern TITMl_C5n_9XLarge = TrainingInstanceType' "ml.c5n.9xlarge"

pattern TITMl_C5n_XLarge :: TrainingInstanceType
pattern TITMl_C5n_XLarge = TrainingInstanceType' "ml.c5n.xlarge"

pattern TITMl_G4dn_12XLarge :: TrainingInstanceType
pattern TITMl_G4dn_12XLarge = TrainingInstanceType' "ml.g4dn.12xlarge"

pattern TITMl_G4dn_16XLarge :: TrainingInstanceType
pattern TITMl_G4dn_16XLarge = TrainingInstanceType' "ml.g4dn.16xlarge"

pattern TITMl_G4dn_2XLarge :: TrainingInstanceType
pattern TITMl_G4dn_2XLarge = TrainingInstanceType' "ml.g4dn.2xlarge"

pattern TITMl_G4dn_4XLarge :: TrainingInstanceType
pattern TITMl_G4dn_4XLarge = TrainingInstanceType' "ml.g4dn.4xlarge"

pattern TITMl_G4dn_8XLarge :: TrainingInstanceType
pattern TITMl_G4dn_8XLarge = TrainingInstanceType' "ml.g4dn.8xlarge"

pattern TITMl_G4dn_XLarge :: TrainingInstanceType
pattern TITMl_G4dn_XLarge = TrainingInstanceType' "ml.g4dn.xlarge"

pattern TITMl_M4_10XLarge :: TrainingInstanceType
pattern TITMl_M4_10XLarge = TrainingInstanceType' "ml.m4.10xlarge"

pattern TITMl_M4_16XLarge :: TrainingInstanceType
pattern TITMl_M4_16XLarge = TrainingInstanceType' "ml.m4.16xlarge"

pattern TITMl_M4_2XLarge :: TrainingInstanceType
pattern TITMl_M4_2XLarge = TrainingInstanceType' "ml.m4.2xlarge"

pattern TITMl_M4_4XLarge :: TrainingInstanceType
pattern TITMl_M4_4XLarge = TrainingInstanceType' "ml.m4.4xlarge"

pattern TITMl_M4_XLarge :: TrainingInstanceType
pattern TITMl_M4_XLarge = TrainingInstanceType' "ml.m4.xlarge"

pattern TITMl_M5_12XLarge :: TrainingInstanceType
pattern TITMl_M5_12XLarge = TrainingInstanceType' "ml.m5.12xlarge"

pattern TITMl_M5_24XLarge :: TrainingInstanceType
pattern TITMl_M5_24XLarge = TrainingInstanceType' "ml.m5.24xlarge"

pattern TITMl_M5_2XLarge :: TrainingInstanceType
pattern TITMl_M5_2XLarge = TrainingInstanceType' "ml.m5.2xlarge"

pattern TITMl_M5_4XLarge :: TrainingInstanceType
pattern TITMl_M5_4XLarge = TrainingInstanceType' "ml.m5.4xlarge"

pattern TITMl_M5_Large :: TrainingInstanceType
pattern TITMl_M5_Large = TrainingInstanceType' "ml.m5.large"

pattern TITMl_M5_XLarge :: TrainingInstanceType
pattern TITMl_M5_XLarge = TrainingInstanceType' "ml.m5.xlarge"

pattern TITMl_P2_16XLarge :: TrainingInstanceType
pattern TITMl_P2_16XLarge = TrainingInstanceType' "ml.p2.16xlarge"

pattern TITMl_P2_8XLarge :: TrainingInstanceType
pattern TITMl_P2_8XLarge = TrainingInstanceType' "ml.p2.8xlarge"

pattern TITMl_P2_XLarge :: TrainingInstanceType
pattern TITMl_P2_XLarge = TrainingInstanceType' "ml.p2.xlarge"

pattern TITMl_P3_16XLarge :: TrainingInstanceType
pattern TITMl_P3_16XLarge = TrainingInstanceType' "ml.p3.16xlarge"

pattern TITMl_P3_2XLarge :: TrainingInstanceType
pattern TITMl_P3_2XLarge = TrainingInstanceType' "ml.p3.2xlarge"

pattern TITMl_P3_8XLarge :: TrainingInstanceType
pattern TITMl_P3_8XLarge = TrainingInstanceType' "ml.p3.8xlarge"

pattern TITMl_P3dn_24XLarge :: TrainingInstanceType
pattern TITMl_P3dn_24XLarge = TrainingInstanceType' "ml.p3dn.24xlarge"

{-# COMPLETE
  TITMl_C4_2XLarge,
  TITMl_C4_4XLarge,
  TITMl_C4_8XLarge,
  TITMl_C4_XLarge,
  TITMl_C5_18XLarge,
  TITMl_C5_2XLarge,
  TITMl_C5_4XLarge,
  TITMl_C5_9XLarge,
  TITMl_C5_XLarge,
  TITMl_C5n_18XLarge,
  TITMl_C5n_2XLarge,
  TITMl_C5n_4XLarge,
  TITMl_C5n_9XLarge,
  TITMl_C5n_XLarge,
  TITMl_G4dn_12XLarge,
  TITMl_G4dn_16XLarge,
  TITMl_G4dn_2XLarge,
  TITMl_G4dn_4XLarge,
  TITMl_G4dn_8XLarge,
  TITMl_G4dn_XLarge,
  TITMl_M4_10XLarge,
  TITMl_M4_16XLarge,
  TITMl_M4_2XLarge,
  TITMl_M4_4XLarge,
  TITMl_M4_XLarge,
  TITMl_M5_12XLarge,
  TITMl_M5_24XLarge,
  TITMl_M5_2XLarge,
  TITMl_M5_4XLarge,
  TITMl_M5_Large,
  TITMl_M5_XLarge,
  TITMl_P2_16XLarge,
  TITMl_P2_8XLarge,
  TITMl_P2_XLarge,
  TITMl_P3_16XLarge,
  TITMl_P3_2XLarge,
  TITMl_P3_8XLarge,
  TITMl_P3dn_24XLarge,
  TrainingInstanceType' #-}

instance FromText TrainingInstanceType where
    parser = (TrainingInstanceType' . mk) <$> takeText

instance ToText TrainingInstanceType where
    toText (TrainingInstanceType' ci) = original ci

-- | Represents an enum of /known/ $TrainingInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrainingInstanceType where
    toEnum i = case i of
        0 -> TITMl_C4_2XLarge
        1 -> TITMl_C4_4XLarge
        2 -> TITMl_C4_8XLarge
        3 -> TITMl_C4_XLarge
        4 -> TITMl_C5_18XLarge
        5 -> TITMl_C5_2XLarge
        6 -> TITMl_C5_4XLarge
        7 -> TITMl_C5_9XLarge
        8 -> TITMl_C5_XLarge
        9 -> TITMl_C5n_18XLarge
        10 -> TITMl_C5n_2XLarge
        11 -> TITMl_C5n_4XLarge
        12 -> TITMl_C5n_9XLarge
        13 -> TITMl_C5n_XLarge
        14 -> TITMl_G4dn_12XLarge
        15 -> TITMl_G4dn_16XLarge
        16 -> TITMl_G4dn_2XLarge
        17 -> TITMl_G4dn_4XLarge
        18 -> TITMl_G4dn_8XLarge
        19 -> TITMl_G4dn_XLarge
        20 -> TITMl_M4_10XLarge
        21 -> TITMl_M4_16XLarge
        22 -> TITMl_M4_2XLarge
        23 -> TITMl_M4_4XLarge
        24 -> TITMl_M4_XLarge
        25 -> TITMl_M5_12XLarge
        26 -> TITMl_M5_24XLarge
        27 -> TITMl_M5_2XLarge
        28 -> TITMl_M5_4XLarge
        29 -> TITMl_M5_Large
        30 -> TITMl_M5_XLarge
        31 -> TITMl_P2_16XLarge
        32 -> TITMl_P2_8XLarge
        33 -> TITMl_P2_XLarge
        34 -> TITMl_P3_16XLarge
        35 -> TITMl_P3_2XLarge
        36 -> TITMl_P3_8XLarge
        37 -> TITMl_P3dn_24XLarge
        _ -> (error . showText) $ "Unknown index for TrainingInstanceType: " <> toText i
    fromEnum x = case x of
        TITMl_C4_2XLarge -> 0
        TITMl_C4_4XLarge -> 1
        TITMl_C4_8XLarge -> 2
        TITMl_C4_XLarge -> 3
        TITMl_C5_18XLarge -> 4
        TITMl_C5_2XLarge -> 5
        TITMl_C5_4XLarge -> 6
        TITMl_C5_9XLarge -> 7
        TITMl_C5_XLarge -> 8
        TITMl_C5n_18XLarge -> 9
        TITMl_C5n_2XLarge -> 10
        TITMl_C5n_4XLarge -> 11
        TITMl_C5n_9XLarge -> 12
        TITMl_C5n_XLarge -> 13
        TITMl_G4dn_12XLarge -> 14
        TITMl_G4dn_16XLarge -> 15
        TITMl_G4dn_2XLarge -> 16
        TITMl_G4dn_4XLarge -> 17
        TITMl_G4dn_8XLarge -> 18
        TITMl_G4dn_XLarge -> 19
        TITMl_M4_10XLarge -> 20
        TITMl_M4_16XLarge -> 21
        TITMl_M4_2XLarge -> 22
        TITMl_M4_4XLarge -> 23
        TITMl_M4_XLarge -> 24
        TITMl_M5_12XLarge -> 25
        TITMl_M5_24XLarge -> 26
        TITMl_M5_2XLarge -> 27
        TITMl_M5_4XLarge -> 28
        TITMl_M5_Large -> 29
        TITMl_M5_XLarge -> 30
        TITMl_P2_16XLarge -> 31
        TITMl_P2_8XLarge -> 32
        TITMl_P2_XLarge -> 33
        TITMl_P3_16XLarge -> 34
        TITMl_P3_2XLarge -> 35
        TITMl_P3_8XLarge -> 36
        TITMl_P3dn_24XLarge -> 37
        TrainingInstanceType' name -> (error . showText) $ "Unknown TrainingInstanceType: " <> original name

-- | Represents the bounds of /known/ $TrainingInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrainingInstanceType where
    minBound = TITMl_C4_2XLarge
    maxBound = TITMl_P3dn_24XLarge

instance Hashable     TrainingInstanceType
instance NFData       TrainingInstanceType
instance ToByteString TrainingInstanceType
instance ToQuery      TrainingInstanceType
instance ToHeader     TrainingInstanceType

instance ToJSON TrainingInstanceType where
    toJSON = toJSONText

instance FromJSON TrainingInstanceType where
    parseJSON = parseJSONText "TrainingInstanceType"
