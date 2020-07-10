{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppInstanceType (
  AppInstanceType (
    ..
    , Ml_C5_12XLarge
    , Ml_C5_18XLarge
    , Ml_C5_24XLarge
    , Ml_C5_2XLarge
    , Ml_C5_4XLarge
    , Ml_C5_9XLarge
    , Ml_C5_Large
    , Ml_C5_XLarge
    , Ml_G4dn_12XLarge
    , Ml_G4dn_16XLarge
    , Ml_G4dn_2XLarge
    , Ml_G4dn_4XLarge
    , Ml_G4dn_8XLarge
    , Ml_G4dn_XLarge
    , Ml_M5_12XLarge
    , Ml_M5_16XLarge
    , Ml_M5_24XLarge
    , Ml_M5_2XLarge
    , Ml_M5_4XLarge
    , Ml_M5_8XLarge
    , Ml_M5_Large
    , Ml_M5_XLarge
    , Ml_P3_16XLarge
    , Ml_P3_2XLarge
    , Ml_P3_8XLarge
    , Ml_T3_2XLarge
    , Ml_T3_Large
    , Ml_T3_Medium
    , Ml_T3_Micro
    , Ml_T3_Small
    , Ml_T3_XLarge
    , System
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AppInstanceType = AppInstanceType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Ml_C5_12XLarge :: AppInstanceType
pattern Ml_C5_12XLarge = AppInstanceType' "ml.c5.12xlarge"

pattern Ml_C5_18XLarge :: AppInstanceType
pattern Ml_C5_18XLarge = AppInstanceType' "ml.c5.18xlarge"

pattern Ml_C5_24XLarge :: AppInstanceType
pattern Ml_C5_24XLarge = AppInstanceType' "ml.c5.24xlarge"

pattern Ml_C5_2XLarge :: AppInstanceType
pattern Ml_C5_2XLarge = AppInstanceType' "ml.c5.2xlarge"

pattern Ml_C5_4XLarge :: AppInstanceType
pattern Ml_C5_4XLarge = AppInstanceType' "ml.c5.4xlarge"

pattern Ml_C5_9XLarge :: AppInstanceType
pattern Ml_C5_9XLarge = AppInstanceType' "ml.c5.9xlarge"

pattern Ml_C5_Large :: AppInstanceType
pattern Ml_C5_Large = AppInstanceType' "ml.c5.large"

pattern Ml_C5_XLarge :: AppInstanceType
pattern Ml_C5_XLarge = AppInstanceType' "ml.c5.xlarge"

pattern Ml_G4dn_12XLarge :: AppInstanceType
pattern Ml_G4dn_12XLarge = AppInstanceType' "ml.g4dn.12xlarge"

pattern Ml_G4dn_16XLarge :: AppInstanceType
pattern Ml_G4dn_16XLarge = AppInstanceType' "ml.g4dn.16xlarge"

pattern Ml_G4dn_2XLarge :: AppInstanceType
pattern Ml_G4dn_2XLarge = AppInstanceType' "ml.g4dn.2xlarge"

pattern Ml_G4dn_4XLarge :: AppInstanceType
pattern Ml_G4dn_4XLarge = AppInstanceType' "ml.g4dn.4xlarge"

pattern Ml_G4dn_8XLarge :: AppInstanceType
pattern Ml_G4dn_8XLarge = AppInstanceType' "ml.g4dn.8xlarge"

pattern Ml_G4dn_XLarge :: AppInstanceType
pattern Ml_G4dn_XLarge = AppInstanceType' "ml.g4dn.xlarge"

pattern Ml_M5_12XLarge :: AppInstanceType
pattern Ml_M5_12XLarge = AppInstanceType' "ml.m5.12xlarge"

pattern Ml_M5_16XLarge :: AppInstanceType
pattern Ml_M5_16XLarge = AppInstanceType' "ml.m5.16xlarge"

pattern Ml_M5_24XLarge :: AppInstanceType
pattern Ml_M5_24XLarge = AppInstanceType' "ml.m5.24xlarge"

pattern Ml_M5_2XLarge :: AppInstanceType
pattern Ml_M5_2XLarge = AppInstanceType' "ml.m5.2xlarge"

pattern Ml_M5_4XLarge :: AppInstanceType
pattern Ml_M5_4XLarge = AppInstanceType' "ml.m5.4xlarge"

pattern Ml_M5_8XLarge :: AppInstanceType
pattern Ml_M5_8XLarge = AppInstanceType' "ml.m5.8xlarge"

pattern Ml_M5_Large :: AppInstanceType
pattern Ml_M5_Large = AppInstanceType' "ml.m5.large"

pattern Ml_M5_XLarge :: AppInstanceType
pattern Ml_M5_XLarge = AppInstanceType' "ml.m5.xlarge"

pattern Ml_P3_16XLarge :: AppInstanceType
pattern Ml_P3_16XLarge = AppInstanceType' "ml.p3.16xlarge"

pattern Ml_P3_2XLarge :: AppInstanceType
pattern Ml_P3_2XLarge = AppInstanceType' "ml.p3.2xlarge"

pattern Ml_P3_8XLarge :: AppInstanceType
pattern Ml_P3_8XLarge = AppInstanceType' "ml.p3.8xlarge"

pattern Ml_T3_2XLarge :: AppInstanceType
pattern Ml_T3_2XLarge = AppInstanceType' "ml.t3.2xlarge"

pattern Ml_T3_Large :: AppInstanceType
pattern Ml_T3_Large = AppInstanceType' "ml.t3.large"

pattern Ml_T3_Medium :: AppInstanceType
pattern Ml_T3_Medium = AppInstanceType' "ml.t3.medium"

pattern Ml_T3_Micro :: AppInstanceType
pattern Ml_T3_Micro = AppInstanceType' "ml.t3.micro"

pattern Ml_T3_Small :: AppInstanceType
pattern Ml_T3_Small = AppInstanceType' "ml.t3.small"

pattern Ml_T3_XLarge :: AppInstanceType
pattern Ml_T3_XLarge = AppInstanceType' "ml.t3.xlarge"

pattern System :: AppInstanceType
pattern System = AppInstanceType' "system"

{-# COMPLETE
  Ml_C5_12XLarge,
  Ml_C5_18XLarge,
  Ml_C5_24XLarge,
  Ml_C5_2XLarge,
  Ml_C5_4XLarge,
  Ml_C5_9XLarge,
  Ml_C5_Large,
  Ml_C5_XLarge,
  Ml_G4dn_12XLarge,
  Ml_G4dn_16XLarge,
  Ml_G4dn_2XLarge,
  Ml_G4dn_4XLarge,
  Ml_G4dn_8XLarge,
  Ml_G4dn_XLarge,
  Ml_M5_12XLarge,
  Ml_M5_16XLarge,
  Ml_M5_24XLarge,
  Ml_M5_2XLarge,
  Ml_M5_4XLarge,
  Ml_M5_8XLarge,
  Ml_M5_Large,
  Ml_M5_XLarge,
  Ml_P3_16XLarge,
  Ml_P3_2XLarge,
  Ml_P3_8XLarge,
  Ml_T3_2XLarge,
  Ml_T3_Large,
  Ml_T3_Medium,
  Ml_T3_Micro,
  Ml_T3_Small,
  Ml_T3_XLarge,
  System,
  AppInstanceType' #-}

instance FromText AppInstanceType where
    parser = (AppInstanceType' . mk) <$> takeText

instance ToText AppInstanceType where
    toText (AppInstanceType' ci) = original ci

-- | Represents an enum of /known/ $AppInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AppInstanceType where
    toEnum i = case i of
        0 -> Ml_C5_12XLarge
        1 -> Ml_C5_18XLarge
        2 -> Ml_C5_24XLarge
        3 -> Ml_C5_2XLarge
        4 -> Ml_C5_4XLarge
        5 -> Ml_C5_9XLarge
        6 -> Ml_C5_Large
        7 -> Ml_C5_XLarge
        8 -> Ml_G4dn_12XLarge
        9 -> Ml_G4dn_16XLarge
        10 -> Ml_G4dn_2XLarge
        11 -> Ml_G4dn_4XLarge
        12 -> Ml_G4dn_8XLarge
        13 -> Ml_G4dn_XLarge
        14 -> Ml_M5_12XLarge
        15 -> Ml_M5_16XLarge
        16 -> Ml_M5_24XLarge
        17 -> Ml_M5_2XLarge
        18 -> Ml_M5_4XLarge
        19 -> Ml_M5_8XLarge
        20 -> Ml_M5_Large
        21 -> Ml_M5_XLarge
        22 -> Ml_P3_16XLarge
        23 -> Ml_P3_2XLarge
        24 -> Ml_P3_8XLarge
        25 -> Ml_T3_2XLarge
        26 -> Ml_T3_Large
        27 -> Ml_T3_Medium
        28 -> Ml_T3_Micro
        29 -> Ml_T3_Small
        30 -> Ml_T3_XLarge
        31 -> System
        _ -> (error . showText) $ "Unknown index for AppInstanceType: " <> toText i
    fromEnum x = case x of
        Ml_C5_12XLarge -> 0
        Ml_C5_18XLarge -> 1
        Ml_C5_24XLarge -> 2
        Ml_C5_2XLarge -> 3
        Ml_C5_4XLarge -> 4
        Ml_C5_9XLarge -> 5
        Ml_C5_Large -> 6
        Ml_C5_XLarge -> 7
        Ml_G4dn_12XLarge -> 8
        Ml_G4dn_16XLarge -> 9
        Ml_G4dn_2XLarge -> 10
        Ml_G4dn_4XLarge -> 11
        Ml_G4dn_8XLarge -> 12
        Ml_G4dn_XLarge -> 13
        Ml_M5_12XLarge -> 14
        Ml_M5_16XLarge -> 15
        Ml_M5_24XLarge -> 16
        Ml_M5_2XLarge -> 17
        Ml_M5_4XLarge -> 18
        Ml_M5_8XLarge -> 19
        Ml_M5_Large -> 20
        Ml_M5_XLarge -> 21
        Ml_P3_16XLarge -> 22
        Ml_P3_2XLarge -> 23
        Ml_P3_8XLarge -> 24
        Ml_T3_2XLarge -> 25
        Ml_T3_Large -> 26
        Ml_T3_Medium -> 27
        Ml_T3_Micro -> 28
        Ml_T3_Small -> 29
        Ml_T3_XLarge -> 30
        System -> 31
        AppInstanceType' name -> (error . showText) $ "Unknown AppInstanceType: " <> original name

-- | Represents the bounds of /known/ $AppInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AppInstanceType where
    minBound = Ml_C5_12XLarge
    maxBound = System

instance Hashable     AppInstanceType
instance NFData       AppInstanceType
instance ToByteString AppInstanceType
instance ToQuery      AppInstanceType
instance ToHeader     AppInstanceType

instance ToJSON AppInstanceType where
    toJSON = toJSONText

instance FromJSON AppInstanceType where
    parseJSON = parseJSONText "AppInstanceType"
