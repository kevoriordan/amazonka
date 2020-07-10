{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.GameServerGroupInstanceType (
  GameServerGroupInstanceType (
    ..
    , C4_2XLarge
    , C4_4XLarge
    , C4_8XLarge
    , C4_Large
    , C4_XLarge
    , C5_12XLarge
    , C5_18XLarge
    , C5_24XLarge
    , C5_2XLarge
    , C5_4XLarge
    , C5_9XLarge
    , C5_Large
    , C5_XLarge
    , M4_10XLarge
    , M4_2XLarge
    , M4_4XLarge
    , M4_Large
    , M4_XLarge
    , M5_12XLarge
    , M5_16XLarge
    , M5_24XLarge
    , M5_2XLarge
    , M5_4XLarge
    , M5_8XLarge
    , M5_Large
    , M5_XLarge
    , R4_16XLarge
    , R4_2XLarge
    , R4_4XLarge
    , R4_8XLarge
    , R4_Large
    , R4_XLarge
    , R5_12XLarge
    , R5_16XLarge
    , R5_24XLarge
    , R5_2XLarge
    , R5_4XLarge
    , R5_8XLarge
    , R5_Large
    , R5_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GameServerGroupInstanceType = GameServerGroupInstanceType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern C4_2XLarge :: GameServerGroupInstanceType
pattern C4_2XLarge = GameServerGroupInstanceType' "c4.2xlarge"

pattern C4_4XLarge :: GameServerGroupInstanceType
pattern C4_4XLarge = GameServerGroupInstanceType' "c4.4xlarge"

pattern C4_8XLarge :: GameServerGroupInstanceType
pattern C4_8XLarge = GameServerGroupInstanceType' "c4.8xlarge"

pattern C4_Large :: GameServerGroupInstanceType
pattern C4_Large = GameServerGroupInstanceType' "c4.large"

pattern C4_XLarge :: GameServerGroupInstanceType
pattern C4_XLarge = GameServerGroupInstanceType' "c4.xlarge"

pattern C5_12XLarge :: GameServerGroupInstanceType
pattern C5_12XLarge = GameServerGroupInstanceType' "c5.12xlarge"

pattern C5_18XLarge :: GameServerGroupInstanceType
pattern C5_18XLarge = GameServerGroupInstanceType' "c5.18xlarge"

pattern C5_24XLarge :: GameServerGroupInstanceType
pattern C5_24XLarge = GameServerGroupInstanceType' "c5.24xlarge"

pattern C5_2XLarge :: GameServerGroupInstanceType
pattern C5_2XLarge = GameServerGroupInstanceType' "c5.2xlarge"

pattern C5_4XLarge :: GameServerGroupInstanceType
pattern C5_4XLarge = GameServerGroupInstanceType' "c5.4xlarge"

pattern C5_9XLarge :: GameServerGroupInstanceType
pattern C5_9XLarge = GameServerGroupInstanceType' "c5.9xlarge"

pattern C5_Large :: GameServerGroupInstanceType
pattern C5_Large = GameServerGroupInstanceType' "c5.large"

pattern C5_XLarge :: GameServerGroupInstanceType
pattern C5_XLarge = GameServerGroupInstanceType' "c5.xlarge"

pattern M4_10XLarge :: GameServerGroupInstanceType
pattern M4_10XLarge = GameServerGroupInstanceType' "m4.10xlarge"

pattern M4_2XLarge :: GameServerGroupInstanceType
pattern M4_2XLarge = GameServerGroupInstanceType' "m4.2xlarge"

pattern M4_4XLarge :: GameServerGroupInstanceType
pattern M4_4XLarge = GameServerGroupInstanceType' "m4.4xlarge"

pattern M4_Large :: GameServerGroupInstanceType
pattern M4_Large = GameServerGroupInstanceType' "m4.large"

pattern M4_XLarge :: GameServerGroupInstanceType
pattern M4_XLarge = GameServerGroupInstanceType' "m4.xlarge"

pattern M5_12XLarge :: GameServerGroupInstanceType
pattern M5_12XLarge = GameServerGroupInstanceType' "m5.12xlarge"

pattern M5_16XLarge :: GameServerGroupInstanceType
pattern M5_16XLarge = GameServerGroupInstanceType' "m5.16xlarge"

pattern M5_24XLarge :: GameServerGroupInstanceType
pattern M5_24XLarge = GameServerGroupInstanceType' "m5.24xlarge"

pattern M5_2XLarge :: GameServerGroupInstanceType
pattern M5_2XLarge = GameServerGroupInstanceType' "m5.2xlarge"

pattern M5_4XLarge :: GameServerGroupInstanceType
pattern M5_4XLarge = GameServerGroupInstanceType' "m5.4xlarge"

pattern M5_8XLarge :: GameServerGroupInstanceType
pattern M5_8XLarge = GameServerGroupInstanceType' "m5.8xlarge"

pattern M5_Large :: GameServerGroupInstanceType
pattern M5_Large = GameServerGroupInstanceType' "m5.large"

pattern M5_XLarge :: GameServerGroupInstanceType
pattern M5_XLarge = GameServerGroupInstanceType' "m5.xlarge"

pattern R4_16XLarge :: GameServerGroupInstanceType
pattern R4_16XLarge = GameServerGroupInstanceType' "r4.16xlarge"

pattern R4_2XLarge :: GameServerGroupInstanceType
pattern R4_2XLarge = GameServerGroupInstanceType' "r4.2xlarge"

pattern R4_4XLarge :: GameServerGroupInstanceType
pattern R4_4XLarge = GameServerGroupInstanceType' "r4.4xlarge"

pattern R4_8XLarge :: GameServerGroupInstanceType
pattern R4_8XLarge = GameServerGroupInstanceType' "r4.8xlarge"

pattern R4_Large :: GameServerGroupInstanceType
pattern R4_Large = GameServerGroupInstanceType' "r4.large"

pattern R4_XLarge :: GameServerGroupInstanceType
pattern R4_XLarge = GameServerGroupInstanceType' "r4.xlarge"

pattern R5_12XLarge :: GameServerGroupInstanceType
pattern R5_12XLarge = GameServerGroupInstanceType' "r5.12xlarge"

pattern R5_16XLarge :: GameServerGroupInstanceType
pattern R5_16XLarge = GameServerGroupInstanceType' "r5.16xlarge"

pattern R5_24XLarge :: GameServerGroupInstanceType
pattern R5_24XLarge = GameServerGroupInstanceType' "r5.24xlarge"

pattern R5_2XLarge :: GameServerGroupInstanceType
pattern R5_2XLarge = GameServerGroupInstanceType' "r5.2xlarge"

pattern R5_4XLarge :: GameServerGroupInstanceType
pattern R5_4XLarge = GameServerGroupInstanceType' "r5.4xlarge"

pattern R5_8XLarge :: GameServerGroupInstanceType
pattern R5_8XLarge = GameServerGroupInstanceType' "r5.8xlarge"

pattern R5_Large :: GameServerGroupInstanceType
pattern R5_Large = GameServerGroupInstanceType' "r5.large"

pattern R5_XLarge :: GameServerGroupInstanceType
pattern R5_XLarge = GameServerGroupInstanceType' "r5.xlarge"

{-# COMPLETE
  C4_2XLarge,
  C4_4XLarge,
  C4_8XLarge,
  C4_Large,
  C4_XLarge,
  C5_12XLarge,
  C5_18XLarge,
  C5_24XLarge,
  C5_2XLarge,
  C5_4XLarge,
  C5_9XLarge,
  C5_Large,
  C5_XLarge,
  M4_10XLarge,
  M4_2XLarge,
  M4_4XLarge,
  M4_Large,
  M4_XLarge,
  M5_12XLarge,
  M5_16XLarge,
  M5_24XLarge,
  M5_2XLarge,
  M5_4XLarge,
  M5_8XLarge,
  M5_Large,
  M5_XLarge,
  R4_16XLarge,
  R4_2XLarge,
  R4_4XLarge,
  R4_8XLarge,
  R4_Large,
  R4_XLarge,
  R5_12XLarge,
  R5_16XLarge,
  R5_24XLarge,
  R5_2XLarge,
  R5_4XLarge,
  R5_8XLarge,
  R5_Large,
  R5_XLarge,
  GameServerGroupInstanceType' #-}

instance FromText GameServerGroupInstanceType where
    parser = (GameServerGroupInstanceType' . mk) <$> takeText

instance ToText GameServerGroupInstanceType where
    toText (GameServerGroupInstanceType' ci) = original ci

-- | Represents an enum of /known/ $GameServerGroupInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GameServerGroupInstanceType where
    toEnum i = case i of
        0 -> C4_2XLarge
        1 -> C4_4XLarge
        2 -> C4_8XLarge
        3 -> C4_Large
        4 -> C4_XLarge
        5 -> C5_12XLarge
        6 -> C5_18XLarge
        7 -> C5_24XLarge
        8 -> C5_2XLarge
        9 -> C5_4XLarge
        10 -> C5_9XLarge
        11 -> C5_Large
        12 -> C5_XLarge
        13 -> M4_10XLarge
        14 -> M4_2XLarge
        15 -> M4_4XLarge
        16 -> M4_Large
        17 -> M4_XLarge
        18 -> M5_12XLarge
        19 -> M5_16XLarge
        20 -> M5_24XLarge
        21 -> M5_2XLarge
        22 -> M5_4XLarge
        23 -> M5_8XLarge
        24 -> M5_Large
        25 -> M5_XLarge
        26 -> R4_16XLarge
        27 -> R4_2XLarge
        28 -> R4_4XLarge
        29 -> R4_8XLarge
        30 -> R4_Large
        31 -> R4_XLarge
        32 -> R5_12XLarge
        33 -> R5_16XLarge
        34 -> R5_24XLarge
        35 -> R5_2XLarge
        36 -> R5_4XLarge
        37 -> R5_8XLarge
        38 -> R5_Large
        39 -> R5_XLarge
        _ -> (error . showText) $ "Unknown index for GameServerGroupInstanceType: " <> toText i
    fromEnum x = case x of
        C4_2XLarge -> 0
        C4_4XLarge -> 1
        C4_8XLarge -> 2
        C4_Large -> 3
        C4_XLarge -> 4
        C5_12XLarge -> 5
        C5_18XLarge -> 6
        C5_24XLarge -> 7
        C5_2XLarge -> 8
        C5_4XLarge -> 9
        C5_9XLarge -> 10
        C5_Large -> 11
        C5_XLarge -> 12
        M4_10XLarge -> 13
        M4_2XLarge -> 14
        M4_4XLarge -> 15
        M4_Large -> 16
        M4_XLarge -> 17
        M5_12XLarge -> 18
        M5_16XLarge -> 19
        M5_24XLarge -> 20
        M5_2XLarge -> 21
        M5_4XLarge -> 22
        M5_8XLarge -> 23
        M5_Large -> 24
        M5_XLarge -> 25
        R4_16XLarge -> 26
        R4_2XLarge -> 27
        R4_4XLarge -> 28
        R4_8XLarge -> 29
        R4_Large -> 30
        R4_XLarge -> 31
        R5_12XLarge -> 32
        R5_16XLarge -> 33
        R5_24XLarge -> 34
        R5_2XLarge -> 35
        R5_4XLarge -> 36
        R5_8XLarge -> 37
        R5_Large -> 38
        R5_XLarge -> 39
        GameServerGroupInstanceType' name -> (error . showText) $ "Unknown GameServerGroupInstanceType: " <> original name

-- | Represents the bounds of /known/ $GameServerGroupInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GameServerGroupInstanceType where
    minBound = C4_2XLarge
    maxBound = R5_XLarge

instance Hashable     GameServerGroupInstanceType
instance NFData       GameServerGroupInstanceType
instance ToByteString GameServerGroupInstanceType
instance ToQuery      GameServerGroupInstanceType
instance ToHeader     GameServerGroupInstanceType

instance ToJSON GameServerGroupInstanceType where
    toJSON = toJSONText

instance FromJSON GameServerGroupInstanceType where
    parseJSON = parseJSONText "GameServerGroupInstanceType"
