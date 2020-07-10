{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TargetDevice (
  TargetDevice (
    ..
    , Aisage
    , AmbaCV22
    , Deeplens
    , Imx8qm
    , JetsonNano
    , JetsonTX1
    , JetsonTX2
    , JetsonXavier
    , Lambda
    , MlC4
    , MlC5
    , MlINF1
    , MlM4
    , MlM5
    , MlP2
    , MlP3
    , QCS603
    , QCS605
    , RK3288
    , RK3399
    , Rasp3b
    , Sbec
    , SitaraAm57x
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetDevice = TargetDevice' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Aisage :: TargetDevice
pattern Aisage = TargetDevice' "aisage"

pattern AmbaCV22 :: TargetDevice
pattern AmbaCV22 = TargetDevice' "amba_cv22"

pattern Deeplens :: TargetDevice
pattern Deeplens = TargetDevice' "deeplens"

pattern Imx8qm :: TargetDevice
pattern Imx8qm = TargetDevice' "imx8qm"

pattern JetsonNano :: TargetDevice
pattern JetsonNano = TargetDevice' "jetson_nano"

pattern JetsonTX1 :: TargetDevice
pattern JetsonTX1 = TargetDevice' "jetson_tx1"

pattern JetsonTX2 :: TargetDevice
pattern JetsonTX2 = TargetDevice' "jetson_tx2"

pattern JetsonXavier :: TargetDevice
pattern JetsonXavier = TargetDevice' "jetson_xavier"

pattern Lambda :: TargetDevice
pattern Lambda = TargetDevice' "lambda"

pattern MlC4 :: TargetDevice
pattern MlC4 = TargetDevice' "ml_c4"

pattern MlC5 :: TargetDevice
pattern MlC5 = TargetDevice' "ml_c5"

pattern MlINF1 :: TargetDevice
pattern MlINF1 = TargetDevice' "ml_inf1"

pattern MlM4 :: TargetDevice
pattern MlM4 = TargetDevice' "ml_m4"

pattern MlM5 :: TargetDevice
pattern MlM5 = TargetDevice' "ml_m5"

pattern MlP2 :: TargetDevice
pattern MlP2 = TargetDevice' "ml_p2"

pattern MlP3 :: TargetDevice
pattern MlP3 = TargetDevice' "ml_p3"

pattern QCS603 :: TargetDevice
pattern QCS603 = TargetDevice' "qcs603"

pattern QCS605 :: TargetDevice
pattern QCS605 = TargetDevice' "qcs605"

pattern RK3288 :: TargetDevice
pattern RK3288 = TargetDevice' "rk3288"

pattern RK3399 :: TargetDevice
pattern RK3399 = TargetDevice' "rk3399"

pattern Rasp3b :: TargetDevice
pattern Rasp3b = TargetDevice' "rasp3b"

pattern Sbec :: TargetDevice
pattern Sbec = TargetDevice' "sbe_c"

pattern SitaraAm57x :: TargetDevice
pattern SitaraAm57x = TargetDevice' "sitara_am57x"

{-# COMPLETE
  Aisage,
  AmbaCV22,
  Deeplens,
  Imx8qm,
  JetsonNano,
  JetsonTX1,
  JetsonTX2,
  JetsonXavier,
  Lambda,
  MlC4,
  MlC5,
  MlINF1,
  MlM4,
  MlM5,
  MlP2,
  MlP3,
  QCS603,
  QCS605,
  RK3288,
  RK3399,
  Rasp3b,
  Sbec,
  SitaraAm57x,
  TargetDevice' #-}

instance FromText TargetDevice where
    parser = (TargetDevice' . mk) <$> takeText

instance ToText TargetDevice where
    toText (TargetDevice' ci) = original ci

-- | Represents an enum of /known/ $TargetDevice.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetDevice where
    toEnum i = case i of
        0 -> Aisage
        1 -> AmbaCV22
        2 -> Deeplens
        3 -> Imx8qm
        4 -> JetsonNano
        5 -> JetsonTX1
        6 -> JetsonTX2
        7 -> JetsonXavier
        8 -> Lambda
        9 -> MlC4
        10 -> MlC5
        11 -> MlINF1
        12 -> MlM4
        13 -> MlM5
        14 -> MlP2
        15 -> MlP3
        16 -> QCS603
        17 -> QCS605
        18 -> RK3288
        19 -> RK3399
        20 -> Rasp3b
        21 -> Sbec
        22 -> SitaraAm57x
        _ -> (error . showText) $ "Unknown index for TargetDevice: " <> toText i
    fromEnum x = case x of
        Aisage -> 0
        AmbaCV22 -> 1
        Deeplens -> 2
        Imx8qm -> 3
        JetsonNano -> 4
        JetsonTX1 -> 5
        JetsonTX2 -> 6
        JetsonXavier -> 7
        Lambda -> 8
        MlC4 -> 9
        MlC5 -> 10
        MlINF1 -> 11
        MlM4 -> 12
        MlM5 -> 13
        MlP2 -> 14
        MlP3 -> 15
        QCS603 -> 16
        QCS605 -> 17
        RK3288 -> 18
        RK3399 -> 19
        Rasp3b -> 20
        Sbec -> 21
        SitaraAm57x -> 22
        TargetDevice' name -> (error . showText) $ "Unknown TargetDevice: " <> original name

-- | Represents the bounds of /known/ $TargetDevice.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetDevice where
    minBound = Aisage
    maxBound = SitaraAm57x

instance Hashable     TargetDevice
instance NFData       TargetDevice
instance ToByteString TargetDevice
instance ToQuery      TargetDevice
instance ToHeader     TargetDevice

instance ToJSON TargetDevice where
    toJSON = toJSONText

instance FromJSON TargetDevice where
    parseJSON = parseJSONText "TargetDevice"
