{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.AnalysisSchemeLanguage (
  AnalysisSchemeLanguage (
    ..
    , AR
    , BG
    , CA
    , CS
    , DA
    , DE
    , EL
    , EN
    , ES
    , EU
    , FA
    , FI
    , FR
    , GA
    , GL
    , HE
    , HI
    , HU
    , HY
    , IT
    , Id
    , JA
    , KO
    , LV
    , Mul
    , NL
    , NO
    , PT
    , RO
    , RU
    , SV
    , TH
    , TR
    , ZhHans
    , ZhHant
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | An <http://tools.ietf.org/html/rfc4646 IETF RFC 4646> language code or @mul@ for multiple languages.
--
--
data AnalysisSchemeLanguage = AnalysisSchemeLanguage' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AR :: AnalysisSchemeLanguage
pattern AR = AnalysisSchemeLanguage' "ar"

pattern BG :: AnalysisSchemeLanguage
pattern BG = AnalysisSchemeLanguage' "bg"

pattern CA :: AnalysisSchemeLanguage
pattern CA = AnalysisSchemeLanguage' "ca"

pattern CS :: AnalysisSchemeLanguage
pattern CS = AnalysisSchemeLanguage' "cs"

pattern DA :: AnalysisSchemeLanguage
pattern DA = AnalysisSchemeLanguage' "da"

pattern DE :: AnalysisSchemeLanguage
pattern DE = AnalysisSchemeLanguage' "de"

pattern EL :: AnalysisSchemeLanguage
pattern EL = AnalysisSchemeLanguage' "el"

pattern EN :: AnalysisSchemeLanguage
pattern EN = AnalysisSchemeLanguage' "en"

pattern ES :: AnalysisSchemeLanguage
pattern ES = AnalysisSchemeLanguage' "es"

pattern EU :: AnalysisSchemeLanguage
pattern EU = AnalysisSchemeLanguage' "eu"

pattern FA :: AnalysisSchemeLanguage
pattern FA = AnalysisSchemeLanguage' "fa"

pattern FI :: AnalysisSchemeLanguage
pattern FI = AnalysisSchemeLanguage' "fi"

pattern FR :: AnalysisSchemeLanguage
pattern FR = AnalysisSchemeLanguage' "fr"

pattern GA :: AnalysisSchemeLanguage
pattern GA = AnalysisSchemeLanguage' "ga"

pattern GL :: AnalysisSchemeLanguage
pattern GL = AnalysisSchemeLanguage' "gl"

pattern HE :: AnalysisSchemeLanguage
pattern HE = AnalysisSchemeLanguage' "he"

pattern HI :: AnalysisSchemeLanguage
pattern HI = AnalysisSchemeLanguage' "hi"

pattern HU :: AnalysisSchemeLanguage
pattern HU = AnalysisSchemeLanguage' "hu"

pattern HY :: AnalysisSchemeLanguage
pattern HY = AnalysisSchemeLanguage' "hy"

pattern IT :: AnalysisSchemeLanguage
pattern IT = AnalysisSchemeLanguage' "it"

pattern Id :: AnalysisSchemeLanguage
pattern Id = AnalysisSchemeLanguage' "id"

pattern JA :: AnalysisSchemeLanguage
pattern JA = AnalysisSchemeLanguage' "ja"

pattern KO :: AnalysisSchemeLanguage
pattern KO = AnalysisSchemeLanguage' "ko"

pattern LV :: AnalysisSchemeLanguage
pattern LV = AnalysisSchemeLanguage' "lv"

pattern Mul :: AnalysisSchemeLanguage
pattern Mul = AnalysisSchemeLanguage' "mul"

pattern NL :: AnalysisSchemeLanguage
pattern NL = AnalysisSchemeLanguage' "nl"

pattern NO :: AnalysisSchemeLanguage
pattern NO = AnalysisSchemeLanguage' "no"

pattern PT :: AnalysisSchemeLanguage
pattern PT = AnalysisSchemeLanguage' "pt"

pattern RO :: AnalysisSchemeLanguage
pattern RO = AnalysisSchemeLanguage' "ro"

pattern RU :: AnalysisSchemeLanguage
pattern RU = AnalysisSchemeLanguage' "ru"

pattern SV :: AnalysisSchemeLanguage
pattern SV = AnalysisSchemeLanguage' "sv"

pattern TH :: AnalysisSchemeLanguage
pattern TH = AnalysisSchemeLanguage' "th"

pattern TR :: AnalysisSchemeLanguage
pattern TR = AnalysisSchemeLanguage' "tr"

pattern ZhHans :: AnalysisSchemeLanguage
pattern ZhHans = AnalysisSchemeLanguage' "zh-Hans"

pattern ZhHant :: AnalysisSchemeLanguage
pattern ZhHant = AnalysisSchemeLanguage' "zh-Hant"

{-# COMPLETE
  AR,
  BG,
  CA,
  CS,
  DA,
  DE,
  EL,
  EN,
  ES,
  EU,
  FA,
  FI,
  FR,
  GA,
  GL,
  HE,
  HI,
  HU,
  HY,
  IT,
  Id,
  JA,
  KO,
  LV,
  Mul,
  NL,
  NO,
  PT,
  RO,
  RU,
  SV,
  TH,
  TR,
  ZhHans,
  ZhHant,
  AnalysisSchemeLanguage' #-}

instance FromText AnalysisSchemeLanguage where
    parser = (AnalysisSchemeLanguage' . mk) <$> takeText

instance ToText AnalysisSchemeLanguage where
    toText (AnalysisSchemeLanguage' ci) = original ci

-- | Represents an enum of /known/ $AnalysisSchemeLanguage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AnalysisSchemeLanguage where
    toEnum i = case i of
        0 -> AR
        1 -> BG
        2 -> CA
        3 -> CS
        4 -> DA
        5 -> DE
        6 -> EL
        7 -> EN
        8 -> ES
        9 -> EU
        10 -> FA
        11 -> FI
        12 -> FR
        13 -> GA
        14 -> GL
        15 -> HE
        16 -> HI
        17 -> HU
        18 -> HY
        19 -> IT
        20 -> Id
        21 -> JA
        22 -> KO
        23 -> LV
        24 -> Mul
        25 -> NL
        26 -> NO
        27 -> PT
        28 -> RO
        29 -> RU
        30 -> SV
        31 -> TH
        32 -> TR
        33 -> ZhHans
        34 -> ZhHant
        _ -> (error . showText) $ "Unknown index for AnalysisSchemeLanguage: " <> toText i
    fromEnum x = case x of
        AR -> 0
        BG -> 1
        CA -> 2
        CS -> 3
        DA -> 4
        DE -> 5
        EL -> 6
        EN -> 7
        ES -> 8
        EU -> 9
        FA -> 10
        FI -> 11
        FR -> 12
        GA -> 13
        GL -> 14
        HE -> 15
        HI -> 16
        HU -> 17
        HY -> 18
        IT -> 19
        Id -> 20
        JA -> 21
        KO -> 22
        LV -> 23
        Mul -> 24
        NL -> 25
        NO -> 26
        PT -> 27
        RO -> 28
        RU -> 29
        SV -> 30
        TH -> 31
        TR -> 32
        ZhHans -> 33
        ZhHant -> 34
        AnalysisSchemeLanguage' name -> (error . showText) $ "Unknown AnalysisSchemeLanguage: " <> original name

-- | Represents the bounds of /known/ $AnalysisSchemeLanguage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AnalysisSchemeLanguage where
    minBound = AR
    maxBound = ZhHant

instance Hashable     AnalysisSchemeLanguage
instance NFData       AnalysisSchemeLanguage
instance ToByteString AnalysisSchemeLanguage
instance ToQuery      AnalysisSchemeLanguage
instance ToHeader     AnalysisSchemeLanguage

instance FromXML AnalysisSchemeLanguage where
    parseXML = parseXMLText "AnalysisSchemeLanguage"
