{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LanguageCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.LanguageCode (
  LanguageCode (
    ..
    , Arb
    , CmnCn
    , CyGb
    , DaDk
    , DeDe
    , EnAu
    , EnGb
    , EnGbWls
    , EnIn
    , EnUs
    , EsEs
    , EsMx
    , EsUs
    , FrCa
    , FrFr
    , HiIn
    , IsIs
    , ItIt
    , JaJp
    , KoKr
    , NbNo
    , NlNl
    , PlPl
    , PtBr
    , PtPt
    , RoRo
    , RuRu
    , SvSe
    , TrTr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LanguageCode = LanguageCode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Arb :: LanguageCode
pattern Arb = LanguageCode' "arb"

pattern CmnCn :: LanguageCode
pattern CmnCn = LanguageCode' "cmn-CN"

pattern CyGb :: LanguageCode
pattern CyGb = LanguageCode' "cy-GB"

pattern DaDk :: LanguageCode
pattern DaDk = LanguageCode' "da-DK"

pattern DeDe :: LanguageCode
pattern DeDe = LanguageCode' "de-DE"

pattern EnAu :: LanguageCode
pattern EnAu = LanguageCode' "en-AU"

pattern EnGb :: LanguageCode
pattern EnGb = LanguageCode' "en-GB"

pattern EnGbWls :: LanguageCode
pattern EnGbWls = LanguageCode' "en-GB-WLS"

pattern EnIn :: LanguageCode
pattern EnIn = LanguageCode' "en-IN"

pattern EnUs :: LanguageCode
pattern EnUs = LanguageCode' "en-US"

pattern EsEs :: LanguageCode
pattern EsEs = LanguageCode' "es-ES"

pattern EsMx :: LanguageCode
pattern EsMx = LanguageCode' "es-MX"

pattern EsUs :: LanguageCode
pattern EsUs = LanguageCode' "es-US"

pattern FrCa :: LanguageCode
pattern FrCa = LanguageCode' "fr-CA"

pattern FrFr :: LanguageCode
pattern FrFr = LanguageCode' "fr-FR"

pattern HiIn :: LanguageCode
pattern HiIn = LanguageCode' "hi-IN"

pattern IsIs :: LanguageCode
pattern IsIs = LanguageCode' "is-IS"

pattern ItIt :: LanguageCode
pattern ItIt = LanguageCode' "it-IT"

pattern JaJp :: LanguageCode
pattern JaJp = LanguageCode' "ja-JP"

pattern KoKr :: LanguageCode
pattern KoKr = LanguageCode' "ko-KR"

pattern NbNo :: LanguageCode
pattern NbNo = LanguageCode' "nb-NO"

pattern NlNl :: LanguageCode
pattern NlNl = LanguageCode' "nl-NL"

pattern PlPl :: LanguageCode
pattern PlPl = LanguageCode' "pl-PL"

pattern PtBr :: LanguageCode
pattern PtBr = LanguageCode' "pt-BR"

pattern PtPt :: LanguageCode
pattern PtPt = LanguageCode' "pt-PT"

pattern RoRo :: LanguageCode
pattern RoRo = LanguageCode' "ro-RO"

pattern RuRu :: LanguageCode
pattern RuRu = LanguageCode' "ru-RU"

pattern SvSe :: LanguageCode
pattern SvSe = LanguageCode' "sv-SE"

pattern TrTr :: LanguageCode
pattern TrTr = LanguageCode' "tr-TR"

{-# COMPLETE
  Arb,
  CmnCn,
  CyGb,
  DaDk,
  DeDe,
  EnAu,
  EnGb,
  EnGbWls,
  EnIn,
  EnUs,
  EsEs,
  EsMx,
  EsUs,
  FrCa,
  FrFr,
  HiIn,
  IsIs,
  ItIt,
  JaJp,
  KoKr,
  NbNo,
  NlNl,
  PlPl,
  PtBr,
  PtPt,
  RoRo,
  RuRu,
  SvSe,
  TrTr,
  LanguageCode' #-}

instance FromText LanguageCode where
    parser = (LanguageCode' . mk) <$> takeText

instance ToText LanguageCode where
    toText (LanguageCode' ci) = original ci

-- | Represents an enum of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LanguageCode where
    toEnum i = case i of
        0 -> Arb
        1 -> CmnCn
        2 -> CyGb
        3 -> DaDk
        4 -> DeDe
        5 -> EnAu
        6 -> EnGb
        7 -> EnGbWls
        8 -> EnIn
        9 -> EnUs
        10 -> EsEs
        11 -> EsMx
        12 -> EsUs
        13 -> FrCa
        14 -> FrFr
        15 -> HiIn
        16 -> IsIs
        17 -> ItIt
        18 -> JaJp
        19 -> KoKr
        20 -> NbNo
        21 -> NlNl
        22 -> PlPl
        23 -> PtBr
        24 -> PtPt
        25 -> RoRo
        26 -> RuRu
        27 -> SvSe
        28 -> TrTr
        _ -> (error . showText) $ "Unknown index for LanguageCode: " <> toText i
    fromEnum x = case x of
        Arb -> 0
        CmnCn -> 1
        CyGb -> 2
        DaDk -> 3
        DeDe -> 4
        EnAu -> 5
        EnGb -> 6
        EnGbWls -> 7
        EnIn -> 8
        EnUs -> 9
        EsEs -> 10
        EsMx -> 11
        EsUs -> 12
        FrCa -> 13
        FrFr -> 14
        HiIn -> 15
        IsIs -> 16
        ItIt -> 17
        JaJp -> 18
        KoKr -> 19
        NbNo -> 20
        NlNl -> 21
        PlPl -> 22
        PtBr -> 23
        PtPt -> 24
        RoRo -> 25
        RuRu -> 26
        SvSe -> 27
        TrTr -> 28
        LanguageCode' name -> (error . showText) $ "Unknown LanguageCode: " <> original name

-- | Represents the bounds of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LanguageCode where
    minBound = Arb
    maxBound = TrTr

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"
