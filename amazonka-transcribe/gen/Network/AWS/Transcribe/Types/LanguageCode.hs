{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.LanguageCode (
  LanguageCode (
    ..
    , ArAe
    , ArSa
    , DeCh
    , DeDe
    , EnAb
    , EnAu
    , EnGb
    , EnIe
    , EnIn
    , EnUs
    , EnWl
    , EsEs
    , EsUs
    , FaIr
    , FrCa
    , FrFr
    , HeIl
    , HiIn
    , IdId
    , ItIt
    , JaJp
    , KoKr
    , MsMy
    , NlNl
    , PtBr
    , PtPt
    , RuRu
    , TaIn
    , TeIn
    , TrTr
    , ZhCn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LanguageCode = LanguageCode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern ArAe :: LanguageCode
pattern ArAe = LanguageCode' "ar-AE"

pattern ArSa :: LanguageCode
pattern ArSa = LanguageCode' "ar-SA"

pattern DeCh :: LanguageCode
pattern DeCh = LanguageCode' "de-CH"

pattern DeDe :: LanguageCode
pattern DeDe = LanguageCode' "de-DE"

pattern EnAb :: LanguageCode
pattern EnAb = LanguageCode' "en-AB"

pattern EnAu :: LanguageCode
pattern EnAu = LanguageCode' "en-AU"

pattern EnGb :: LanguageCode
pattern EnGb = LanguageCode' "en-GB"

pattern EnIe :: LanguageCode
pattern EnIe = LanguageCode' "en-IE"

pattern EnIn :: LanguageCode
pattern EnIn = LanguageCode' "en-IN"

pattern EnUs :: LanguageCode
pattern EnUs = LanguageCode' "en-US"

pattern EnWl :: LanguageCode
pattern EnWl = LanguageCode' "en-WL"

pattern EsEs :: LanguageCode
pattern EsEs = LanguageCode' "es-ES"

pattern EsUs :: LanguageCode
pattern EsUs = LanguageCode' "es-US"

pattern FaIr :: LanguageCode
pattern FaIr = LanguageCode' "fa-IR"

pattern FrCa :: LanguageCode
pattern FrCa = LanguageCode' "fr-CA"

pattern FrFr :: LanguageCode
pattern FrFr = LanguageCode' "fr-FR"

pattern HeIl :: LanguageCode
pattern HeIl = LanguageCode' "he-IL"

pattern HiIn :: LanguageCode
pattern HiIn = LanguageCode' "hi-IN"

pattern IdId :: LanguageCode
pattern IdId = LanguageCode' "id-ID"

pattern ItIt :: LanguageCode
pattern ItIt = LanguageCode' "it-IT"

pattern JaJp :: LanguageCode
pattern JaJp = LanguageCode' "ja-JP"

pattern KoKr :: LanguageCode
pattern KoKr = LanguageCode' "ko-KR"

pattern MsMy :: LanguageCode
pattern MsMy = LanguageCode' "ms-MY"

pattern NlNl :: LanguageCode
pattern NlNl = LanguageCode' "nl-NL"

pattern PtBr :: LanguageCode
pattern PtBr = LanguageCode' "pt-BR"

pattern PtPt :: LanguageCode
pattern PtPt = LanguageCode' "pt-PT"

pattern RuRu :: LanguageCode
pattern RuRu = LanguageCode' "ru-RU"

pattern TaIn :: LanguageCode
pattern TaIn = LanguageCode' "ta-IN"

pattern TeIn :: LanguageCode
pattern TeIn = LanguageCode' "te-IN"

pattern TrTr :: LanguageCode
pattern TrTr = LanguageCode' "tr-TR"

pattern ZhCn :: LanguageCode
pattern ZhCn = LanguageCode' "zh-CN"

{-# COMPLETE
  ArAe,
  ArSa,
  DeCh,
  DeDe,
  EnAb,
  EnAu,
  EnGb,
  EnIe,
  EnIn,
  EnUs,
  EnWl,
  EsEs,
  EsUs,
  FaIr,
  FrCa,
  FrFr,
  HeIl,
  HiIn,
  IdId,
  ItIt,
  JaJp,
  KoKr,
  MsMy,
  NlNl,
  PtBr,
  PtPt,
  RuRu,
  TaIn,
  TeIn,
  TrTr,
  ZhCn,
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
        0 -> ArAe
        1 -> ArSa
        2 -> DeCh
        3 -> DeDe
        4 -> EnAb
        5 -> EnAu
        6 -> EnGb
        7 -> EnIe
        8 -> EnIn
        9 -> EnUs
        10 -> EnWl
        11 -> EsEs
        12 -> EsUs
        13 -> FaIr
        14 -> FrCa
        15 -> FrFr
        16 -> HeIl
        17 -> HiIn
        18 -> IdId
        19 -> ItIt
        20 -> JaJp
        21 -> KoKr
        22 -> MsMy
        23 -> NlNl
        24 -> PtBr
        25 -> PtPt
        26 -> RuRu
        27 -> TaIn
        28 -> TeIn
        29 -> TrTr
        30 -> ZhCn
        _ -> (error . showText) $ "Unknown index for LanguageCode: " <> toText i
    fromEnum x = case x of
        ArAe -> 0
        ArSa -> 1
        DeCh -> 2
        DeDe -> 3
        EnAb -> 4
        EnAu -> 5
        EnGb -> 6
        EnIe -> 7
        EnIn -> 8
        EnUs -> 9
        EnWl -> 10
        EsEs -> 11
        EsUs -> 12
        FaIr -> 13
        FrCa -> 14
        FrFr -> 15
        HeIl -> 16
        HiIn -> 17
        IdId -> 18
        ItIt -> 19
        JaJp -> 20
        KoKr -> 21
        MsMy -> 22
        NlNl -> 23
        PtBr -> 24
        PtPt -> 25
        RuRu -> 26
        TaIn -> 27
        TeIn -> 28
        TrTr -> 29
        ZhCn -> 30
        LanguageCode' name -> (error . showText) $ "Unknown LanguageCode: " <> original name

-- | Represents the bounds of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LanguageCode where
    minBound = ArAe
    maxBound = ZhCn

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"
