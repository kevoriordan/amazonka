{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.LanguageCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.LanguageCode (
  LanguageCode (
    ..
    , Aar
    , Abk
    , Afr
    , Aka
    , Amh
    , Ara
    , Arg
    , Asm
    , Ava
    , Ave
    , Aym
    , Aze
    , Bak
    , Bam
    , Bel
    , Ben
    , Bih
    , Bis
    , Bod
    , Bos
    , Bre
    , Bul
    , Cat
    , Ces
    , Cha
    , Che
    , Chu
    , Chv
    , Cor
    , Cos
    , Cre
    , Cym
    , Dan
    , Deu
    , Div
    , Dzo
    , Ell
    , Eng
    , Enm
    , Epo
    , Est
    , Eus
    , Ewe
    , Fao
    , Fas
    , Fij
    , Fin
    , Fra
    , Frm
    , Fry
    , Ful
    , Ger
    , Gla
    , Gle
    , Glg
    , Glv
    , Grn
    , Guj
    , Hat
    , Hau
    , Heb
    , Her
    , Hin
    , Hmo
    , Hrv
    , Hun
    , Hye
    , IPk
    , Ibo
    , Ido
    , Iii
    , Iku
    , Ile
    , Ina
    , Ind
    , Isl
    , Ita
    , Jav
    , Jpn
    , Kal
    , Kan
    , Kas
    , Kat
    , Kau
    , Kaz
    , Khm
    , Kik
    , Kin
    , Kir
    , Kom
    , Kon
    , Kor
    , Kua
    , Kur
    , Lao
    , Lat
    , Lav
    , Lim
    , Lin
    , Lit
    , Ltz
    , Lub
    , Lug
    , Mah
    , Mal
    , Mar
    , Mkd
    , Mlg
    , Mlt
    , Mon
    , Mri
    , Msa
    , Mya
    , Nau
    , Nav
    , Nbl
    , Nde
    , Ndo
    , Nep
    , Nld
    , Nno
    , Nob
    , Nor
    , Nya
    , OSs
    , Oci
    , Oji
    , Ori
    , Orj
    , Orm
    , Pan
    , Pli
    , Pol
    , Por
    , Pus
    , Qaa
    , Qpc
    , Que
    , Roh
    , Ron
    , Run
    , Rus
    , Sag
    , San
    , Sin
    , Slk
    , Slv
    , Sme
    , Smo
    , Sna
    , Snd
    , Som
    , Sot
    , Spa
    , Sqi
    , Srb
    , Srd
    , Ssw
    , Sun
    , Swa
    , Swe
    , Tah
    , Tam
    , Tat
    , Tel
    , Tgk
    , Tgl
    , Tha
    , Tir
    , Tng
    , Ton
    , Tsn
    , Tso
    , Tuk
    , Tur
    , Twi
    , Uig
    , Ukr
    , Urd
    , Uzb
    , Ven
    , Vie
    , Vol
    , Wln
    , Wol
    , Xho
    , Yid
    , Yor
    , Zha
    , Zho
    , Zul
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify the language, using the ISO 639-2 three-letter code listed at https://www.loc.gov/standards/iso639-2/php/code_list.php.
data LanguageCode = LanguageCode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Aar :: LanguageCode
pattern Aar = LanguageCode' "AAR"

pattern Abk :: LanguageCode
pattern Abk = LanguageCode' "ABK"

pattern Afr :: LanguageCode
pattern Afr = LanguageCode' "AFR"

pattern Aka :: LanguageCode
pattern Aka = LanguageCode' "AKA"

pattern Amh :: LanguageCode
pattern Amh = LanguageCode' "AMH"

pattern Ara :: LanguageCode
pattern Ara = LanguageCode' "ARA"

pattern Arg :: LanguageCode
pattern Arg = LanguageCode' "ARG"

pattern Asm :: LanguageCode
pattern Asm = LanguageCode' "ASM"

pattern Ava :: LanguageCode
pattern Ava = LanguageCode' "AVA"

pattern Ave :: LanguageCode
pattern Ave = LanguageCode' "AVE"

pattern Aym :: LanguageCode
pattern Aym = LanguageCode' "AYM"

pattern Aze :: LanguageCode
pattern Aze = LanguageCode' "AZE"

pattern Bak :: LanguageCode
pattern Bak = LanguageCode' "BAK"

pattern Bam :: LanguageCode
pattern Bam = LanguageCode' "BAM"

pattern Bel :: LanguageCode
pattern Bel = LanguageCode' "BEL"

pattern Ben :: LanguageCode
pattern Ben = LanguageCode' "BEN"

pattern Bih :: LanguageCode
pattern Bih = LanguageCode' "BIH"

pattern Bis :: LanguageCode
pattern Bis = LanguageCode' "BIS"

pattern Bod :: LanguageCode
pattern Bod = LanguageCode' "BOD"

pattern Bos :: LanguageCode
pattern Bos = LanguageCode' "BOS"

pattern Bre :: LanguageCode
pattern Bre = LanguageCode' "BRE"

pattern Bul :: LanguageCode
pattern Bul = LanguageCode' "BUL"

pattern Cat :: LanguageCode
pattern Cat = LanguageCode' "CAT"

pattern Ces :: LanguageCode
pattern Ces = LanguageCode' "CES"

pattern Cha :: LanguageCode
pattern Cha = LanguageCode' "CHA"

pattern Che :: LanguageCode
pattern Che = LanguageCode' "CHE"

pattern Chu :: LanguageCode
pattern Chu = LanguageCode' "CHU"

pattern Chv :: LanguageCode
pattern Chv = LanguageCode' "CHV"

pattern Cor :: LanguageCode
pattern Cor = LanguageCode' "COR"

pattern Cos :: LanguageCode
pattern Cos = LanguageCode' "COS"

pattern Cre :: LanguageCode
pattern Cre = LanguageCode' "CRE"

pattern Cym :: LanguageCode
pattern Cym = LanguageCode' "CYM"

pattern Dan :: LanguageCode
pattern Dan = LanguageCode' "DAN"

pattern Deu :: LanguageCode
pattern Deu = LanguageCode' "DEU"

pattern Div :: LanguageCode
pattern Div = LanguageCode' "DIV"

pattern Dzo :: LanguageCode
pattern Dzo = LanguageCode' "DZO"

pattern Ell :: LanguageCode
pattern Ell = LanguageCode' "ELL"

pattern Eng :: LanguageCode
pattern Eng = LanguageCode' "ENG"

pattern Enm :: LanguageCode
pattern Enm = LanguageCode' "ENM"

pattern Epo :: LanguageCode
pattern Epo = LanguageCode' "EPO"

pattern Est :: LanguageCode
pattern Est = LanguageCode' "EST"

pattern Eus :: LanguageCode
pattern Eus = LanguageCode' "EUS"

pattern Ewe :: LanguageCode
pattern Ewe = LanguageCode' "EWE"

pattern Fao :: LanguageCode
pattern Fao = LanguageCode' "FAO"

pattern Fas :: LanguageCode
pattern Fas = LanguageCode' "FAS"

pattern Fij :: LanguageCode
pattern Fij = LanguageCode' "FIJ"

pattern Fin :: LanguageCode
pattern Fin = LanguageCode' "FIN"

pattern Fra :: LanguageCode
pattern Fra = LanguageCode' "FRA"

pattern Frm :: LanguageCode
pattern Frm = LanguageCode' "FRM"

pattern Fry :: LanguageCode
pattern Fry = LanguageCode' "FRY"

pattern Ful :: LanguageCode
pattern Ful = LanguageCode' "FUL"

pattern Ger :: LanguageCode
pattern Ger = LanguageCode' "GER"

pattern Gla :: LanguageCode
pattern Gla = LanguageCode' "GLA"

pattern Gle :: LanguageCode
pattern Gle = LanguageCode' "GLE"

pattern Glg :: LanguageCode
pattern Glg = LanguageCode' "GLG"

pattern Glv :: LanguageCode
pattern Glv = LanguageCode' "GLV"

pattern Grn :: LanguageCode
pattern Grn = LanguageCode' "GRN"

pattern Guj :: LanguageCode
pattern Guj = LanguageCode' "GUJ"

pattern Hat :: LanguageCode
pattern Hat = LanguageCode' "HAT"

pattern Hau :: LanguageCode
pattern Hau = LanguageCode' "HAU"

pattern Heb :: LanguageCode
pattern Heb = LanguageCode' "HEB"

pattern Her :: LanguageCode
pattern Her = LanguageCode' "HER"

pattern Hin :: LanguageCode
pattern Hin = LanguageCode' "HIN"

pattern Hmo :: LanguageCode
pattern Hmo = LanguageCode' "HMO"

pattern Hrv :: LanguageCode
pattern Hrv = LanguageCode' "HRV"

pattern Hun :: LanguageCode
pattern Hun = LanguageCode' "HUN"

pattern Hye :: LanguageCode
pattern Hye = LanguageCode' "HYE"

pattern IPk :: LanguageCode
pattern IPk = LanguageCode' "IPK"

pattern Ibo :: LanguageCode
pattern Ibo = LanguageCode' "IBO"

pattern Ido :: LanguageCode
pattern Ido = LanguageCode' "IDO"

pattern Iii :: LanguageCode
pattern Iii = LanguageCode' "III"

pattern Iku :: LanguageCode
pattern Iku = LanguageCode' "IKU"

pattern Ile :: LanguageCode
pattern Ile = LanguageCode' "ILE"

pattern Ina :: LanguageCode
pattern Ina = LanguageCode' "INA"

pattern Ind :: LanguageCode
pattern Ind = LanguageCode' "IND"

pattern Isl :: LanguageCode
pattern Isl = LanguageCode' "ISL"

pattern Ita :: LanguageCode
pattern Ita = LanguageCode' "ITA"

pattern Jav :: LanguageCode
pattern Jav = LanguageCode' "JAV"

pattern Jpn :: LanguageCode
pattern Jpn = LanguageCode' "JPN"

pattern Kal :: LanguageCode
pattern Kal = LanguageCode' "KAL"

pattern Kan :: LanguageCode
pattern Kan = LanguageCode' "KAN"

pattern Kas :: LanguageCode
pattern Kas = LanguageCode' "KAS"

pattern Kat :: LanguageCode
pattern Kat = LanguageCode' "KAT"

pattern Kau :: LanguageCode
pattern Kau = LanguageCode' "KAU"

pattern Kaz :: LanguageCode
pattern Kaz = LanguageCode' "KAZ"

pattern Khm :: LanguageCode
pattern Khm = LanguageCode' "KHM"

pattern Kik :: LanguageCode
pattern Kik = LanguageCode' "KIK"

pattern Kin :: LanguageCode
pattern Kin = LanguageCode' "KIN"

pattern Kir :: LanguageCode
pattern Kir = LanguageCode' "KIR"

pattern Kom :: LanguageCode
pattern Kom = LanguageCode' "KOM"

pattern Kon :: LanguageCode
pattern Kon = LanguageCode' "KON"

pattern Kor :: LanguageCode
pattern Kor = LanguageCode' "KOR"

pattern Kua :: LanguageCode
pattern Kua = LanguageCode' "KUA"

pattern Kur :: LanguageCode
pattern Kur = LanguageCode' "KUR"

pattern Lao :: LanguageCode
pattern Lao = LanguageCode' "LAO"

pattern Lat :: LanguageCode
pattern Lat = LanguageCode' "LAT"

pattern Lav :: LanguageCode
pattern Lav = LanguageCode' "LAV"

pattern Lim :: LanguageCode
pattern Lim = LanguageCode' "LIM"

pattern Lin :: LanguageCode
pattern Lin = LanguageCode' "LIN"

pattern Lit :: LanguageCode
pattern Lit = LanguageCode' "LIT"

pattern Ltz :: LanguageCode
pattern Ltz = LanguageCode' "LTZ"

pattern Lub :: LanguageCode
pattern Lub = LanguageCode' "LUB"

pattern Lug :: LanguageCode
pattern Lug = LanguageCode' "LUG"

pattern Mah :: LanguageCode
pattern Mah = LanguageCode' "MAH"

pattern Mal :: LanguageCode
pattern Mal = LanguageCode' "MAL"

pattern Mar :: LanguageCode
pattern Mar = LanguageCode' "MAR"

pattern Mkd :: LanguageCode
pattern Mkd = LanguageCode' "MKD"

pattern Mlg :: LanguageCode
pattern Mlg = LanguageCode' "MLG"

pattern Mlt :: LanguageCode
pattern Mlt = LanguageCode' "MLT"

pattern Mon :: LanguageCode
pattern Mon = LanguageCode' "MON"

pattern Mri :: LanguageCode
pattern Mri = LanguageCode' "MRI"

pattern Msa :: LanguageCode
pattern Msa = LanguageCode' "MSA"

pattern Mya :: LanguageCode
pattern Mya = LanguageCode' "MYA"

pattern Nau :: LanguageCode
pattern Nau = LanguageCode' "NAU"

pattern Nav :: LanguageCode
pattern Nav = LanguageCode' "NAV"

pattern Nbl :: LanguageCode
pattern Nbl = LanguageCode' "NBL"

pattern Nde :: LanguageCode
pattern Nde = LanguageCode' "NDE"

pattern Ndo :: LanguageCode
pattern Ndo = LanguageCode' "NDO"

pattern Nep :: LanguageCode
pattern Nep = LanguageCode' "NEP"

pattern Nld :: LanguageCode
pattern Nld = LanguageCode' "NLD"

pattern Nno :: LanguageCode
pattern Nno = LanguageCode' "NNO"

pattern Nob :: LanguageCode
pattern Nob = LanguageCode' "NOB"

pattern Nor :: LanguageCode
pattern Nor = LanguageCode' "NOR"

pattern Nya :: LanguageCode
pattern Nya = LanguageCode' "NYA"

pattern OSs :: LanguageCode
pattern OSs = LanguageCode' "OSS"

pattern Oci :: LanguageCode
pattern Oci = LanguageCode' "OCI"

pattern Oji :: LanguageCode
pattern Oji = LanguageCode' "OJI"

pattern Ori :: LanguageCode
pattern Ori = LanguageCode' "ORI"

pattern Orj :: LanguageCode
pattern Orj = LanguageCode' "ORJ"

pattern Orm :: LanguageCode
pattern Orm = LanguageCode' "ORM"

pattern Pan :: LanguageCode
pattern Pan = LanguageCode' "PAN"

pattern Pli :: LanguageCode
pattern Pli = LanguageCode' "PLI"

pattern Pol :: LanguageCode
pattern Pol = LanguageCode' "POL"

pattern Por :: LanguageCode
pattern Por = LanguageCode' "POR"

pattern Pus :: LanguageCode
pattern Pus = LanguageCode' "PUS"

pattern Qaa :: LanguageCode
pattern Qaa = LanguageCode' "QAA"

pattern Qpc :: LanguageCode
pattern Qpc = LanguageCode' "QPC"

pattern Que :: LanguageCode
pattern Que = LanguageCode' "QUE"

pattern Roh :: LanguageCode
pattern Roh = LanguageCode' "ROH"

pattern Ron :: LanguageCode
pattern Ron = LanguageCode' "RON"

pattern Run :: LanguageCode
pattern Run = LanguageCode' "RUN"

pattern Rus :: LanguageCode
pattern Rus = LanguageCode' "RUS"

pattern Sag :: LanguageCode
pattern Sag = LanguageCode' "SAG"

pattern San :: LanguageCode
pattern San = LanguageCode' "SAN"

pattern Sin :: LanguageCode
pattern Sin = LanguageCode' "SIN"

pattern Slk :: LanguageCode
pattern Slk = LanguageCode' "SLK"

pattern Slv :: LanguageCode
pattern Slv = LanguageCode' "SLV"

pattern Sme :: LanguageCode
pattern Sme = LanguageCode' "SME"

pattern Smo :: LanguageCode
pattern Smo = LanguageCode' "SMO"

pattern Sna :: LanguageCode
pattern Sna = LanguageCode' "SNA"

pattern Snd :: LanguageCode
pattern Snd = LanguageCode' "SND"

pattern Som :: LanguageCode
pattern Som = LanguageCode' "SOM"

pattern Sot :: LanguageCode
pattern Sot = LanguageCode' "SOT"

pattern Spa :: LanguageCode
pattern Spa = LanguageCode' "SPA"

pattern Sqi :: LanguageCode
pattern Sqi = LanguageCode' "SQI"

pattern Srb :: LanguageCode
pattern Srb = LanguageCode' "SRB"

pattern Srd :: LanguageCode
pattern Srd = LanguageCode' "SRD"

pattern Ssw :: LanguageCode
pattern Ssw = LanguageCode' "SSW"

pattern Sun :: LanguageCode
pattern Sun = LanguageCode' "SUN"

pattern Swa :: LanguageCode
pattern Swa = LanguageCode' "SWA"

pattern Swe :: LanguageCode
pattern Swe = LanguageCode' "SWE"

pattern Tah :: LanguageCode
pattern Tah = LanguageCode' "TAH"

pattern Tam :: LanguageCode
pattern Tam = LanguageCode' "TAM"

pattern Tat :: LanguageCode
pattern Tat = LanguageCode' "TAT"

pattern Tel :: LanguageCode
pattern Tel = LanguageCode' "TEL"

pattern Tgk :: LanguageCode
pattern Tgk = LanguageCode' "TGK"

pattern Tgl :: LanguageCode
pattern Tgl = LanguageCode' "TGL"

pattern Tha :: LanguageCode
pattern Tha = LanguageCode' "THA"

pattern Tir :: LanguageCode
pattern Tir = LanguageCode' "TIR"

pattern Tng :: LanguageCode
pattern Tng = LanguageCode' "TNG"

pattern Ton :: LanguageCode
pattern Ton = LanguageCode' "TON"

pattern Tsn :: LanguageCode
pattern Tsn = LanguageCode' "TSN"

pattern Tso :: LanguageCode
pattern Tso = LanguageCode' "TSO"

pattern Tuk :: LanguageCode
pattern Tuk = LanguageCode' "TUK"

pattern Tur :: LanguageCode
pattern Tur = LanguageCode' "TUR"

pattern Twi :: LanguageCode
pattern Twi = LanguageCode' "TWI"

pattern Uig :: LanguageCode
pattern Uig = LanguageCode' "UIG"

pattern Ukr :: LanguageCode
pattern Ukr = LanguageCode' "UKR"

pattern Urd :: LanguageCode
pattern Urd = LanguageCode' "URD"

pattern Uzb :: LanguageCode
pattern Uzb = LanguageCode' "UZB"

pattern Ven :: LanguageCode
pattern Ven = LanguageCode' "VEN"

pattern Vie :: LanguageCode
pattern Vie = LanguageCode' "VIE"

pattern Vol :: LanguageCode
pattern Vol = LanguageCode' "VOL"

pattern Wln :: LanguageCode
pattern Wln = LanguageCode' "WLN"

pattern Wol :: LanguageCode
pattern Wol = LanguageCode' "WOL"

pattern Xho :: LanguageCode
pattern Xho = LanguageCode' "XHO"

pattern Yid :: LanguageCode
pattern Yid = LanguageCode' "YID"

pattern Yor :: LanguageCode
pattern Yor = LanguageCode' "YOR"

pattern Zha :: LanguageCode
pattern Zha = LanguageCode' "ZHA"

pattern Zho :: LanguageCode
pattern Zho = LanguageCode' "ZHO"

pattern Zul :: LanguageCode
pattern Zul = LanguageCode' "ZUL"

{-# COMPLETE
  Aar,
  Abk,
  Afr,
  Aka,
  Amh,
  Ara,
  Arg,
  Asm,
  Ava,
  Ave,
  Aym,
  Aze,
  Bak,
  Bam,
  Bel,
  Ben,
  Bih,
  Bis,
  Bod,
  Bos,
  Bre,
  Bul,
  Cat,
  Ces,
  Cha,
  Che,
  Chu,
  Chv,
  Cor,
  Cos,
  Cre,
  Cym,
  Dan,
  Deu,
  Div,
  Dzo,
  Ell,
  Eng,
  Enm,
  Epo,
  Est,
  Eus,
  Ewe,
  Fao,
  Fas,
  Fij,
  Fin,
  Fra,
  Frm,
  Fry,
  Ful,
  Ger,
  Gla,
  Gle,
  Glg,
  Glv,
  Grn,
  Guj,
  Hat,
  Hau,
  Heb,
  Her,
  Hin,
  Hmo,
  Hrv,
  Hun,
  Hye,
  IPk,
  Ibo,
  Ido,
  Iii,
  Iku,
  Ile,
  Ina,
  Ind,
  Isl,
  Ita,
  Jav,
  Jpn,
  Kal,
  Kan,
  Kas,
  Kat,
  Kau,
  Kaz,
  Khm,
  Kik,
  Kin,
  Kir,
  Kom,
  Kon,
  Kor,
  Kua,
  Kur,
  Lao,
  Lat,
  Lav,
  Lim,
  Lin,
  Lit,
  Ltz,
  Lub,
  Lug,
  Mah,
  Mal,
  Mar,
  Mkd,
  Mlg,
  Mlt,
  Mon,
  Mri,
  Msa,
  Mya,
  Nau,
  Nav,
  Nbl,
  Nde,
  Ndo,
  Nep,
  Nld,
  Nno,
  Nob,
  Nor,
  Nya,
  OSs,
  Oci,
  Oji,
  Ori,
  Orj,
  Orm,
  Pan,
  Pli,
  Pol,
  Por,
  Pus,
  Qaa,
  Qpc,
  Que,
  Roh,
  Ron,
  Run,
  Rus,
  Sag,
  San,
  Sin,
  Slk,
  Slv,
  Sme,
  Smo,
  Sna,
  Snd,
  Som,
  Sot,
  Spa,
  Sqi,
  Srb,
  Srd,
  Ssw,
  Sun,
  Swa,
  Swe,
  Tah,
  Tam,
  Tat,
  Tel,
  Tgk,
  Tgl,
  Tha,
  Tir,
  Tng,
  Ton,
  Tsn,
  Tso,
  Tuk,
  Tur,
  Twi,
  Uig,
  Ukr,
  Urd,
  Uzb,
  Ven,
  Vie,
  Vol,
  Wln,
  Wol,
  Xho,
  Yid,
  Yor,
  Zha,
  Zho,
  Zul,
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
        0 -> Aar
        1 -> Abk
        2 -> Afr
        3 -> Aka
        4 -> Amh
        5 -> Ara
        6 -> Arg
        7 -> Asm
        8 -> Ava
        9 -> Ave
        10 -> Aym
        11 -> Aze
        12 -> Bak
        13 -> Bam
        14 -> Bel
        15 -> Ben
        16 -> Bih
        17 -> Bis
        18 -> Bod
        19 -> Bos
        20 -> Bre
        21 -> Bul
        22 -> Cat
        23 -> Ces
        24 -> Cha
        25 -> Che
        26 -> Chu
        27 -> Chv
        28 -> Cor
        29 -> Cos
        30 -> Cre
        31 -> Cym
        32 -> Dan
        33 -> Deu
        34 -> Div
        35 -> Dzo
        36 -> Ell
        37 -> Eng
        38 -> Enm
        39 -> Epo
        40 -> Est
        41 -> Eus
        42 -> Ewe
        43 -> Fao
        44 -> Fas
        45 -> Fij
        46 -> Fin
        47 -> Fra
        48 -> Frm
        49 -> Fry
        50 -> Ful
        51 -> Ger
        52 -> Gla
        53 -> Gle
        54 -> Glg
        55 -> Glv
        56 -> Grn
        57 -> Guj
        58 -> Hat
        59 -> Hau
        60 -> Heb
        61 -> Her
        62 -> Hin
        63 -> Hmo
        64 -> Hrv
        65 -> Hun
        66 -> Hye
        67 -> IPk
        68 -> Ibo
        69 -> Ido
        70 -> Iii
        71 -> Iku
        72 -> Ile
        73 -> Ina
        74 -> Ind
        75 -> Isl
        76 -> Ita
        77 -> Jav
        78 -> Jpn
        79 -> Kal
        80 -> Kan
        81 -> Kas
        82 -> Kat
        83 -> Kau
        84 -> Kaz
        85 -> Khm
        86 -> Kik
        87 -> Kin
        88 -> Kir
        89 -> Kom
        90 -> Kon
        91 -> Kor
        92 -> Kua
        93 -> Kur
        94 -> Lao
        95 -> Lat
        96 -> Lav
        97 -> Lim
        98 -> Lin
        99 -> Lit
        100 -> Ltz
        101 -> Lub
        102 -> Lug
        103 -> Mah
        104 -> Mal
        105 -> Mar
        106 -> Mkd
        107 -> Mlg
        108 -> Mlt
        109 -> Mon
        110 -> Mri
        111 -> Msa
        112 -> Mya
        113 -> Nau
        114 -> Nav
        115 -> Nbl
        116 -> Nde
        117 -> Ndo
        118 -> Nep
        119 -> Nld
        120 -> Nno
        121 -> Nob
        122 -> Nor
        123 -> Nya
        124 -> OSs
        125 -> Oci
        126 -> Oji
        127 -> Ori
        128 -> Orj
        129 -> Orm
        130 -> Pan
        131 -> Pli
        132 -> Pol
        133 -> Por
        134 -> Pus
        135 -> Qaa
        136 -> Qpc
        137 -> Que
        138 -> Roh
        139 -> Ron
        140 -> Run
        141 -> Rus
        142 -> Sag
        143 -> San
        144 -> Sin
        145 -> Slk
        146 -> Slv
        147 -> Sme
        148 -> Smo
        149 -> Sna
        150 -> Snd
        151 -> Som
        152 -> Sot
        153 -> Spa
        154 -> Sqi
        155 -> Srb
        156 -> Srd
        157 -> Ssw
        158 -> Sun
        159 -> Swa
        160 -> Swe
        161 -> Tah
        162 -> Tam
        163 -> Tat
        164 -> Tel
        165 -> Tgk
        166 -> Tgl
        167 -> Tha
        168 -> Tir
        169 -> Tng
        170 -> Ton
        171 -> Tsn
        172 -> Tso
        173 -> Tuk
        174 -> Tur
        175 -> Twi
        176 -> Uig
        177 -> Ukr
        178 -> Urd
        179 -> Uzb
        180 -> Ven
        181 -> Vie
        182 -> Vol
        183 -> Wln
        184 -> Wol
        185 -> Xho
        186 -> Yid
        187 -> Yor
        188 -> Zha
        189 -> Zho
        190 -> Zul
        _ -> (error . showText) $ "Unknown index for LanguageCode: " <> toText i
    fromEnum x = case x of
        Aar -> 0
        Abk -> 1
        Afr -> 2
        Aka -> 3
        Amh -> 4
        Ara -> 5
        Arg -> 6
        Asm -> 7
        Ava -> 8
        Ave -> 9
        Aym -> 10
        Aze -> 11
        Bak -> 12
        Bam -> 13
        Bel -> 14
        Ben -> 15
        Bih -> 16
        Bis -> 17
        Bod -> 18
        Bos -> 19
        Bre -> 20
        Bul -> 21
        Cat -> 22
        Ces -> 23
        Cha -> 24
        Che -> 25
        Chu -> 26
        Chv -> 27
        Cor -> 28
        Cos -> 29
        Cre -> 30
        Cym -> 31
        Dan -> 32
        Deu -> 33
        Div -> 34
        Dzo -> 35
        Ell -> 36
        Eng -> 37
        Enm -> 38
        Epo -> 39
        Est -> 40
        Eus -> 41
        Ewe -> 42
        Fao -> 43
        Fas -> 44
        Fij -> 45
        Fin -> 46
        Fra -> 47
        Frm -> 48
        Fry -> 49
        Ful -> 50
        Ger -> 51
        Gla -> 52
        Gle -> 53
        Glg -> 54
        Glv -> 55
        Grn -> 56
        Guj -> 57
        Hat -> 58
        Hau -> 59
        Heb -> 60
        Her -> 61
        Hin -> 62
        Hmo -> 63
        Hrv -> 64
        Hun -> 65
        Hye -> 66
        IPk -> 67
        Ibo -> 68
        Ido -> 69
        Iii -> 70
        Iku -> 71
        Ile -> 72
        Ina -> 73
        Ind -> 74
        Isl -> 75
        Ita -> 76
        Jav -> 77
        Jpn -> 78
        Kal -> 79
        Kan -> 80
        Kas -> 81
        Kat -> 82
        Kau -> 83
        Kaz -> 84
        Khm -> 85
        Kik -> 86
        Kin -> 87
        Kir -> 88
        Kom -> 89
        Kon -> 90
        Kor -> 91
        Kua -> 92
        Kur -> 93
        Lao -> 94
        Lat -> 95
        Lav -> 96
        Lim -> 97
        Lin -> 98
        Lit -> 99
        Ltz -> 100
        Lub -> 101
        Lug -> 102
        Mah -> 103
        Mal -> 104
        Mar -> 105
        Mkd -> 106
        Mlg -> 107
        Mlt -> 108
        Mon -> 109
        Mri -> 110
        Msa -> 111
        Mya -> 112
        Nau -> 113
        Nav -> 114
        Nbl -> 115
        Nde -> 116
        Ndo -> 117
        Nep -> 118
        Nld -> 119
        Nno -> 120
        Nob -> 121
        Nor -> 122
        Nya -> 123
        OSs -> 124
        Oci -> 125
        Oji -> 126
        Ori -> 127
        Orj -> 128
        Orm -> 129
        Pan -> 130
        Pli -> 131
        Pol -> 132
        Por -> 133
        Pus -> 134
        Qaa -> 135
        Qpc -> 136
        Que -> 137
        Roh -> 138
        Ron -> 139
        Run -> 140
        Rus -> 141
        Sag -> 142
        San -> 143
        Sin -> 144
        Slk -> 145
        Slv -> 146
        Sme -> 147
        Smo -> 148
        Sna -> 149
        Snd -> 150
        Som -> 151
        Sot -> 152
        Spa -> 153
        Sqi -> 154
        Srb -> 155
        Srd -> 156
        Ssw -> 157
        Sun -> 158
        Swa -> 159
        Swe -> 160
        Tah -> 161
        Tam -> 162
        Tat -> 163
        Tel -> 164
        Tgk -> 165
        Tgl -> 166
        Tha -> 167
        Tir -> 168
        Tng -> 169
        Ton -> 170
        Tsn -> 171
        Tso -> 172
        Tuk -> 173
        Tur -> 174
        Twi -> 175
        Uig -> 176
        Ukr -> 177
        Urd -> 178
        Uzb -> 179
        Ven -> 180
        Vie -> 181
        Vol -> 182
        Wln -> 183
        Wol -> 184
        Xho -> 185
        Yid -> 186
        Yor -> 187
        Zha -> 188
        Zho -> 189
        Zul -> 190
        LanguageCode' name -> (error . showText) $ "Unknown LanguageCode: " <> original name

-- | Represents the bounds of /known/ $LanguageCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LanguageCode where
    minBound = Aar
    maxBound = Zul

instance Hashable     LanguageCode
instance NFData       LanguageCode
instance ToByteString LanguageCode
instance ToQuery      LanguageCode
instance ToHeader     LanguageCode

instance ToJSON LanguageCode where
    toJSON = toJSONText

instance FromJSON LanguageCode where
    parseJSON = parseJSONText "LanguageCode"
