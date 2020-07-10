{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.CountryCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.CountryCode (
  CountryCode (
    ..
    , AD
    , AE
    , AF
    , AG
    , AI
    , AL
    , AM
    , AN
    , AO
    , AQ
    , AR
    , AS
    , AT
    , AU
    , AW
    , AZ
    , BA
    , BB
    , BD
    , BE
    , BF
    , BG
    , BH
    , BI
    , BJ
    , BL
    , BM
    , BN
    , BO
    , BR
    , BS
    , BT
    , BW
    , BY
    , BZ
    , CA
    , CC
    , CD
    , CF
    , CG
    , CH
    , CI
    , CK
    , CL
    , CM
    , CN
    , CO
    , CR
    , CU
    , CV
    , CX
    , CY
    , CZ
    , DE
    , DJ
    , DK
    , DM
    , DO
    , DZ
    , EC
    , EE
    , EG
    , ER
    , ES
    , ET
    , FI
    , FJ
    , FK
    , FM
    , FO
    , FR
    , GA
    , GB
    , GD
    , GE
    , GH
    , GI
    , GL
    , GM
    , GN
    , GQ
    , GR
    , GT'
    , GU
    , GW
    , GY
    , HK
    , HN
    , HR
    , HT
    , HU
    , IE
    , IL
    , IM
    , IN
    , IQ
    , IR
    , IS
    , IT
    , Id
    , JM
    , JO
    , JP
    , KE
    , KG
    , KH
    , KI
    , KM
    , KN
    , KP
    , KR
    , KW
    , KY
    , KZ
    , LA
    , LB
    , LC
    , LI
    , LK
    , LR
    , LS
    , LT'
    , LU
    , LV
    , LY
    , MA
    , MC
    , MD
    , ME
    , MF
    , MG
    , MH
    , MK
    , ML
    , MM
    , MN
    , MO
    , MP
    , MR
    , MS
    , MT
    , MU
    , MV
    , MW
    , MX
    , MY
    , MZ
    , NA
    , NC
    , NE
    , NG
    , NI
    , NL
    , NO
    , NP
    , NR
    , NU
    , NZ
    , OM
    , PA
    , PE
    , PF
    , PG
    , PH
    , PK
    , PL
    , PM
    , PN
    , PR
    , PT
    , PW
    , PY
    , QA
    , RO
    , RS
    , RU
    , RW
    , SA
    , SB
    , SC
    , SD
    , SE
    , SG
    , SH
    , SI
    , SK
    , SL
    , SM
    , SN
    , SO
    , SR
    , ST
    , SV
    , SY
    , SZ
    , TC
    , TD
    , TG
    , TH
    , TJ
    , TK
    , TL
    , TM
    , TN
    , TO
    , TR
    , TT
    , TV
    , TW
    , TZ
    , UA
    , UG
    , US
    , UY
    , UZ
    , VA
    , VC
    , VE
    , VG
    , VI
    , VN
    , VU
    , WF
    , WS
    , YE
    , YT
    , ZA
    , ZM
    , ZW
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CountryCode = CountryCode' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AD :: CountryCode
pattern AD = CountryCode' "AD"

pattern AE :: CountryCode
pattern AE = CountryCode' "AE"

pattern AF :: CountryCode
pattern AF = CountryCode' "AF"

pattern AG :: CountryCode
pattern AG = CountryCode' "AG"

pattern AI :: CountryCode
pattern AI = CountryCode' "AI"

pattern AL :: CountryCode
pattern AL = CountryCode' "AL"

pattern AM :: CountryCode
pattern AM = CountryCode' "AM"

pattern AN :: CountryCode
pattern AN = CountryCode' "AN"

pattern AO :: CountryCode
pattern AO = CountryCode' "AO"

pattern AQ :: CountryCode
pattern AQ = CountryCode' "AQ"

pattern AR :: CountryCode
pattern AR = CountryCode' "AR"

pattern AS :: CountryCode
pattern AS = CountryCode' "AS"

pattern AT :: CountryCode
pattern AT = CountryCode' "AT"

pattern AU :: CountryCode
pattern AU = CountryCode' "AU"

pattern AW :: CountryCode
pattern AW = CountryCode' "AW"

pattern AZ :: CountryCode
pattern AZ = CountryCode' "AZ"

pattern BA :: CountryCode
pattern BA = CountryCode' "BA"

pattern BB :: CountryCode
pattern BB = CountryCode' "BB"

pattern BD :: CountryCode
pattern BD = CountryCode' "BD"

pattern BE :: CountryCode
pattern BE = CountryCode' "BE"

pattern BF :: CountryCode
pattern BF = CountryCode' "BF"

pattern BG :: CountryCode
pattern BG = CountryCode' "BG"

pattern BH :: CountryCode
pattern BH = CountryCode' "BH"

pattern BI :: CountryCode
pattern BI = CountryCode' "BI"

pattern BJ :: CountryCode
pattern BJ = CountryCode' "BJ"

pattern BL :: CountryCode
pattern BL = CountryCode' "BL"

pattern BM :: CountryCode
pattern BM = CountryCode' "BM"

pattern BN :: CountryCode
pattern BN = CountryCode' "BN"

pattern BO :: CountryCode
pattern BO = CountryCode' "BO"

pattern BR :: CountryCode
pattern BR = CountryCode' "BR"

pattern BS :: CountryCode
pattern BS = CountryCode' "BS"

pattern BT :: CountryCode
pattern BT = CountryCode' "BT"

pattern BW :: CountryCode
pattern BW = CountryCode' "BW"

pattern BY :: CountryCode
pattern BY = CountryCode' "BY"

pattern BZ :: CountryCode
pattern BZ = CountryCode' "BZ"

pattern CA :: CountryCode
pattern CA = CountryCode' "CA"

pattern CC :: CountryCode
pattern CC = CountryCode' "CC"

pattern CD :: CountryCode
pattern CD = CountryCode' "CD"

pattern CF :: CountryCode
pattern CF = CountryCode' "CF"

pattern CG :: CountryCode
pattern CG = CountryCode' "CG"

pattern CH :: CountryCode
pattern CH = CountryCode' "CH"

pattern CI :: CountryCode
pattern CI = CountryCode' "CI"

pattern CK :: CountryCode
pattern CK = CountryCode' "CK"

pattern CL :: CountryCode
pattern CL = CountryCode' "CL"

pattern CM :: CountryCode
pattern CM = CountryCode' "CM"

pattern CN :: CountryCode
pattern CN = CountryCode' "CN"

pattern CO :: CountryCode
pattern CO = CountryCode' "CO"

pattern CR :: CountryCode
pattern CR = CountryCode' "CR"

pattern CU :: CountryCode
pattern CU = CountryCode' "CU"

pattern CV :: CountryCode
pattern CV = CountryCode' "CV"

pattern CX :: CountryCode
pattern CX = CountryCode' "CX"

pattern CY :: CountryCode
pattern CY = CountryCode' "CY"

pattern CZ :: CountryCode
pattern CZ = CountryCode' "CZ"

pattern DE :: CountryCode
pattern DE = CountryCode' "DE"

pattern DJ :: CountryCode
pattern DJ = CountryCode' "DJ"

pattern DK :: CountryCode
pattern DK = CountryCode' "DK"

pattern DM :: CountryCode
pattern DM = CountryCode' "DM"

pattern DO :: CountryCode
pattern DO = CountryCode' "DO"

pattern DZ :: CountryCode
pattern DZ = CountryCode' "DZ"

pattern EC :: CountryCode
pattern EC = CountryCode' "EC"

pattern EE :: CountryCode
pattern EE = CountryCode' "EE"

pattern EG :: CountryCode
pattern EG = CountryCode' "EG"

pattern ER :: CountryCode
pattern ER = CountryCode' "ER"

pattern ES :: CountryCode
pattern ES = CountryCode' "ES"

pattern ET :: CountryCode
pattern ET = CountryCode' "ET"

pattern FI :: CountryCode
pattern FI = CountryCode' "FI"

pattern FJ :: CountryCode
pattern FJ = CountryCode' "FJ"

pattern FK :: CountryCode
pattern FK = CountryCode' "FK"

pattern FM :: CountryCode
pattern FM = CountryCode' "FM"

pattern FO :: CountryCode
pattern FO = CountryCode' "FO"

pattern FR :: CountryCode
pattern FR = CountryCode' "FR"

pattern GA :: CountryCode
pattern GA = CountryCode' "GA"

pattern GB :: CountryCode
pattern GB = CountryCode' "GB"

pattern GD :: CountryCode
pattern GD = CountryCode' "GD"

pattern GE :: CountryCode
pattern GE = CountryCode' "GE"

pattern GH :: CountryCode
pattern GH = CountryCode' "GH"

pattern GI :: CountryCode
pattern GI = CountryCode' "GI"

pattern GL :: CountryCode
pattern GL = CountryCode' "GL"

pattern GM :: CountryCode
pattern GM = CountryCode' "GM"

pattern GN :: CountryCode
pattern GN = CountryCode' "GN"

pattern GQ :: CountryCode
pattern GQ = CountryCode' "GQ"

pattern GR :: CountryCode
pattern GR = CountryCode' "GR"

pattern GT' :: CountryCode
pattern GT' = CountryCode' "GT"

pattern GU :: CountryCode
pattern GU = CountryCode' "GU"

pattern GW :: CountryCode
pattern GW = CountryCode' "GW"

pattern GY :: CountryCode
pattern GY = CountryCode' "GY"

pattern HK :: CountryCode
pattern HK = CountryCode' "HK"

pattern HN :: CountryCode
pattern HN = CountryCode' "HN"

pattern HR :: CountryCode
pattern HR = CountryCode' "HR"

pattern HT :: CountryCode
pattern HT = CountryCode' "HT"

pattern HU :: CountryCode
pattern HU = CountryCode' "HU"

pattern IE :: CountryCode
pattern IE = CountryCode' "IE"

pattern IL :: CountryCode
pattern IL = CountryCode' "IL"

pattern IM :: CountryCode
pattern IM = CountryCode' "IM"

pattern IN :: CountryCode
pattern IN = CountryCode' "IN"

pattern IQ :: CountryCode
pattern IQ = CountryCode' "IQ"

pattern IR :: CountryCode
pattern IR = CountryCode' "IR"

pattern IS :: CountryCode
pattern IS = CountryCode' "IS"

pattern IT :: CountryCode
pattern IT = CountryCode' "IT"

pattern Id :: CountryCode
pattern Id = CountryCode' "ID"

pattern JM :: CountryCode
pattern JM = CountryCode' "JM"

pattern JO :: CountryCode
pattern JO = CountryCode' "JO"

pattern JP :: CountryCode
pattern JP = CountryCode' "JP"

pattern KE :: CountryCode
pattern KE = CountryCode' "KE"

pattern KG :: CountryCode
pattern KG = CountryCode' "KG"

pattern KH :: CountryCode
pattern KH = CountryCode' "KH"

pattern KI :: CountryCode
pattern KI = CountryCode' "KI"

pattern KM :: CountryCode
pattern KM = CountryCode' "KM"

pattern KN :: CountryCode
pattern KN = CountryCode' "KN"

pattern KP :: CountryCode
pattern KP = CountryCode' "KP"

pattern KR :: CountryCode
pattern KR = CountryCode' "KR"

pattern KW :: CountryCode
pattern KW = CountryCode' "KW"

pattern KY :: CountryCode
pattern KY = CountryCode' "KY"

pattern KZ :: CountryCode
pattern KZ = CountryCode' "KZ"

pattern LA :: CountryCode
pattern LA = CountryCode' "LA"

pattern LB :: CountryCode
pattern LB = CountryCode' "LB"

pattern LC :: CountryCode
pattern LC = CountryCode' "LC"

pattern LI :: CountryCode
pattern LI = CountryCode' "LI"

pattern LK :: CountryCode
pattern LK = CountryCode' "LK"

pattern LR :: CountryCode
pattern LR = CountryCode' "LR"

pattern LS :: CountryCode
pattern LS = CountryCode' "LS"

pattern LT' :: CountryCode
pattern LT' = CountryCode' "LT"

pattern LU :: CountryCode
pattern LU = CountryCode' "LU"

pattern LV :: CountryCode
pattern LV = CountryCode' "LV"

pattern LY :: CountryCode
pattern LY = CountryCode' "LY"

pattern MA :: CountryCode
pattern MA = CountryCode' "MA"

pattern MC :: CountryCode
pattern MC = CountryCode' "MC"

pattern MD :: CountryCode
pattern MD = CountryCode' "MD"

pattern ME :: CountryCode
pattern ME = CountryCode' "ME"

pattern MF :: CountryCode
pattern MF = CountryCode' "MF"

pattern MG :: CountryCode
pattern MG = CountryCode' "MG"

pattern MH :: CountryCode
pattern MH = CountryCode' "MH"

pattern MK :: CountryCode
pattern MK = CountryCode' "MK"

pattern ML :: CountryCode
pattern ML = CountryCode' "ML"

pattern MM :: CountryCode
pattern MM = CountryCode' "MM"

pattern MN :: CountryCode
pattern MN = CountryCode' "MN"

pattern MO :: CountryCode
pattern MO = CountryCode' "MO"

pattern MP :: CountryCode
pattern MP = CountryCode' "MP"

pattern MR :: CountryCode
pattern MR = CountryCode' "MR"

pattern MS :: CountryCode
pattern MS = CountryCode' "MS"

pattern MT :: CountryCode
pattern MT = CountryCode' "MT"

pattern MU :: CountryCode
pattern MU = CountryCode' "MU"

pattern MV :: CountryCode
pattern MV = CountryCode' "MV"

pattern MW :: CountryCode
pattern MW = CountryCode' "MW"

pattern MX :: CountryCode
pattern MX = CountryCode' "MX"

pattern MY :: CountryCode
pattern MY = CountryCode' "MY"

pattern MZ :: CountryCode
pattern MZ = CountryCode' "MZ"

pattern NA :: CountryCode
pattern NA = CountryCode' "NA"

pattern NC :: CountryCode
pattern NC = CountryCode' "NC"

pattern NE :: CountryCode
pattern NE = CountryCode' "NE"

pattern NG :: CountryCode
pattern NG = CountryCode' "NG"

pattern NI :: CountryCode
pattern NI = CountryCode' "NI"

pattern NL :: CountryCode
pattern NL = CountryCode' "NL"

pattern NO :: CountryCode
pattern NO = CountryCode' "NO"

pattern NP :: CountryCode
pattern NP = CountryCode' "NP"

pattern NR :: CountryCode
pattern NR = CountryCode' "NR"

pattern NU :: CountryCode
pattern NU = CountryCode' "NU"

pattern NZ :: CountryCode
pattern NZ = CountryCode' "NZ"

pattern OM :: CountryCode
pattern OM = CountryCode' "OM"

pattern PA :: CountryCode
pattern PA = CountryCode' "PA"

pattern PE :: CountryCode
pattern PE = CountryCode' "PE"

pattern PF :: CountryCode
pattern PF = CountryCode' "PF"

pattern PG :: CountryCode
pattern PG = CountryCode' "PG"

pattern PH :: CountryCode
pattern PH = CountryCode' "PH"

pattern PK :: CountryCode
pattern PK = CountryCode' "PK"

pattern PL :: CountryCode
pattern PL = CountryCode' "PL"

pattern PM :: CountryCode
pattern PM = CountryCode' "PM"

pattern PN :: CountryCode
pattern PN = CountryCode' "PN"

pattern PR :: CountryCode
pattern PR = CountryCode' "PR"

pattern PT :: CountryCode
pattern PT = CountryCode' "PT"

pattern PW :: CountryCode
pattern PW = CountryCode' "PW"

pattern PY :: CountryCode
pattern PY = CountryCode' "PY"

pattern QA :: CountryCode
pattern QA = CountryCode' "QA"

pattern RO :: CountryCode
pattern RO = CountryCode' "RO"

pattern RS :: CountryCode
pattern RS = CountryCode' "RS"

pattern RU :: CountryCode
pattern RU = CountryCode' "RU"

pattern RW :: CountryCode
pattern RW = CountryCode' "RW"

pattern SA :: CountryCode
pattern SA = CountryCode' "SA"

pattern SB :: CountryCode
pattern SB = CountryCode' "SB"

pattern SC :: CountryCode
pattern SC = CountryCode' "SC"

pattern SD :: CountryCode
pattern SD = CountryCode' "SD"

pattern SE :: CountryCode
pattern SE = CountryCode' "SE"

pattern SG :: CountryCode
pattern SG = CountryCode' "SG"

pattern SH :: CountryCode
pattern SH = CountryCode' "SH"

pattern SI :: CountryCode
pattern SI = CountryCode' "SI"

pattern SK :: CountryCode
pattern SK = CountryCode' "SK"

pattern SL :: CountryCode
pattern SL = CountryCode' "SL"

pattern SM :: CountryCode
pattern SM = CountryCode' "SM"

pattern SN :: CountryCode
pattern SN = CountryCode' "SN"

pattern SO :: CountryCode
pattern SO = CountryCode' "SO"

pattern SR :: CountryCode
pattern SR = CountryCode' "SR"

pattern ST :: CountryCode
pattern ST = CountryCode' "ST"

pattern SV :: CountryCode
pattern SV = CountryCode' "SV"

pattern SY :: CountryCode
pattern SY = CountryCode' "SY"

pattern SZ :: CountryCode
pattern SZ = CountryCode' "SZ"

pattern TC :: CountryCode
pattern TC = CountryCode' "TC"

pattern TD :: CountryCode
pattern TD = CountryCode' "TD"

pattern TG :: CountryCode
pattern TG = CountryCode' "TG"

pattern TH :: CountryCode
pattern TH = CountryCode' "TH"

pattern TJ :: CountryCode
pattern TJ = CountryCode' "TJ"

pattern TK :: CountryCode
pattern TK = CountryCode' "TK"

pattern TL :: CountryCode
pattern TL = CountryCode' "TL"

pattern TM :: CountryCode
pattern TM = CountryCode' "TM"

pattern TN :: CountryCode
pattern TN = CountryCode' "TN"

pattern TO :: CountryCode
pattern TO = CountryCode' "TO"

pattern TR :: CountryCode
pattern TR = CountryCode' "TR"

pattern TT :: CountryCode
pattern TT = CountryCode' "TT"

pattern TV :: CountryCode
pattern TV = CountryCode' "TV"

pattern TW :: CountryCode
pattern TW = CountryCode' "TW"

pattern TZ :: CountryCode
pattern TZ = CountryCode' "TZ"

pattern UA :: CountryCode
pattern UA = CountryCode' "UA"

pattern UG :: CountryCode
pattern UG = CountryCode' "UG"

pattern US :: CountryCode
pattern US = CountryCode' "US"

pattern UY :: CountryCode
pattern UY = CountryCode' "UY"

pattern UZ :: CountryCode
pattern UZ = CountryCode' "UZ"

pattern VA :: CountryCode
pattern VA = CountryCode' "VA"

pattern VC :: CountryCode
pattern VC = CountryCode' "VC"

pattern VE :: CountryCode
pattern VE = CountryCode' "VE"

pattern VG :: CountryCode
pattern VG = CountryCode' "VG"

pattern VI :: CountryCode
pattern VI = CountryCode' "VI"

pattern VN :: CountryCode
pattern VN = CountryCode' "VN"

pattern VU :: CountryCode
pattern VU = CountryCode' "VU"

pattern WF :: CountryCode
pattern WF = CountryCode' "WF"

pattern WS :: CountryCode
pattern WS = CountryCode' "WS"

pattern YE :: CountryCode
pattern YE = CountryCode' "YE"

pattern YT :: CountryCode
pattern YT = CountryCode' "YT"

pattern ZA :: CountryCode
pattern ZA = CountryCode' "ZA"

pattern ZM :: CountryCode
pattern ZM = CountryCode' "ZM"

pattern ZW :: CountryCode
pattern ZW = CountryCode' "ZW"

{-# COMPLETE
  AD,
  AE,
  AF,
  AG,
  AI,
  AL,
  AM,
  AN,
  AO,
  AQ,
  AR,
  AS,
  AT,
  AU,
  AW,
  AZ,
  BA,
  BB,
  BD,
  BE,
  BF,
  BG,
  BH,
  BI,
  BJ,
  BL,
  BM,
  BN,
  BO,
  BR,
  BS,
  BT,
  BW,
  BY,
  BZ,
  CA,
  CC,
  CD,
  CF,
  CG,
  CH,
  CI,
  CK,
  CL,
  CM,
  CN,
  CO,
  CR,
  CU,
  CV,
  CX,
  CY,
  CZ,
  DE,
  DJ,
  DK,
  DM,
  DO,
  DZ,
  EC,
  EE,
  EG,
  ER,
  ES,
  ET,
  FI,
  FJ,
  FK,
  FM,
  FO,
  FR,
  GA,
  GB,
  GD,
  GE,
  GH,
  GI,
  GL,
  GM,
  GN,
  GQ,
  GR,
  GT',
  GU,
  GW,
  GY,
  HK,
  HN,
  HR,
  HT,
  HU,
  IE,
  IL,
  IM,
  IN,
  IQ,
  IR,
  IS,
  IT,
  Id,
  JM,
  JO,
  JP,
  KE,
  KG,
  KH,
  KI,
  KM,
  KN,
  KP,
  KR,
  KW,
  KY,
  KZ,
  LA,
  LB,
  LC,
  LI,
  LK,
  LR,
  LS,
  LT',
  LU,
  LV,
  LY,
  MA,
  MC,
  MD,
  ME,
  MF,
  MG,
  MH,
  MK,
  ML,
  MM,
  MN,
  MO,
  MP,
  MR,
  MS,
  MT,
  MU,
  MV,
  MW,
  MX,
  MY,
  MZ,
  NA,
  NC,
  NE,
  NG,
  NI,
  NL,
  NO,
  NP,
  NR,
  NU,
  NZ,
  OM,
  PA,
  PE,
  PF,
  PG,
  PH,
  PK,
  PL,
  PM,
  PN,
  PR,
  PT,
  PW,
  PY,
  QA,
  RO,
  RS,
  RU,
  RW,
  SA,
  SB,
  SC,
  SD,
  SE,
  SG,
  SH,
  SI,
  SK,
  SL,
  SM,
  SN,
  SO,
  SR,
  ST,
  SV,
  SY,
  SZ,
  TC,
  TD,
  TG,
  TH,
  TJ,
  TK,
  TL,
  TM,
  TN,
  TO,
  TR,
  TT,
  TV,
  TW,
  TZ,
  UA,
  UG,
  US,
  UY,
  UZ,
  VA,
  VC,
  VE,
  VG,
  VI,
  VN,
  VU,
  WF,
  WS,
  YE,
  YT,
  ZA,
  ZM,
  ZW,
  CountryCode' #-}

instance FromText CountryCode where
    parser = (CountryCode' . mk) <$> takeText

instance ToText CountryCode where
    toText (CountryCode' ci) = original ci

-- | Represents an enum of /known/ $CountryCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CountryCode where
    toEnum i = case i of
        0 -> AD
        1 -> AE
        2 -> AF
        3 -> AG
        4 -> AI
        5 -> AL
        6 -> AM
        7 -> AN
        8 -> AO
        9 -> AQ
        10 -> AR
        11 -> AS
        12 -> AT
        13 -> AU
        14 -> AW
        15 -> AZ
        16 -> BA
        17 -> BB
        18 -> BD
        19 -> BE
        20 -> BF
        21 -> BG
        22 -> BH
        23 -> BI
        24 -> BJ
        25 -> BL
        26 -> BM
        27 -> BN
        28 -> BO
        29 -> BR
        30 -> BS
        31 -> BT
        32 -> BW
        33 -> BY
        34 -> BZ
        35 -> CA
        36 -> CC
        37 -> CD
        38 -> CF
        39 -> CG
        40 -> CH
        41 -> CI
        42 -> CK
        43 -> CL
        44 -> CM
        45 -> CN
        46 -> CO
        47 -> CR
        48 -> CU
        49 -> CV
        50 -> CX
        51 -> CY
        52 -> CZ
        53 -> DE
        54 -> DJ
        55 -> DK
        56 -> DM
        57 -> DO
        58 -> DZ
        59 -> EC
        60 -> EE
        61 -> EG
        62 -> ER
        63 -> ES
        64 -> ET
        65 -> FI
        66 -> FJ
        67 -> FK
        68 -> FM
        69 -> FO
        70 -> FR
        71 -> GA
        72 -> GB
        73 -> GD
        74 -> GE
        75 -> GH
        76 -> GI
        77 -> GL
        78 -> GM
        79 -> GN
        80 -> GQ
        81 -> GR
        82 -> GT'
        83 -> GU
        84 -> GW
        85 -> GY
        86 -> HK
        87 -> HN
        88 -> HR
        89 -> HT
        90 -> HU
        91 -> IE
        92 -> IL
        93 -> IM
        94 -> IN
        95 -> IQ
        96 -> IR
        97 -> IS
        98 -> IT
        99 -> Id
        100 -> JM
        101 -> JO
        102 -> JP
        103 -> KE
        104 -> KG
        105 -> KH
        106 -> KI
        107 -> KM
        108 -> KN
        109 -> KP
        110 -> KR
        111 -> KW
        112 -> KY
        113 -> KZ
        114 -> LA
        115 -> LB
        116 -> LC
        117 -> LI
        118 -> LK
        119 -> LR
        120 -> LS
        121 -> LT'
        122 -> LU
        123 -> LV
        124 -> LY
        125 -> MA
        126 -> MC
        127 -> MD
        128 -> ME
        129 -> MF
        130 -> MG
        131 -> MH
        132 -> MK
        133 -> ML
        134 -> MM
        135 -> MN
        136 -> MO
        137 -> MP
        138 -> MR
        139 -> MS
        140 -> MT
        141 -> MU
        142 -> MV
        143 -> MW
        144 -> MX
        145 -> MY
        146 -> MZ
        147 -> NA
        148 -> NC
        149 -> NE
        150 -> NG
        151 -> NI
        152 -> NL
        153 -> NO
        154 -> NP
        155 -> NR
        156 -> NU
        157 -> NZ
        158 -> OM
        159 -> PA
        160 -> PE
        161 -> PF
        162 -> PG
        163 -> PH
        164 -> PK
        165 -> PL
        166 -> PM
        167 -> PN
        168 -> PR
        169 -> PT
        170 -> PW
        171 -> PY
        172 -> QA
        173 -> RO
        174 -> RS
        175 -> RU
        176 -> RW
        177 -> SA
        178 -> SB
        179 -> SC
        180 -> SD
        181 -> SE
        182 -> SG
        183 -> SH
        184 -> SI
        185 -> SK
        186 -> SL
        187 -> SM
        188 -> SN
        189 -> SO
        190 -> SR
        191 -> ST
        192 -> SV
        193 -> SY
        194 -> SZ
        195 -> TC
        196 -> TD
        197 -> TG
        198 -> TH
        199 -> TJ
        200 -> TK
        201 -> TL
        202 -> TM
        203 -> TN
        204 -> TO
        205 -> TR
        206 -> TT
        207 -> TV
        208 -> TW
        209 -> TZ
        210 -> UA
        211 -> UG
        212 -> US
        213 -> UY
        214 -> UZ
        215 -> VA
        216 -> VC
        217 -> VE
        218 -> VG
        219 -> VI
        220 -> VN
        221 -> VU
        222 -> WF
        223 -> WS
        224 -> YE
        225 -> YT
        226 -> ZA
        227 -> ZM
        228 -> ZW
        _ -> (error . showText) $ "Unknown index for CountryCode: " <> toText i
    fromEnum x = case x of
        AD -> 0
        AE -> 1
        AF -> 2
        AG -> 3
        AI -> 4
        AL -> 5
        AM -> 6
        AN -> 7
        AO -> 8
        AQ -> 9
        AR -> 10
        AS -> 11
        AT -> 12
        AU -> 13
        AW -> 14
        AZ -> 15
        BA -> 16
        BB -> 17
        BD -> 18
        BE -> 19
        BF -> 20
        BG -> 21
        BH -> 22
        BI -> 23
        BJ -> 24
        BL -> 25
        BM -> 26
        BN -> 27
        BO -> 28
        BR -> 29
        BS -> 30
        BT -> 31
        BW -> 32
        BY -> 33
        BZ -> 34
        CA -> 35
        CC -> 36
        CD -> 37
        CF -> 38
        CG -> 39
        CH -> 40
        CI -> 41
        CK -> 42
        CL -> 43
        CM -> 44
        CN -> 45
        CO -> 46
        CR -> 47
        CU -> 48
        CV -> 49
        CX -> 50
        CY -> 51
        CZ -> 52
        DE -> 53
        DJ -> 54
        DK -> 55
        DM -> 56
        DO -> 57
        DZ -> 58
        EC -> 59
        EE -> 60
        EG -> 61
        ER -> 62
        ES -> 63
        ET -> 64
        FI -> 65
        FJ -> 66
        FK -> 67
        FM -> 68
        FO -> 69
        FR -> 70
        GA -> 71
        GB -> 72
        GD -> 73
        GE -> 74
        GH -> 75
        GI -> 76
        GL -> 77
        GM -> 78
        GN -> 79
        GQ -> 80
        GR -> 81
        GT' -> 82
        GU -> 83
        GW -> 84
        GY -> 85
        HK -> 86
        HN -> 87
        HR -> 88
        HT -> 89
        HU -> 90
        IE -> 91
        IL -> 92
        IM -> 93
        IN -> 94
        IQ -> 95
        IR -> 96
        IS -> 97
        IT -> 98
        Id -> 99
        JM -> 100
        JO -> 101
        JP -> 102
        KE -> 103
        KG -> 104
        KH -> 105
        KI -> 106
        KM -> 107
        KN -> 108
        KP -> 109
        KR -> 110
        KW -> 111
        KY -> 112
        KZ -> 113
        LA -> 114
        LB -> 115
        LC -> 116
        LI -> 117
        LK -> 118
        LR -> 119
        LS -> 120
        LT' -> 121
        LU -> 122
        LV -> 123
        LY -> 124
        MA -> 125
        MC -> 126
        MD -> 127
        ME -> 128
        MF -> 129
        MG -> 130
        MH -> 131
        MK -> 132
        ML -> 133
        MM -> 134
        MN -> 135
        MO -> 136
        MP -> 137
        MR -> 138
        MS -> 139
        MT -> 140
        MU -> 141
        MV -> 142
        MW -> 143
        MX -> 144
        MY -> 145
        MZ -> 146
        NA -> 147
        NC -> 148
        NE -> 149
        NG -> 150
        NI -> 151
        NL -> 152
        NO -> 153
        NP -> 154
        NR -> 155
        NU -> 156
        NZ -> 157
        OM -> 158
        PA -> 159
        PE -> 160
        PF -> 161
        PG -> 162
        PH -> 163
        PK -> 164
        PL -> 165
        PM -> 166
        PN -> 167
        PR -> 168
        PT -> 169
        PW -> 170
        PY -> 171
        QA -> 172
        RO -> 173
        RS -> 174
        RU -> 175
        RW -> 176
        SA -> 177
        SB -> 178
        SC -> 179
        SD -> 180
        SE -> 181
        SG -> 182
        SH -> 183
        SI -> 184
        SK -> 185
        SL -> 186
        SM -> 187
        SN -> 188
        SO -> 189
        SR -> 190
        ST -> 191
        SV -> 192
        SY -> 193
        SZ -> 194
        TC -> 195
        TD -> 196
        TG -> 197
        TH -> 198
        TJ -> 199
        TK -> 200
        TL -> 201
        TM -> 202
        TN -> 203
        TO -> 204
        TR -> 205
        TT -> 206
        TV -> 207
        TW -> 208
        TZ -> 209
        UA -> 210
        UG -> 211
        US -> 212
        UY -> 213
        UZ -> 214
        VA -> 215
        VC -> 216
        VE -> 217
        VG -> 218
        VI -> 219
        VN -> 220
        VU -> 221
        WF -> 222
        WS -> 223
        YE -> 224
        YT -> 225
        ZA -> 226
        ZM -> 227
        ZW -> 228
        CountryCode' name -> (error . showText) $ "Unknown CountryCode: " <> original name

-- | Represents the bounds of /known/ $CountryCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CountryCode where
    minBound = AD
    maxBound = ZW

instance Hashable     CountryCode
instance NFData       CountryCode
instance ToByteString CountryCode
instance ToQuery      CountryCode
instance ToHeader     CountryCode

instance ToJSON CountryCode where
    toJSON = toJSONText

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"
