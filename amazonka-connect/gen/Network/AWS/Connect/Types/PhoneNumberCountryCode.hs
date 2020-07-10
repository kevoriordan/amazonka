{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneNumberCountryCode (
  PhoneNumberCountryCode (
    ..
    , PNCCAD
    , PNCCAE
    , PNCCAF
    , PNCCAG
    , PNCCAI
    , PNCCAL
    , PNCCAM
    , PNCCAN
    , PNCCAO
    , PNCCAQ
    , PNCCAR
    , PNCCAS
    , PNCCAT
    , PNCCAU
    , PNCCAW
    , PNCCAZ
    , PNCCBA
    , PNCCBB
    , PNCCBD
    , PNCCBE
    , PNCCBF
    , PNCCBG
    , PNCCBH
    , PNCCBI
    , PNCCBJ
    , PNCCBL
    , PNCCBM
    , PNCCBN
    , PNCCBO
    , PNCCBR
    , PNCCBS
    , PNCCBT
    , PNCCBW
    , PNCCBY
    , PNCCBZ
    , PNCCCA
    , PNCCCC
    , PNCCCD
    , PNCCCF
    , PNCCCG
    , PNCCCH
    , PNCCCI
    , PNCCCK
    , PNCCCL
    , PNCCCM
    , PNCCCN
    , PNCCCO
    , PNCCCR
    , PNCCCU
    , PNCCCV
    , PNCCCW
    , PNCCCX
    , PNCCCY
    , PNCCCZ
    , PNCCDE
    , PNCCDJ
    , PNCCDK
    , PNCCDM
    , PNCCDO
    , PNCCDZ
    , PNCCEC
    , PNCCEE
    , PNCCEG
    , PNCCEH
    , PNCCER
    , PNCCES
    , PNCCET
    , PNCCFI
    , PNCCFJ
    , PNCCFK
    , PNCCFM
    , PNCCFO
    , PNCCFR
    , PNCCGA
    , PNCCGB
    , PNCCGD
    , PNCCGE
    , PNCCGG
    , PNCCGH
    , PNCCGI
    , PNCCGL
    , PNCCGM
    , PNCCGN
    , PNCCGQ
    , PNCCGR
    , PNCCGT'
    , PNCCGU
    , PNCCGW
    , PNCCGY
    , PNCCHK
    , PNCCHN
    , PNCCHR
    , PNCCHT
    , PNCCHU
    , PNCCIE
    , PNCCIL
    , PNCCIM
    , PNCCIN
    , PNCCIO
    , PNCCIQ
    , PNCCIR
    , PNCCIS
    , PNCCIT
    , PNCCId
    , PNCCJE
    , PNCCJM
    , PNCCJO
    , PNCCJP
    , PNCCKE
    , PNCCKG
    , PNCCKH
    , PNCCKI
    , PNCCKM
    , PNCCKN
    , PNCCKP
    , PNCCKR
    , PNCCKW
    , PNCCKY
    , PNCCKZ
    , PNCCLA
    , PNCCLB
    , PNCCLC
    , PNCCLI
    , PNCCLK
    , PNCCLR
    , PNCCLS
    , PNCCLT'
    , PNCCLU
    , PNCCLV
    , PNCCLY
    , PNCCMA
    , PNCCMC
    , PNCCMD
    , PNCCME
    , PNCCMF
    , PNCCMG
    , PNCCMH
    , PNCCMK
    , PNCCML
    , PNCCMM
    , PNCCMN
    , PNCCMO
    , PNCCMP
    , PNCCMR
    , PNCCMS
    , PNCCMT
    , PNCCMU
    , PNCCMV
    , PNCCMW
    , PNCCMX
    , PNCCMY
    , PNCCMZ
    , PNCCNA
    , PNCCNC
    , PNCCNE
    , PNCCNG
    , PNCCNI
    , PNCCNL
    , PNCCNO
    , PNCCNP
    , PNCCNR
    , PNCCNU
    , PNCCNZ
    , PNCCOM
    , PNCCPA
    , PNCCPE
    , PNCCPF
    , PNCCPG
    , PNCCPH
    , PNCCPK
    , PNCCPL
    , PNCCPM
    , PNCCPN
    , PNCCPR
    , PNCCPT
    , PNCCPW
    , PNCCPY
    , PNCCQA
    , PNCCRE
    , PNCCRO
    , PNCCRS
    , PNCCRU
    , PNCCRW
    , PNCCSA
    , PNCCSB
    , PNCCSC
    , PNCCSD
    , PNCCSE
    , PNCCSG
    , PNCCSH
    , PNCCSI
    , PNCCSJ
    , PNCCSK
    , PNCCSL
    , PNCCSM
    , PNCCSN
    , PNCCSO
    , PNCCSR
    , PNCCST
    , PNCCSV
    , PNCCSX
    , PNCCSY
    , PNCCSZ
    , PNCCTC
    , PNCCTD
    , PNCCTG
    , PNCCTH
    , PNCCTJ
    , PNCCTK
    , PNCCTL
    , PNCCTM
    , PNCCTN
    , PNCCTO
    , PNCCTR
    , PNCCTT
    , PNCCTV
    , PNCCTW
    , PNCCTZ
    , PNCCUA
    , PNCCUG
    , PNCCUS
    , PNCCUY
    , PNCCUZ
    , PNCCVA
    , PNCCVC
    , PNCCVE
    , PNCCVG
    , PNCCVI
    , PNCCVN
    , PNCCVU
    , PNCCWF
    , PNCCWS
    , PNCCYE
    , PNCCYT
    , PNCCZA
    , PNCCZM
    , PNCCZW
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PhoneNumberCountryCode = PhoneNumberCountryCode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern PNCCAD :: PhoneNumberCountryCode
pattern PNCCAD = PhoneNumberCountryCode' "AD"

pattern PNCCAE :: PhoneNumberCountryCode
pattern PNCCAE = PhoneNumberCountryCode' "AE"

pattern PNCCAF :: PhoneNumberCountryCode
pattern PNCCAF = PhoneNumberCountryCode' "AF"

pattern PNCCAG :: PhoneNumberCountryCode
pattern PNCCAG = PhoneNumberCountryCode' "AG"

pattern PNCCAI :: PhoneNumberCountryCode
pattern PNCCAI = PhoneNumberCountryCode' "AI"

pattern PNCCAL :: PhoneNumberCountryCode
pattern PNCCAL = PhoneNumberCountryCode' "AL"

pattern PNCCAM :: PhoneNumberCountryCode
pattern PNCCAM = PhoneNumberCountryCode' "AM"

pattern PNCCAN :: PhoneNumberCountryCode
pattern PNCCAN = PhoneNumberCountryCode' "AN"

pattern PNCCAO :: PhoneNumberCountryCode
pattern PNCCAO = PhoneNumberCountryCode' "AO"

pattern PNCCAQ :: PhoneNumberCountryCode
pattern PNCCAQ = PhoneNumberCountryCode' "AQ"

pattern PNCCAR :: PhoneNumberCountryCode
pattern PNCCAR = PhoneNumberCountryCode' "AR"

pattern PNCCAS :: PhoneNumberCountryCode
pattern PNCCAS = PhoneNumberCountryCode' "AS"

pattern PNCCAT :: PhoneNumberCountryCode
pattern PNCCAT = PhoneNumberCountryCode' "AT"

pattern PNCCAU :: PhoneNumberCountryCode
pattern PNCCAU = PhoneNumberCountryCode' "AU"

pattern PNCCAW :: PhoneNumberCountryCode
pattern PNCCAW = PhoneNumberCountryCode' "AW"

pattern PNCCAZ :: PhoneNumberCountryCode
pattern PNCCAZ = PhoneNumberCountryCode' "AZ"

pattern PNCCBA :: PhoneNumberCountryCode
pattern PNCCBA = PhoneNumberCountryCode' "BA"

pattern PNCCBB :: PhoneNumberCountryCode
pattern PNCCBB = PhoneNumberCountryCode' "BB"

pattern PNCCBD :: PhoneNumberCountryCode
pattern PNCCBD = PhoneNumberCountryCode' "BD"

pattern PNCCBE :: PhoneNumberCountryCode
pattern PNCCBE = PhoneNumberCountryCode' "BE"

pattern PNCCBF :: PhoneNumberCountryCode
pattern PNCCBF = PhoneNumberCountryCode' "BF"

pattern PNCCBG :: PhoneNumberCountryCode
pattern PNCCBG = PhoneNumberCountryCode' "BG"

pattern PNCCBH :: PhoneNumberCountryCode
pattern PNCCBH = PhoneNumberCountryCode' "BH"

pattern PNCCBI :: PhoneNumberCountryCode
pattern PNCCBI = PhoneNumberCountryCode' "BI"

pattern PNCCBJ :: PhoneNumberCountryCode
pattern PNCCBJ = PhoneNumberCountryCode' "BJ"

pattern PNCCBL :: PhoneNumberCountryCode
pattern PNCCBL = PhoneNumberCountryCode' "BL"

pattern PNCCBM :: PhoneNumberCountryCode
pattern PNCCBM = PhoneNumberCountryCode' "BM"

pattern PNCCBN :: PhoneNumberCountryCode
pattern PNCCBN = PhoneNumberCountryCode' "BN"

pattern PNCCBO :: PhoneNumberCountryCode
pattern PNCCBO = PhoneNumberCountryCode' "BO"

pattern PNCCBR :: PhoneNumberCountryCode
pattern PNCCBR = PhoneNumberCountryCode' "BR"

pattern PNCCBS :: PhoneNumberCountryCode
pattern PNCCBS = PhoneNumberCountryCode' "BS"

pattern PNCCBT :: PhoneNumberCountryCode
pattern PNCCBT = PhoneNumberCountryCode' "BT"

pattern PNCCBW :: PhoneNumberCountryCode
pattern PNCCBW = PhoneNumberCountryCode' "BW"

pattern PNCCBY :: PhoneNumberCountryCode
pattern PNCCBY = PhoneNumberCountryCode' "BY"

pattern PNCCBZ :: PhoneNumberCountryCode
pattern PNCCBZ = PhoneNumberCountryCode' "BZ"

pattern PNCCCA :: PhoneNumberCountryCode
pattern PNCCCA = PhoneNumberCountryCode' "CA"

pattern PNCCCC :: PhoneNumberCountryCode
pattern PNCCCC = PhoneNumberCountryCode' "CC"

pattern PNCCCD :: PhoneNumberCountryCode
pattern PNCCCD = PhoneNumberCountryCode' "CD"

pattern PNCCCF :: PhoneNumberCountryCode
pattern PNCCCF = PhoneNumberCountryCode' "CF"

pattern PNCCCG :: PhoneNumberCountryCode
pattern PNCCCG = PhoneNumberCountryCode' "CG"

pattern PNCCCH :: PhoneNumberCountryCode
pattern PNCCCH = PhoneNumberCountryCode' "CH"

pattern PNCCCI :: PhoneNumberCountryCode
pattern PNCCCI = PhoneNumberCountryCode' "CI"

pattern PNCCCK :: PhoneNumberCountryCode
pattern PNCCCK = PhoneNumberCountryCode' "CK"

pattern PNCCCL :: PhoneNumberCountryCode
pattern PNCCCL = PhoneNumberCountryCode' "CL"

pattern PNCCCM :: PhoneNumberCountryCode
pattern PNCCCM = PhoneNumberCountryCode' "CM"

pattern PNCCCN :: PhoneNumberCountryCode
pattern PNCCCN = PhoneNumberCountryCode' "CN"

pattern PNCCCO :: PhoneNumberCountryCode
pattern PNCCCO = PhoneNumberCountryCode' "CO"

pattern PNCCCR :: PhoneNumberCountryCode
pattern PNCCCR = PhoneNumberCountryCode' "CR"

pattern PNCCCU :: PhoneNumberCountryCode
pattern PNCCCU = PhoneNumberCountryCode' "CU"

pattern PNCCCV :: PhoneNumberCountryCode
pattern PNCCCV = PhoneNumberCountryCode' "CV"

pattern PNCCCW :: PhoneNumberCountryCode
pattern PNCCCW = PhoneNumberCountryCode' "CW"

pattern PNCCCX :: PhoneNumberCountryCode
pattern PNCCCX = PhoneNumberCountryCode' "CX"

pattern PNCCCY :: PhoneNumberCountryCode
pattern PNCCCY = PhoneNumberCountryCode' "CY"

pattern PNCCCZ :: PhoneNumberCountryCode
pattern PNCCCZ = PhoneNumberCountryCode' "CZ"

pattern PNCCDE :: PhoneNumberCountryCode
pattern PNCCDE = PhoneNumberCountryCode' "DE"

pattern PNCCDJ :: PhoneNumberCountryCode
pattern PNCCDJ = PhoneNumberCountryCode' "DJ"

pattern PNCCDK :: PhoneNumberCountryCode
pattern PNCCDK = PhoneNumberCountryCode' "DK"

pattern PNCCDM :: PhoneNumberCountryCode
pattern PNCCDM = PhoneNumberCountryCode' "DM"

pattern PNCCDO :: PhoneNumberCountryCode
pattern PNCCDO = PhoneNumberCountryCode' "DO"

pattern PNCCDZ :: PhoneNumberCountryCode
pattern PNCCDZ = PhoneNumberCountryCode' "DZ"

pattern PNCCEC :: PhoneNumberCountryCode
pattern PNCCEC = PhoneNumberCountryCode' "EC"

pattern PNCCEE :: PhoneNumberCountryCode
pattern PNCCEE = PhoneNumberCountryCode' "EE"

pattern PNCCEG :: PhoneNumberCountryCode
pattern PNCCEG = PhoneNumberCountryCode' "EG"

pattern PNCCEH :: PhoneNumberCountryCode
pattern PNCCEH = PhoneNumberCountryCode' "EH"

pattern PNCCER :: PhoneNumberCountryCode
pattern PNCCER = PhoneNumberCountryCode' "ER"

pattern PNCCES :: PhoneNumberCountryCode
pattern PNCCES = PhoneNumberCountryCode' "ES"

pattern PNCCET :: PhoneNumberCountryCode
pattern PNCCET = PhoneNumberCountryCode' "ET"

pattern PNCCFI :: PhoneNumberCountryCode
pattern PNCCFI = PhoneNumberCountryCode' "FI"

pattern PNCCFJ :: PhoneNumberCountryCode
pattern PNCCFJ = PhoneNumberCountryCode' "FJ"

pattern PNCCFK :: PhoneNumberCountryCode
pattern PNCCFK = PhoneNumberCountryCode' "FK"

pattern PNCCFM :: PhoneNumberCountryCode
pattern PNCCFM = PhoneNumberCountryCode' "FM"

pattern PNCCFO :: PhoneNumberCountryCode
pattern PNCCFO = PhoneNumberCountryCode' "FO"

pattern PNCCFR :: PhoneNumberCountryCode
pattern PNCCFR = PhoneNumberCountryCode' "FR"

pattern PNCCGA :: PhoneNumberCountryCode
pattern PNCCGA = PhoneNumberCountryCode' "GA"

pattern PNCCGB :: PhoneNumberCountryCode
pattern PNCCGB = PhoneNumberCountryCode' "GB"

pattern PNCCGD :: PhoneNumberCountryCode
pattern PNCCGD = PhoneNumberCountryCode' "GD"

pattern PNCCGE :: PhoneNumberCountryCode
pattern PNCCGE = PhoneNumberCountryCode' "GE"

pattern PNCCGG :: PhoneNumberCountryCode
pattern PNCCGG = PhoneNumberCountryCode' "GG"

pattern PNCCGH :: PhoneNumberCountryCode
pattern PNCCGH = PhoneNumberCountryCode' "GH"

pattern PNCCGI :: PhoneNumberCountryCode
pattern PNCCGI = PhoneNumberCountryCode' "GI"

pattern PNCCGL :: PhoneNumberCountryCode
pattern PNCCGL = PhoneNumberCountryCode' "GL"

pattern PNCCGM :: PhoneNumberCountryCode
pattern PNCCGM = PhoneNumberCountryCode' "GM"

pattern PNCCGN :: PhoneNumberCountryCode
pattern PNCCGN = PhoneNumberCountryCode' "GN"

pattern PNCCGQ :: PhoneNumberCountryCode
pattern PNCCGQ = PhoneNumberCountryCode' "GQ"

pattern PNCCGR :: PhoneNumberCountryCode
pattern PNCCGR = PhoneNumberCountryCode' "GR"

pattern PNCCGT' :: PhoneNumberCountryCode
pattern PNCCGT' = PhoneNumberCountryCode' "GT"

pattern PNCCGU :: PhoneNumberCountryCode
pattern PNCCGU = PhoneNumberCountryCode' "GU"

pattern PNCCGW :: PhoneNumberCountryCode
pattern PNCCGW = PhoneNumberCountryCode' "GW"

pattern PNCCGY :: PhoneNumberCountryCode
pattern PNCCGY = PhoneNumberCountryCode' "GY"

pattern PNCCHK :: PhoneNumberCountryCode
pattern PNCCHK = PhoneNumberCountryCode' "HK"

pattern PNCCHN :: PhoneNumberCountryCode
pattern PNCCHN = PhoneNumberCountryCode' "HN"

pattern PNCCHR :: PhoneNumberCountryCode
pattern PNCCHR = PhoneNumberCountryCode' "HR"

pattern PNCCHT :: PhoneNumberCountryCode
pattern PNCCHT = PhoneNumberCountryCode' "HT"

pattern PNCCHU :: PhoneNumberCountryCode
pattern PNCCHU = PhoneNumberCountryCode' "HU"

pattern PNCCIE :: PhoneNumberCountryCode
pattern PNCCIE = PhoneNumberCountryCode' "IE"

pattern PNCCIL :: PhoneNumberCountryCode
pattern PNCCIL = PhoneNumberCountryCode' "IL"

pattern PNCCIM :: PhoneNumberCountryCode
pattern PNCCIM = PhoneNumberCountryCode' "IM"

pattern PNCCIN :: PhoneNumberCountryCode
pattern PNCCIN = PhoneNumberCountryCode' "IN"

pattern PNCCIO :: PhoneNumberCountryCode
pattern PNCCIO = PhoneNumberCountryCode' "IO"

pattern PNCCIQ :: PhoneNumberCountryCode
pattern PNCCIQ = PhoneNumberCountryCode' "IQ"

pattern PNCCIR :: PhoneNumberCountryCode
pattern PNCCIR = PhoneNumberCountryCode' "IR"

pattern PNCCIS :: PhoneNumberCountryCode
pattern PNCCIS = PhoneNumberCountryCode' "IS"

pattern PNCCIT :: PhoneNumberCountryCode
pattern PNCCIT = PhoneNumberCountryCode' "IT"

pattern PNCCId :: PhoneNumberCountryCode
pattern PNCCId = PhoneNumberCountryCode' "ID"

pattern PNCCJE :: PhoneNumberCountryCode
pattern PNCCJE = PhoneNumberCountryCode' "JE"

pattern PNCCJM :: PhoneNumberCountryCode
pattern PNCCJM = PhoneNumberCountryCode' "JM"

pattern PNCCJO :: PhoneNumberCountryCode
pattern PNCCJO = PhoneNumberCountryCode' "JO"

pattern PNCCJP :: PhoneNumberCountryCode
pattern PNCCJP = PhoneNumberCountryCode' "JP"

pattern PNCCKE :: PhoneNumberCountryCode
pattern PNCCKE = PhoneNumberCountryCode' "KE"

pattern PNCCKG :: PhoneNumberCountryCode
pattern PNCCKG = PhoneNumberCountryCode' "KG"

pattern PNCCKH :: PhoneNumberCountryCode
pattern PNCCKH = PhoneNumberCountryCode' "KH"

pattern PNCCKI :: PhoneNumberCountryCode
pattern PNCCKI = PhoneNumberCountryCode' "KI"

pattern PNCCKM :: PhoneNumberCountryCode
pattern PNCCKM = PhoneNumberCountryCode' "KM"

pattern PNCCKN :: PhoneNumberCountryCode
pattern PNCCKN = PhoneNumberCountryCode' "KN"

pattern PNCCKP :: PhoneNumberCountryCode
pattern PNCCKP = PhoneNumberCountryCode' "KP"

pattern PNCCKR :: PhoneNumberCountryCode
pattern PNCCKR = PhoneNumberCountryCode' "KR"

pattern PNCCKW :: PhoneNumberCountryCode
pattern PNCCKW = PhoneNumberCountryCode' "KW"

pattern PNCCKY :: PhoneNumberCountryCode
pattern PNCCKY = PhoneNumberCountryCode' "KY"

pattern PNCCKZ :: PhoneNumberCountryCode
pattern PNCCKZ = PhoneNumberCountryCode' "KZ"

pattern PNCCLA :: PhoneNumberCountryCode
pattern PNCCLA = PhoneNumberCountryCode' "LA"

pattern PNCCLB :: PhoneNumberCountryCode
pattern PNCCLB = PhoneNumberCountryCode' "LB"

pattern PNCCLC :: PhoneNumberCountryCode
pattern PNCCLC = PhoneNumberCountryCode' "LC"

pattern PNCCLI :: PhoneNumberCountryCode
pattern PNCCLI = PhoneNumberCountryCode' "LI"

pattern PNCCLK :: PhoneNumberCountryCode
pattern PNCCLK = PhoneNumberCountryCode' "LK"

pattern PNCCLR :: PhoneNumberCountryCode
pattern PNCCLR = PhoneNumberCountryCode' "LR"

pattern PNCCLS :: PhoneNumberCountryCode
pattern PNCCLS = PhoneNumberCountryCode' "LS"

pattern PNCCLT' :: PhoneNumberCountryCode
pattern PNCCLT' = PhoneNumberCountryCode' "LT"

pattern PNCCLU :: PhoneNumberCountryCode
pattern PNCCLU = PhoneNumberCountryCode' "LU"

pattern PNCCLV :: PhoneNumberCountryCode
pattern PNCCLV = PhoneNumberCountryCode' "LV"

pattern PNCCLY :: PhoneNumberCountryCode
pattern PNCCLY = PhoneNumberCountryCode' "LY"

pattern PNCCMA :: PhoneNumberCountryCode
pattern PNCCMA = PhoneNumberCountryCode' "MA"

pattern PNCCMC :: PhoneNumberCountryCode
pattern PNCCMC = PhoneNumberCountryCode' "MC"

pattern PNCCMD :: PhoneNumberCountryCode
pattern PNCCMD = PhoneNumberCountryCode' "MD"

pattern PNCCME :: PhoneNumberCountryCode
pattern PNCCME = PhoneNumberCountryCode' "ME"

pattern PNCCMF :: PhoneNumberCountryCode
pattern PNCCMF = PhoneNumberCountryCode' "MF"

pattern PNCCMG :: PhoneNumberCountryCode
pattern PNCCMG = PhoneNumberCountryCode' "MG"

pattern PNCCMH :: PhoneNumberCountryCode
pattern PNCCMH = PhoneNumberCountryCode' "MH"

pattern PNCCMK :: PhoneNumberCountryCode
pattern PNCCMK = PhoneNumberCountryCode' "MK"

pattern PNCCML :: PhoneNumberCountryCode
pattern PNCCML = PhoneNumberCountryCode' "ML"

pattern PNCCMM :: PhoneNumberCountryCode
pattern PNCCMM = PhoneNumberCountryCode' "MM"

pattern PNCCMN :: PhoneNumberCountryCode
pattern PNCCMN = PhoneNumberCountryCode' "MN"

pattern PNCCMO :: PhoneNumberCountryCode
pattern PNCCMO = PhoneNumberCountryCode' "MO"

pattern PNCCMP :: PhoneNumberCountryCode
pattern PNCCMP = PhoneNumberCountryCode' "MP"

pattern PNCCMR :: PhoneNumberCountryCode
pattern PNCCMR = PhoneNumberCountryCode' "MR"

pattern PNCCMS :: PhoneNumberCountryCode
pattern PNCCMS = PhoneNumberCountryCode' "MS"

pattern PNCCMT :: PhoneNumberCountryCode
pattern PNCCMT = PhoneNumberCountryCode' "MT"

pattern PNCCMU :: PhoneNumberCountryCode
pattern PNCCMU = PhoneNumberCountryCode' "MU"

pattern PNCCMV :: PhoneNumberCountryCode
pattern PNCCMV = PhoneNumberCountryCode' "MV"

pattern PNCCMW :: PhoneNumberCountryCode
pattern PNCCMW = PhoneNumberCountryCode' "MW"

pattern PNCCMX :: PhoneNumberCountryCode
pattern PNCCMX = PhoneNumberCountryCode' "MX"

pattern PNCCMY :: PhoneNumberCountryCode
pattern PNCCMY = PhoneNumberCountryCode' "MY"

pattern PNCCMZ :: PhoneNumberCountryCode
pattern PNCCMZ = PhoneNumberCountryCode' "MZ"

pattern PNCCNA :: PhoneNumberCountryCode
pattern PNCCNA = PhoneNumberCountryCode' "NA"

pattern PNCCNC :: PhoneNumberCountryCode
pattern PNCCNC = PhoneNumberCountryCode' "NC"

pattern PNCCNE :: PhoneNumberCountryCode
pattern PNCCNE = PhoneNumberCountryCode' "NE"

pattern PNCCNG :: PhoneNumberCountryCode
pattern PNCCNG = PhoneNumberCountryCode' "NG"

pattern PNCCNI :: PhoneNumberCountryCode
pattern PNCCNI = PhoneNumberCountryCode' "NI"

pattern PNCCNL :: PhoneNumberCountryCode
pattern PNCCNL = PhoneNumberCountryCode' "NL"

pattern PNCCNO :: PhoneNumberCountryCode
pattern PNCCNO = PhoneNumberCountryCode' "NO"

pattern PNCCNP :: PhoneNumberCountryCode
pattern PNCCNP = PhoneNumberCountryCode' "NP"

pattern PNCCNR :: PhoneNumberCountryCode
pattern PNCCNR = PhoneNumberCountryCode' "NR"

pattern PNCCNU :: PhoneNumberCountryCode
pattern PNCCNU = PhoneNumberCountryCode' "NU"

pattern PNCCNZ :: PhoneNumberCountryCode
pattern PNCCNZ = PhoneNumberCountryCode' "NZ"

pattern PNCCOM :: PhoneNumberCountryCode
pattern PNCCOM = PhoneNumberCountryCode' "OM"

pattern PNCCPA :: PhoneNumberCountryCode
pattern PNCCPA = PhoneNumberCountryCode' "PA"

pattern PNCCPE :: PhoneNumberCountryCode
pattern PNCCPE = PhoneNumberCountryCode' "PE"

pattern PNCCPF :: PhoneNumberCountryCode
pattern PNCCPF = PhoneNumberCountryCode' "PF"

pattern PNCCPG :: PhoneNumberCountryCode
pattern PNCCPG = PhoneNumberCountryCode' "PG"

pattern PNCCPH :: PhoneNumberCountryCode
pattern PNCCPH = PhoneNumberCountryCode' "PH"

pattern PNCCPK :: PhoneNumberCountryCode
pattern PNCCPK = PhoneNumberCountryCode' "PK"

pattern PNCCPL :: PhoneNumberCountryCode
pattern PNCCPL = PhoneNumberCountryCode' "PL"

pattern PNCCPM :: PhoneNumberCountryCode
pattern PNCCPM = PhoneNumberCountryCode' "PM"

pattern PNCCPN :: PhoneNumberCountryCode
pattern PNCCPN = PhoneNumberCountryCode' "PN"

pattern PNCCPR :: PhoneNumberCountryCode
pattern PNCCPR = PhoneNumberCountryCode' "PR"

pattern PNCCPT :: PhoneNumberCountryCode
pattern PNCCPT = PhoneNumberCountryCode' "PT"

pattern PNCCPW :: PhoneNumberCountryCode
pattern PNCCPW = PhoneNumberCountryCode' "PW"

pattern PNCCPY :: PhoneNumberCountryCode
pattern PNCCPY = PhoneNumberCountryCode' "PY"

pattern PNCCQA :: PhoneNumberCountryCode
pattern PNCCQA = PhoneNumberCountryCode' "QA"

pattern PNCCRE :: PhoneNumberCountryCode
pattern PNCCRE = PhoneNumberCountryCode' "RE"

pattern PNCCRO :: PhoneNumberCountryCode
pattern PNCCRO = PhoneNumberCountryCode' "RO"

pattern PNCCRS :: PhoneNumberCountryCode
pattern PNCCRS = PhoneNumberCountryCode' "RS"

pattern PNCCRU :: PhoneNumberCountryCode
pattern PNCCRU = PhoneNumberCountryCode' "RU"

pattern PNCCRW :: PhoneNumberCountryCode
pattern PNCCRW = PhoneNumberCountryCode' "RW"

pattern PNCCSA :: PhoneNumberCountryCode
pattern PNCCSA = PhoneNumberCountryCode' "SA"

pattern PNCCSB :: PhoneNumberCountryCode
pattern PNCCSB = PhoneNumberCountryCode' "SB"

pattern PNCCSC :: PhoneNumberCountryCode
pattern PNCCSC = PhoneNumberCountryCode' "SC"

pattern PNCCSD :: PhoneNumberCountryCode
pattern PNCCSD = PhoneNumberCountryCode' "SD"

pattern PNCCSE :: PhoneNumberCountryCode
pattern PNCCSE = PhoneNumberCountryCode' "SE"

pattern PNCCSG :: PhoneNumberCountryCode
pattern PNCCSG = PhoneNumberCountryCode' "SG"

pattern PNCCSH :: PhoneNumberCountryCode
pattern PNCCSH = PhoneNumberCountryCode' "SH"

pattern PNCCSI :: PhoneNumberCountryCode
pattern PNCCSI = PhoneNumberCountryCode' "SI"

pattern PNCCSJ :: PhoneNumberCountryCode
pattern PNCCSJ = PhoneNumberCountryCode' "SJ"

pattern PNCCSK :: PhoneNumberCountryCode
pattern PNCCSK = PhoneNumberCountryCode' "SK"

pattern PNCCSL :: PhoneNumberCountryCode
pattern PNCCSL = PhoneNumberCountryCode' "SL"

pattern PNCCSM :: PhoneNumberCountryCode
pattern PNCCSM = PhoneNumberCountryCode' "SM"

pattern PNCCSN :: PhoneNumberCountryCode
pattern PNCCSN = PhoneNumberCountryCode' "SN"

pattern PNCCSO :: PhoneNumberCountryCode
pattern PNCCSO = PhoneNumberCountryCode' "SO"

pattern PNCCSR :: PhoneNumberCountryCode
pattern PNCCSR = PhoneNumberCountryCode' "SR"

pattern PNCCST :: PhoneNumberCountryCode
pattern PNCCST = PhoneNumberCountryCode' "ST"

pattern PNCCSV :: PhoneNumberCountryCode
pattern PNCCSV = PhoneNumberCountryCode' "SV"

pattern PNCCSX :: PhoneNumberCountryCode
pattern PNCCSX = PhoneNumberCountryCode' "SX"

pattern PNCCSY :: PhoneNumberCountryCode
pattern PNCCSY = PhoneNumberCountryCode' "SY"

pattern PNCCSZ :: PhoneNumberCountryCode
pattern PNCCSZ = PhoneNumberCountryCode' "SZ"

pattern PNCCTC :: PhoneNumberCountryCode
pattern PNCCTC = PhoneNumberCountryCode' "TC"

pattern PNCCTD :: PhoneNumberCountryCode
pattern PNCCTD = PhoneNumberCountryCode' "TD"

pattern PNCCTG :: PhoneNumberCountryCode
pattern PNCCTG = PhoneNumberCountryCode' "TG"

pattern PNCCTH :: PhoneNumberCountryCode
pattern PNCCTH = PhoneNumberCountryCode' "TH"

pattern PNCCTJ :: PhoneNumberCountryCode
pattern PNCCTJ = PhoneNumberCountryCode' "TJ"

pattern PNCCTK :: PhoneNumberCountryCode
pattern PNCCTK = PhoneNumberCountryCode' "TK"

pattern PNCCTL :: PhoneNumberCountryCode
pattern PNCCTL = PhoneNumberCountryCode' "TL"

pattern PNCCTM :: PhoneNumberCountryCode
pattern PNCCTM = PhoneNumberCountryCode' "TM"

pattern PNCCTN :: PhoneNumberCountryCode
pattern PNCCTN = PhoneNumberCountryCode' "TN"

pattern PNCCTO :: PhoneNumberCountryCode
pattern PNCCTO = PhoneNumberCountryCode' "TO"

pattern PNCCTR :: PhoneNumberCountryCode
pattern PNCCTR = PhoneNumberCountryCode' "TR"

pattern PNCCTT :: PhoneNumberCountryCode
pattern PNCCTT = PhoneNumberCountryCode' "TT"

pattern PNCCTV :: PhoneNumberCountryCode
pattern PNCCTV = PhoneNumberCountryCode' "TV"

pattern PNCCTW :: PhoneNumberCountryCode
pattern PNCCTW = PhoneNumberCountryCode' "TW"

pattern PNCCTZ :: PhoneNumberCountryCode
pattern PNCCTZ = PhoneNumberCountryCode' "TZ"

pattern PNCCUA :: PhoneNumberCountryCode
pattern PNCCUA = PhoneNumberCountryCode' "UA"

pattern PNCCUG :: PhoneNumberCountryCode
pattern PNCCUG = PhoneNumberCountryCode' "UG"

pattern PNCCUS :: PhoneNumberCountryCode
pattern PNCCUS = PhoneNumberCountryCode' "US"

pattern PNCCUY :: PhoneNumberCountryCode
pattern PNCCUY = PhoneNumberCountryCode' "UY"

pattern PNCCUZ :: PhoneNumberCountryCode
pattern PNCCUZ = PhoneNumberCountryCode' "UZ"

pattern PNCCVA :: PhoneNumberCountryCode
pattern PNCCVA = PhoneNumberCountryCode' "VA"

pattern PNCCVC :: PhoneNumberCountryCode
pattern PNCCVC = PhoneNumberCountryCode' "VC"

pattern PNCCVE :: PhoneNumberCountryCode
pattern PNCCVE = PhoneNumberCountryCode' "VE"

pattern PNCCVG :: PhoneNumberCountryCode
pattern PNCCVG = PhoneNumberCountryCode' "VG"

pattern PNCCVI :: PhoneNumberCountryCode
pattern PNCCVI = PhoneNumberCountryCode' "VI"

pattern PNCCVN :: PhoneNumberCountryCode
pattern PNCCVN = PhoneNumberCountryCode' "VN"

pattern PNCCVU :: PhoneNumberCountryCode
pattern PNCCVU = PhoneNumberCountryCode' "VU"

pattern PNCCWF :: PhoneNumberCountryCode
pattern PNCCWF = PhoneNumberCountryCode' "WF"

pattern PNCCWS :: PhoneNumberCountryCode
pattern PNCCWS = PhoneNumberCountryCode' "WS"

pattern PNCCYE :: PhoneNumberCountryCode
pattern PNCCYE = PhoneNumberCountryCode' "YE"

pattern PNCCYT :: PhoneNumberCountryCode
pattern PNCCYT = PhoneNumberCountryCode' "YT"

pattern PNCCZA :: PhoneNumberCountryCode
pattern PNCCZA = PhoneNumberCountryCode' "ZA"

pattern PNCCZM :: PhoneNumberCountryCode
pattern PNCCZM = PhoneNumberCountryCode' "ZM"

pattern PNCCZW :: PhoneNumberCountryCode
pattern PNCCZW = PhoneNumberCountryCode' "ZW"

{-# COMPLETE
  PNCCAD,
  PNCCAE,
  PNCCAF,
  PNCCAG,
  PNCCAI,
  PNCCAL,
  PNCCAM,
  PNCCAN,
  PNCCAO,
  PNCCAQ,
  PNCCAR,
  PNCCAS,
  PNCCAT,
  PNCCAU,
  PNCCAW,
  PNCCAZ,
  PNCCBA,
  PNCCBB,
  PNCCBD,
  PNCCBE,
  PNCCBF,
  PNCCBG,
  PNCCBH,
  PNCCBI,
  PNCCBJ,
  PNCCBL,
  PNCCBM,
  PNCCBN,
  PNCCBO,
  PNCCBR,
  PNCCBS,
  PNCCBT,
  PNCCBW,
  PNCCBY,
  PNCCBZ,
  PNCCCA,
  PNCCCC,
  PNCCCD,
  PNCCCF,
  PNCCCG,
  PNCCCH,
  PNCCCI,
  PNCCCK,
  PNCCCL,
  PNCCCM,
  PNCCCN,
  PNCCCO,
  PNCCCR,
  PNCCCU,
  PNCCCV,
  PNCCCW,
  PNCCCX,
  PNCCCY,
  PNCCCZ,
  PNCCDE,
  PNCCDJ,
  PNCCDK,
  PNCCDM,
  PNCCDO,
  PNCCDZ,
  PNCCEC,
  PNCCEE,
  PNCCEG,
  PNCCEH,
  PNCCER,
  PNCCES,
  PNCCET,
  PNCCFI,
  PNCCFJ,
  PNCCFK,
  PNCCFM,
  PNCCFO,
  PNCCFR,
  PNCCGA,
  PNCCGB,
  PNCCGD,
  PNCCGE,
  PNCCGG,
  PNCCGH,
  PNCCGI,
  PNCCGL,
  PNCCGM,
  PNCCGN,
  PNCCGQ,
  PNCCGR,
  PNCCGT',
  PNCCGU,
  PNCCGW,
  PNCCGY,
  PNCCHK,
  PNCCHN,
  PNCCHR,
  PNCCHT,
  PNCCHU,
  PNCCIE,
  PNCCIL,
  PNCCIM,
  PNCCIN,
  PNCCIO,
  PNCCIQ,
  PNCCIR,
  PNCCIS,
  PNCCIT,
  PNCCId,
  PNCCJE,
  PNCCJM,
  PNCCJO,
  PNCCJP,
  PNCCKE,
  PNCCKG,
  PNCCKH,
  PNCCKI,
  PNCCKM,
  PNCCKN,
  PNCCKP,
  PNCCKR,
  PNCCKW,
  PNCCKY,
  PNCCKZ,
  PNCCLA,
  PNCCLB,
  PNCCLC,
  PNCCLI,
  PNCCLK,
  PNCCLR,
  PNCCLS,
  PNCCLT',
  PNCCLU,
  PNCCLV,
  PNCCLY,
  PNCCMA,
  PNCCMC,
  PNCCMD,
  PNCCME,
  PNCCMF,
  PNCCMG,
  PNCCMH,
  PNCCMK,
  PNCCML,
  PNCCMM,
  PNCCMN,
  PNCCMO,
  PNCCMP,
  PNCCMR,
  PNCCMS,
  PNCCMT,
  PNCCMU,
  PNCCMV,
  PNCCMW,
  PNCCMX,
  PNCCMY,
  PNCCMZ,
  PNCCNA,
  PNCCNC,
  PNCCNE,
  PNCCNG,
  PNCCNI,
  PNCCNL,
  PNCCNO,
  PNCCNP,
  PNCCNR,
  PNCCNU,
  PNCCNZ,
  PNCCOM,
  PNCCPA,
  PNCCPE,
  PNCCPF,
  PNCCPG,
  PNCCPH,
  PNCCPK,
  PNCCPL,
  PNCCPM,
  PNCCPN,
  PNCCPR,
  PNCCPT,
  PNCCPW,
  PNCCPY,
  PNCCQA,
  PNCCRE,
  PNCCRO,
  PNCCRS,
  PNCCRU,
  PNCCRW,
  PNCCSA,
  PNCCSB,
  PNCCSC,
  PNCCSD,
  PNCCSE,
  PNCCSG,
  PNCCSH,
  PNCCSI,
  PNCCSJ,
  PNCCSK,
  PNCCSL,
  PNCCSM,
  PNCCSN,
  PNCCSO,
  PNCCSR,
  PNCCST,
  PNCCSV,
  PNCCSX,
  PNCCSY,
  PNCCSZ,
  PNCCTC,
  PNCCTD,
  PNCCTG,
  PNCCTH,
  PNCCTJ,
  PNCCTK,
  PNCCTL,
  PNCCTM,
  PNCCTN,
  PNCCTO,
  PNCCTR,
  PNCCTT,
  PNCCTV,
  PNCCTW,
  PNCCTZ,
  PNCCUA,
  PNCCUG,
  PNCCUS,
  PNCCUY,
  PNCCUZ,
  PNCCVA,
  PNCCVC,
  PNCCVE,
  PNCCVG,
  PNCCVI,
  PNCCVN,
  PNCCVU,
  PNCCWF,
  PNCCWS,
  PNCCYE,
  PNCCYT,
  PNCCZA,
  PNCCZM,
  PNCCZW,
  PhoneNumberCountryCode' #-}

instance FromText PhoneNumberCountryCode where
    parser = (PhoneNumberCountryCode' . mk) <$> takeText

instance ToText PhoneNumberCountryCode where
    toText (PhoneNumberCountryCode' ci) = original ci

-- | Represents an enum of /known/ $PhoneNumberCountryCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PhoneNumberCountryCode where
    toEnum i = case i of
        0 -> PNCCAD
        1 -> PNCCAE
        2 -> PNCCAF
        3 -> PNCCAG
        4 -> PNCCAI
        5 -> PNCCAL
        6 -> PNCCAM
        7 -> PNCCAN
        8 -> PNCCAO
        9 -> PNCCAQ
        10 -> PNCCAR
        11 -> PNCCAS
        12 -> PNCCAT
        13 -> PNCCAU
        14 -> PNCCAW
        15 -> PNCCAZ
        16 -> PNCCBA
        17 -> PNCCBB
        18 -> PNCCBD
        19 -> PNCCBE
        20 -> PNCCBF
        21 -> PNCCBG
        22 -> PNCCBH
        23 -> PNCCBI
        24 -> PNCCBJ
        25 -> PNCCBL
        26 -> PNCCBM
        27 -> PNCCBN
        28 -> PNCCBO
        29 -> PNCCBR
        30 -> PNCCBS
        31 -> PNCCBT
        32 -> PNCCBW
        33 -> PNCCBY
        34 -> PNCCBZ
        35 -> PNCCCA
        36 -> PNCCCC
        37 -> PNCCCD
        38 -> PNCCCF
        39 -> PNCCCG
        40 -> PNCCCH
        41 -> PNCCCI
        42 -> PNCCCK
        43 -> PNCCCL
        44 -> PNCCCM
        45 -> PNCCCN
        46 -> PNCCCO
        47 -> PNCCCR
        48 -> PNCCCU
        49 -> PNCCCV
        50 -> PNCCCW
        51 -> PNCCCX
        52 -> PNCCCY
        53 -> PNCCCZ
        54 -> PNCCDE
        55 -> PNCCDJ
        56 -> PNCCDK
        57 -> PNCCDM
        58 -> PNCCDO
        59 -> PNCCDZ
        60 -> PNCCEC
        61 -> PNCCEE
        62 -> PNCCEG
        63 -> PNCCEH
        64 -> PNCCER
        65 -> PNCCES
        66 -> PNCCET
        67 -> PNCCFI
        68 -> PNCCFJ
        69 -> PNCCFK
        70 -> PNCCFM
        71 -> PNCCFO
        72 -> PNCCFR
        73 -> PNCCGA
        74 -> PNCCGB
        75 -> PNCCGD
        76 -> PNCCGE
        77 -> PNCCGG
        78 -> PNCCGH
        79 -> PNCCGI
        80 -> PNCCGL
        81 -> PNCCGM
        82 -> PNCCGN
        83 -> PNCCGQ
        84 -> PNCCGR
        85 -> PNCCGT'
        86 -> PNCCGU
        87 -> PNCCGW
        88 -> PNCCGY
        89 -> PNCCHK
        90 -> PNCCHN
        91 -> PNCCHR
        92 -> PNCCHT
        93 -> PNCCHU
        94 -> PNCCIE
        95 -> PNCCIL
        96 -> PNCCIM
        97 -> PNCCIN
        98 -> PNCCIO
        99 -> PNCCIQ
        100 -> PNCCIR
        101 -> PNCCIS
        102 -> PNCCIT
        103 -> PNCCId
        104 -> PNCCJE
        105 -> PNCCJM
        106 -> PNCCJO
        107 -> PNCCJP
        108 -> PNCCKE
        109 -> PNCCKG
        110 -> PNCCKH
        111 -> PNCCKI
        112 -> PNCCKM
        113 -> PNCCKN
        114 -> PNCCKP
        115 -> PNCCKR
        116 -> PNCCKW
        117 -> PNCCKY
        118 -> PNCCKZ
        119 -> PNCCLA
        120 -> PNCCLB
        121 -> PNCCLC
        122 -> PNCCLI
        123 -> PNCCLK
        124 -> PNCCLR
        125 -> PNCCLS
        126 -> PNCCLT'
        127 -> PNCCLU
        128 -> PNCCLV
        129 -> PNCCLY
        130 -> PNCCMA
        131 -> PNCCMC
        132 -> PNCCMD
        133 -> PNCCME
        134 -> PNCCMF
        135 -> PNCCMG
        136 -> PNCCMH
        137 -> PNCCMK
        138 -> PNCCML
        139 -> PNCCMM
        140 -> PNCCMN
        141 -> PNCCMO
        142 -> PNCCMP
        143 -> PNCCMR
        144 -> PNCCMS
        145 -> PNCCMT
        146 -> PNCCMU
        147 -> PNCCMV
        148 -> PNCCMW
        149 -> PNCCMX
        150 -> PNCCMY
        151 -> PNCCMZ
        152 -> PNCCNA
        153 -> PNCCNC
        154 -> PNCCNE
        155 -> PNCCNG
        156 -> PNCCNI
        157 -> PNCCNL
        158 -> PNCCNO
        159 -> PNCCNP
        160 -> PNCCNR
        161 -> PNCCNU
        162 -> PNCCNZ
        163 -> PNCCOM
        164 -> PNCCPA
        165 -> PNCCPE
        166 -> PNCCPF
        167 -> PNCCPG
        168 -> PNCCPH
        169 -> PNCCPK
        170 -> PNCCPL
        171 -> PNCCPM
        172 -> PNCCPN
        173 -> PNCCPR
        174 -> PNCCPT
        175 -> PNCCPW
        176 -> PNCCPY
        177 -> PNCCQA
        178 -> PNCCRE
        179 -> PNCCRO
        180 -> PNCCRS
        181 -> PNCCRU
        182 -> PNCCRW
        183 -> PNCCSA
        184 -> PNCCSB
        185 -> PNCCSC
        186 -> PNCCSD
        187 -> PNCCSE
        188 -> PNCCSG
        189 -> PNCCSH
        190 -> PNCCSI
        191 -> PNCCSJ
        192 -> PNCCSK
        193 -> PNCCSL
        194 -> PNCCSM
        195 -> PNCCSN
        196 -> PNCCSO
        197 -> PNCCSR
        198 -> PNCCST
        199 -> PNCCSV
        200 -> PNCCSX
        201 -> PNCCSY
        202 -> PNCCSZ
        203 -> PNCCTC
        204 -> PNCCTD
        205 -> PNCCTG
        206 -> PNCCTH
        207 -> PNCCTJ
        208 -> PNCCTK
        209 -> PNCCTL
        210 -> PNCCTM
        211 -> PNCCTN
        212 -> PNCCTO
        213 -> PNCCTR
        214 -> PNCCTT
        215 -> PNCCTV
        216 -> PNCCTW
        217 -> PNCCTZ
        218 -> PNCCUA
        219 -> PNCCUG
        220 -> PNCCUS
        221 -> PNCCUY
        222 -> PNCCUZ
        223 -> PNCCVA
        224 -> PNCCVC
        225 -> PNCCVE
        226 -> PNCCVG
        227 -> PNCCVI
        228 -> PNCCVN
        229 -> PNCCVU
        230 -> PNCCWF
        231 -> PNCCWS
        232 -> PNCCYE
        233 -> PNCCYT
        234 -> PNCCZA
        235 -> PNCCZM
        236 -> PNCCZW
        _ -> (error . showText) $ "Unknown index for PhoneNumberCountryCode: " <> toText i
    fromEnum x = case x of
        PNCCAD -> 0
        PNCCAE -> 1
        PNCCAF -> 2
        PNCCAG -> 3
        PNCCAI -> 4
        PNCCAL -> 5
        PNCCAM -> 6
        PNCCAN -> 7
        PNCCAO -> 8
        PNCCAQ -> 9
        PNCCAR -> 10
        PNCCAS -> 11
        PNCCAT -> 12
        PNCCAU -> 13
        PNCCAW -> 14
        PNCCAZ -> 15
        PNCCBA -> 16
        PNCCBB -> 17
        PNCCBD -> 18
        PNCCBE -> 19
        PNCCBF -> 20
        PNCCBG -> 21
        PNCCBH -> 22
        PNCCBI -> 23
        PNCCBJ -> 24
        PNCCBL -> 25
        PNCCBM -> 26
        PNCCBN -> 27
        PNCCBO -> 28
        PNCCBR -> 29
        PNCCBS -> 30
        PNCCBT -> 31
        PNCCBW -> 32
        PNCCBY -> 33
        PNCCBZ -> 34
        PNCCCA -> 35
        PNCCCC -> 36
        PNCCCD -> 37
        PNCCCF -> 38
        PNCCCG -> 39
        PNCCCH -> 40
        PNCCCI -> 41
        PNCCCK -> 42
        PNCCCL -> 43
        PNCCCM -> 44
        PNCCCN -> 45
        PNCCCO -> 46
        PNCCCR -> 47
        PNCCCU -> 48
        PNCCCV -> 49
        PNCCCW -> 50
        PNCCCX -> 51
        PNCCCY -> 52
        PNCCCZ -> 53
        PNCCDE -> 54
        PNCCDJ -> 55
        PNCCDK -> 56
        PNCCDM -> 57
        PNCCDO -> 58
        PNCCDZ -> 59
        PNCCEC -> 60
        PNCCEE -> 61
        PNCCEG -> 62
        PNCCEH -> 63
        PNCCER -> 64
        PNCCES -> 65
        PNCCET -> 66
        PNCCFI -> 67
        PNCCFJ -> 68
        PNCCFK -> 69
        PNCCFM -> 70
        PNCCFO -> 71
        PNCCFR -> 72
        PNCCGA -> 73
        PNCCGB -> 74
        PNCCGD -> 75
        PNCCGE -> 76
        PNCCGG -> 77
        PNCCGH -> 78
        PNCCGI -> 79
        PNCCGL -> 80
        PNCCGM -> 81
        PNCCGN -> 82
        PNCCGQ -> 83
        PNCCGR -> 84
        PNCCGT' -> 85
        PNCCGU -> 86
        PNCCGW -> 87
        PNCCGY -> 88
        PNCCHK -> 89
        PNCCHN -> 90
        PNCCHR -> 91
        PNCCHT -> 92
        PNCCHU -> 93
        PNCCIE -> 94
        PNCCIL -> 95
        PNCCIM -> 96
        PNCCIN -> 97
        PNCCIO -> 98
        PNCCIQ -> 99
        PNCCIR -> 100
        PNCCIS -> 101
        PNCCIT -> 102
        PNCCId -> 103
        PNCCJE -> 104
        PNCCJM -> 105
        PNCCJO -> 106
        PNCCJP -> 107
        PNCCKE -> 108
        PNCCKG -> 109
        PNCCKH -> 110
        PNCCKI -> 111
        PNCCKM -> 112
        PNCCKN -> 113
        PNCCKP -> 114
        PNCCKR -> 115
        PNCCKW -> 116
        PNCCKY -> 117
        PNCCKZ -> 118
        PNCCLA -> 119
        PNCCLB -> 120
        PNCCLC -> 121
        PNCCLI -> 122
        PNCCLK -> 123
        PNCCLR -> 124
        PNCCLS -> 125
        PNCCLT' -> 126
        PNCCLU -> 127
        PNCCLV -> 128
        PNCCLY -> 129
        PNCCMA -> 130
        PNCCMC -> 131
        PNCCMD -> 132
        PNCCME -> 133
        PNCCMF -> 134
        PNCCMG -> 135
        PNCCMH -> 136
        PNCCMK -> 137
        PNCCML -> 138
        PNCCMM -> 139
        PNCCMN -> 140
        PNCCMO -> 141
        PNCCMP -> 142
        PNCCMR -> 143
        PNCCMS -> 144
        PNCCMT -> 145
        PNCCMU -> 146
        PNCCMV -> 147
        PNCCMW -> 148
        PNCCMX -> 149
        PNCCMY -> 150
        PNCCMZ -> 151
        PNCCNA -> 152
        PNCCNC -> 153
        PNCCNE -> 154
        PNCCNG -> 155
        PNCCNI -> 156
        PNCCNL -> 157
        PNCCNO -> 158
        PNCCNP -> 159
        PNCCNR -> 160
        PNCCNU -> 161
        PNCCNZ -> 162
        PNCCOM -> 163
        PNCCPA -> 164
        PNCCPE -> 165
        PNCCPF -> 166
        PNCCPG -> 167
        PNCCPH -> 168
        PNCCPK -> 169
        PNCCPL -> 170
        PNCCPM -> 171
        PNCCPN -> 172
        PNCCPR -> 173
        PNCCPT -> 174
        PNCCPW -> 175
        PNCCPY -> 176
        PNCCQA -> 177
        PNCCRE -> 178
        PNCCRO -> 179
        PNCCRS -> 180
        PNCCRU -> 181
        PNCCRW -> 182
        PNCCSA -> 183
        PNCCSB -> 184
        PNCCSC -> 185
        PNCCSD -> 186
        PNCCSE -> 187
        PNCCSG -> 188
        PNCCSH -> 189
        PNCCSI -> 190
        PNCCSJ -> 191
        PNCCSK -> 192
        PNCCSL -> 193
        PNCCSM -> 194
        PNCCSN -> 195
        PNCCSO -> 196
        PNCCSR -> 197
        PNCCST -> 198
        PNCCSV -> 199
        PNCCSX -> 200
        PNCCSY -> 201
        PNCCSZ -> 202
        PNCCTC -> 203
        PNCCTD -> 204
        PNCCTG -> 205
        PNCCTH -> 206
        PNCCTJ -> 207
        PNCCTK -> 208
        PNCCTL -> 209
        PNCCTM -> 210
        PNCCTN -> 211
        PNCCTO -> 212
        PNCCTR -> 213
        PNCCTT -> 214
        PNCCTV -> 215
        PNCCTW -> 216
        PNCCTZ -> 217
        PNCCUA -> 218
        PNCCUG -> 219
        PNCCUS -> 220
        PNCCUY -> 221
        PNCCUZ -> 222
        PNCCVA -> 223
        PNCCVC -> 224
        PNCCVE -> 225
        PNCCVG -> 226
        PNCCVI -> 227
        PNCCVN -> 228
        PNCCVU -> 229
        PNCCWF -> 230
        PNCCWS -> 231
        PNCCYE -> 232
        PNCCYT -> 233
        PNCCZA -> 234
        PNCCZM -> 235
        PNCCZW -> 236
        PhoneNumberCountryCode' name -> (error . showText) $ "Unknown PhoneNumberCountryCode: " <> original name

-- | Represents the bounds of /known/ $PhoneNumberCountryCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PhoneNumberCountryCode where
    minBound = PNCCAD
    maxBound = PNCCZW

instance Hashable     PhoneNumberCountryCode
instance NFData       PhoneNumberCountryCode
instance ToByteString PhoneNumberCountryCode
instance ToQuery      PhoneNumberCountryCode
instance ToHeader     PhoneNumberCountryCode

instance ToJSON PhoneNumberCountryCode where
    toJSON = toJSONText

instance FromJSON PhoneNumberCountryCode where
    parseJSON = parseJSONText "PhoneNumberCountryCode"
