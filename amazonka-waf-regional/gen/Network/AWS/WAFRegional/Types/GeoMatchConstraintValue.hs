{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchConstraintValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.GeoMatchConstraintValue (
  GeoMatchConstraintValue (
    ..
    , GMCVAD
    , GMCVAE
    , GMCVAF
    , GMCVAG
    , GMCVAI
    , GMCVAL
    , GMCVAM
    , GMCVAO
    , GMCVAQ
    , GMCVAR
    , GMCVAS
    , GMCVAT
    , GMCVAU
    , GMCVAW
    , GMCVAX
    , GMCVAZ
    , GMCVBA
    , GMCVBB
    , GMCVBD
    , GMCVBE
    , GMCVBF
    , GMCVBG
    , GMCVBH
    , GMCVBI
    , GMCVBJ
    , GMCVBL
    , GMCVBM
    , GMCVBN
    , GMCVBO
    , GMCVBQ
    , GMCVBR
    , GMCVBS
    , GMCVBT
    , GMCVBV
    , GMCVBW
    , GMCVBY
    , GMCVBZ
    , GMCVCA
    , GMCVCC
    , GMCVCD
    , GMCVCF
    , GMCVCG
    , GMCVCH
    , GMCVCI
    , GMCVCK
    , GMCVCL
    , GMCVCM
    , GMCVCN
    , GMCVCO
    , GMCVCR
    , GMCVCU
    , GMCVCV
    , GMCVCW
    , GMCVCX
    , GMCVCY
    , GMCVCZ
    , GMCVDE
    , GMCVDJ
    , GMCVDK
    , GMCVDM
    , GMCVDO
    , GMCVDZ
    , GMCVEC
    , GMCVEE
    , GMCVEG
    , GMCVEH
    , GMCVER
    , GMCVES
    , GMCVET
    , GMCVFI
    , GMCVFJ
    , GMCVFK
    , GMCVFM
    , GMCVFO
    , GMCVFR
    , GMCVGA
    , GMCVGB
    , GMCVGD
    , GMCVGE
    , GMCVGF
    , GMCVGG
    , GMCVGH
    , GMCVGI
    , GMCVGL
    , GMCVGM
    , GMCVGN
    , GMCVGP
    , GMCVGQ
    , GMCVGR
    , GMCVGS
    , GMCVGT'
    , GMCVGU
    , GMCVGW
    , GMCVGY
    , GMCVHK
    , GMCVHM
    , GMCVHN
    , GMCVHR
    , GMCVHT
    , GMCVHU
    , GMCVIE
    , GMCVIL
    , GMCVIM
    , GMCVIN
    , GMCVIO
    , GMCVIQ
    , GMCVIR
    , GMCVIS
    , GMCVIT
    , GMCVId
    , GMCVJE
    , GMCVJM
    , GMCVJO
    , GMCVJP
    , GMCVKE
    , GMCVKG
    , GMCVKH
    , GMCVKI
    , GMCVKM
    , GMCVKN
    , GMCVKP
    , GMCVKR
    , GMCVKW
    , GMCVKY
    , GMCVKZ
    , GMCVLA
    , GMCVLB
    , GMCVLC
    , GMCVLI
    , GMCVLK
    , GMCVLR
    , GMCVLS
    , GMCVLT'
    , GMCVLU
    , GMCVLV
    , GMCVLY
    , GMCVMA
    , GMCVMC
    , GMCVMD
    , GMCVME
    , GMCVMF
    , GMCVMG
    , GMCVMH
    , GMCVMK
    , GMCVML
    , GMCVMM
    , GMCVMN
    , GMCVMO
    , GMCVMP
    , GMCVMQ
    , GMCVMR
    , GMCVMS
    , GMCVMT
    , GMCVMU
    , GMCVMV
    , GMCVMW
    , GMCVMX
    , GMCVMY
    , GMCVMZ
    , GMCVNA
    , GMCVNC
    , GMCVNE
    , GMCVNF
    , GMCVNG
    , GMCVNI
    , GMCVNL
    , GMCVNO
    , GMCVNP
    , GMCVNR
    , GMCVNU
    , GMCVNZ
    , GMCVOM
    , GMCVPA
    , GMCVPE
    , GMCVPF
    , GMCVPG
    , GMCVPH
    , GMCVPK
    , GMCVPL
    , GMCVPM
    , GMCVPN
    , GMCVPR
    , GMCVPS
    , GMCVPT
    , GMCVPW
    , GMCVPY
    , GMCVQA
    , GMCVRE
    , GMCVRO
    , GMCVRS
    , GMCVRU
    , GMCVRW
    , GMCVSA
    , GMCVSB
    , GMCVSC
    , GMCVSD
    , GMCVSE
    , GMCVSG
    , GMCVSH
    , GMCVSI
    , GMCVSJ
    , GMCVSK
    , GMCVSL
    , GMCVSM
    , GMCVSN
    , GMCVSO
    , GMCVSR
    , GMCVSS
    , GMCVST
    , GMCVSV
    , GMCVSX
    , GMCVSY
    , GMCVSZ
    , GMCVTC
    , GMCVTD
    , GMCVTF
    , GMCVTG
    , GMCVTH
    , GMCVTJ
    , GMCVTK
    , GMCVTL
    , GMCVTM
    , GMCVTN
    , GMCVTO
    , GMCVTR
    , GMCVTT
    , GMCVTV
    , GMCVTW
    , GMCVTZ
    , GMCVUA
    , GMCVUG
    , GMCVUM
    , GMCVUS
    , GMCVUY
    , GMCVUZ
    , GMCVVA
    , GMCVVC
    , GMCVVE
    , GMCVVG
    , GMCVVI
    , GMCVVN
    , GMCVVU
    , GMCVWF
    , GMCVWS
    , GMCVYE
    , GMCVYT
    , GMCVZA
    , GMCVZM
    , GMCVZW
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data GeoMatchConstraintValue = GeoMatchConstraintValue' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern GMCVAD :: GeoMatchConstraintValue
pattern GMCVAD = GeoMatchConstraintValue' "AD"

pattern GMCVAE :: GeoMatchConstraintValue
pattern GMCVAE = GeoMatchConstraintValue' "AE"

pattern GMCVAF :: GeoMatchConstraintValue
pattern GMCVAF = GeoMatchConstraintValue' "AF"

pattern GMCVAG :: GeoMatchConstraintValue
pattern GMCVAG = GeoMatchConstraintValue' "AG"

pattern GMCVAI :: GeoMatchConstraintValue
pattern GMCVAI = GeoMatchConstraintValue' "AI"

pattern GMCVAL :: GeoMatchConstraintValue
pattern GMCVAL = GeoMatchConstraintValue' "AL"

pattern GMCVAM :: GeoMatchConstraintValue
pattern GMCVAM = GeoMatchConstraintValue' "AM"

pattern GMCVAO :: GeoMatchConstraintValue
pattern GMCVAO = GeoMatchConstraintValue' "AO"

pattern GMCVAQ :: GeoMatchConstraintValue
pattern GMCVAQ = GeoMatchConstraintValue' "AQ"

pattern GMCVAR :: GeoMatchConstraintValue
pattern GMCVAR = GeoMatchConstraintValue' "AR"

pattern GMCVAS :: GeoMatchConstraintValue
pattern GMCVAS = GeoMatchConstraintValue' "AS"

pattern GMCVAT :: GeoMatchConstraintValue
pattern GMCVAT = GeoMatchConstraintValue' "AT"

pattern GMCVAU :: GeoMatchConstraintValue
pattern GMCVAU = GeoMatchConstraintValue' "AU"

pattern GMCVAW :: GeoMatchConstraintValue
pattern GMCVAW = GeoMatchConstraintValue' "AW"

pattern GMCVAX :: GeoMatchConstraintValue
pattern GMCVAX = GeoMatchConstraintValue' "AX"

pattern GMCVAZ :: GeoMatchConstraintValue
pattern GMCVAZ = GeoMatchConstraintValue' "AZ"

pattern GMCVBA :: GeoMatchConstraintValue
pattern GMCVBA = GeoMatchConstraintValue' "BA"

pattern GMCVBB :: GeoMatchConstraintValue
pattern GMCVBB = GeoMatchConstraintValue' "BB"

pattern GMCVBD :: GeoMatchConstraintValue
pattern GMCVBD = GeoMatchConstraintValue' "BD"

pattern GMCVBE :: GeoMatchConstraintValue
pattern GMCVBE = GeoMatchConstraintValue' "BE"

pattern GMCVBF :: GeoMatchConstraintValue
pattern GMCVBF = GeoMatchConstraintValue' "BF"

pattern GMCVBG :: GeoMatchConstraintValue
pattern GMCVBG = GeoMatchConstraintValue' "BG"

pattern GMCVBH :: GeoMatchConstraintValue
pattern GMCVBH = GeoMatchConstraintValue' "BH"

pattern GMCVBI :: GeoMatchConstraintValue
pattern GMCVBI = GeoMatchConstraintValue' "BI"

pattern GMCVBJ :: GeoMatchConstraintValue
pattern GMCVBJ = GeoMatchConstraintValue' "BJ"

pattern GMCVBL :: GeoMatchConstraintValue
pattern GMCVBL = GeoMatchConstraintValue' "BL"

pattern GMCVBM :: GeoMatchConstraintValue
pattern GMCVBM = GeoMatchConstraintValue' "BM"

pattern GMCVBN :: GeoMatchConstraintValue
pattern GMCVBN = GeoMatchConstraintValue' "BN"

pattern GMCVBO :: GeoMatchConstraintValue
pattern GMCVBO = GeoMatchConstraintValue' "BO"

pattern GMCVBQ :: GeoMatchConstraintValue
pattern GMCVBQ = GeoMatchConstraintValue' "BQ"

pattern GMCVBR :: GeoMatchConstraintValue
pattern GMCVBR = GeoMatchConstraintValue' "BR"

pattern GMCVBS :: GeoMatchConstraintValue
pattern GMCVBS = GeoMatchConstraintValue' "BS"

pattern GMCVBT :: GeoMatchConstraintValue
pattern GMCVBT = GeoMatchConstraintValue' "BT"

pattern GMCVBV :: GeoMatchConstraintValue
pattern GMCVBV = GeoMatchConstraintValue' "BV"

pattern GMCVBW :: GeoMatchConstraintValue
pattern GMCVBW = GeoMatchConstraintValue' "BW"

pattern GMCVBY :: GeoMatchConstraintValue
pattern GMCVBY = GeoMatchConstraintValue' "BY"

pattern GMCVBZ :: GeoMatchConstraintValue
pattern GMCVBZ = GeoMatchConstraintValue' "BZ"

pattern GMCVCA :: GeoMatchConstraintValue
pattern GMCVCA = GeoMatchConstraintValue' "CA"

pattern GMCVCC :: GeoMatchConstraintValue
pattern GMCVCC = GeoMatchConstraintValue' "CC"

pattern GMCVCD :: GeoMatchConstraintValue
pattern GMCVCD = GeoMatchConstraintValue' "CD"

pattern GMCVCF :: GeoMatchConstraintValue
pattern GMCVCF = GeoMatchConstraintValue' "CF"

pattern GMCVCG :: GeoMatchConstraintValue
pattern GMCVCG = GeoMatchConstraintValue' "CG"

pattern GMCVCH :: GeoMatchConstraintValue
pattern GMCVCH = GeoMatchConstraintValue' "CH"

pattern GMCVCI :: GeoMatchConstraintValue
pattern GMCVCI = GeoMatchConstraintValue' "CI"

pattern GMCVCK :: GeoMatchConstraintValue
pattern GMCVCK = GeoMatchConstraintValue' "CK"

pattern GMCVCL :: GeoMatchConstraintValue
pattern GMCVCL = GeoMatchConstraintValue' "CL"

pattern GMCVCM :: GeoMatchConstraintValue
pattern GMCVCM = GeoMatchConstraintValue' "CM"

pattern GMCVCN :: GeoMatchConstraintValue
pattern GMCVCN = GeoMatchConstraintValue' "CN"

pattern GMCVCO :: GeoMatchConstraintValue
pattern GMCVCO = GeoMatchConstraintValue' "CO"

pattern GMCVCR :: GeoMatchConstraintValue
pattern GMCVCR = GeoMatchConstraintValue' "CR"

pattern GMCVCU :: GeoMatchConstraintValue
pattern GMCVCU = GeoMatchConstraintValue' "CU"

pattern GMCVCV :: GeoMatchConstraintValue
pattern GMCVCV = GeoMatchConstraintValue' "CV"

pattern GMCVCW :: GeoMatchConstraintValue
pattern GMCVCW = GeoMatchConstraintValue' "CW"

pattern GMCVCX :: GeoMatchConstraintValue
pattern GMCVCX = GeoMatchConstraintValue' "CX"

pattern GMCVCY :: GeoMatchConstraintValue
pattern GMCVCY = GeoMatchConstraintValue' "CY"

pattern GMCVCZ :: GeoMatchConstraintValue
pattern GMCVCZ = GeoMatchConstraintValue' "CZ"

pattern GMCVDE :: GeoMatchConstraintValue
pattern GMCVDE = GeoMatchConstraintValue' "DE"

pattern GMCVDJ :: GeoMatchConstraintValue
pattern GMCVDJ = GeoMatchConstraintValue' "DJ"

pattern GMCVDK :: GeoMatchConstraintValue
pattern GMCVDK = GeoMatchConstraintValue' "DK"

pattern GMCVDM :: GeoMatchConstraintValue
pattern GMCVDM = GeoMatchConstraintValue' "DM"

pattern GMCVDO :: GeoMatchConstraintValue
pattern GMCVDO = GeoMatchConstraintValue' "DO"

pattern GMCVDZ :: GeoMatchConstraintValue
pattern GMCVDZ = GeoMatchConstraintValue' "DZ"

pattern GMCVEC :: GeoMatchConstraintValue
pattern GMCVEC = GeoMatchConstraintValue' "EC"

pattern GMCVEE :: GeoMatchConstraintValue
pattern GMCVEE = GeoMatchConstraintValue' "EE"

pattern GMCVEG :: GeoMatchConstraintValue
pattern GMCVEG = GeoMatchConstraintValue' "EG"

pattern GMCVEH :: GeoMatchConstraintValue
pattern GMCVEH = GeoMatchConstraintValue' "EH"

pattern GMCVER :: GeoMatchConstraintValue
pattern GMCVER = GeoMatchConstraintValue' "ER"

pattern GMCVES :: GeoMatchConstraintValue
pattern GMCVES = GeoMatchConstraintValue' "ES"

pattern GMCVET :: GeoMatchConstraintValue
pattern GMCVET = GeoMatchConstraintValue' "ET"

pattern GMCVFI :: GeoMatchConstraintValue
pattern GMCVFI = GeoMatchConstraintValue' "FI"

pattern GMCVFJ :: GeoMatchConstraintValue
pattern GMCVFJ = GeoMatchConstraintValue' "FJ"

pattern GMCVFK :: GeoMatchConstraintValue
pattern GMCVFK = GeoMatchConstraintValue' "FK"

pattern GMCVFM :: GeoMatchConstraintValue
pattern GMCVFM = GeoMatchConstraintValue' "FM"

pattern GMCVFO :: GeoMatchConstraintValue
pattern GMCVFO = GeoMatchConstraintValue' "FO"

pattern GMCVFR :: GeoMatchConstraintValue
pattern GMCVFR = GeoMatchConstraintValue' "FR"

pattern GMCVGA :: GeoMatchConstraintValue
pattern GMCVGA = GeoMatchConstraintValue' "GA"

pattern GMCVGB :: GeoMatchConstraintValue
pattern GMCVGB = GeoMatchConstraintValue' "GB"

pattern GMCVGD :: GeoMatchConstraintValue
pattern GMCVGD = GeoMatchConstraintValue' "GD"

pattern GMCVGE :: GeoMatchConstraintValue
pattern GMCVGE = GeoMatchConstraintValue' "GE"

pattern GMCVGF :: GeoMatchConstraintValue
pattern GMCVGF = GeoMatchConstraintValue' "GF"

pattern GMCVGG :: GeoMatchConstraintValue
pattern GMCVGG = GeoMatchConstraintValue' "GG"

pattern GMCVGH :: GeoMatchConstraintValue
pattern GMCVGH = GeoMatchConstraintValue' "GH"

pattern GMCVGI :: GeoMatchConstraintValue
pattern GMCVGI = GeoMatchConstraintValue' "GI"

pattern GMCVGL :: GeoMatchConstraintValue
pattern GMCVGL = GeoMatchConstraintValue' "GL"

pattern GMCVGM :: GeoMatchConstraintValue
pattern GMCVGM = GeoMatchConstraintValue' "GM"

pattern GMCVGN :: GeoMatchConstraintValue
pattern GMCVGN = GeoMatchConstraintValue' "GN"

pattern GMCVGP :: GeoMatchConstraintValue
pattern GMCVGP = GeoMatchConstraintValue' "GP"

pattern GMCVGQ :: GeoMatchConstraintValue
pattern GMCVGQ = GeoMatchConstraintValue' "GQ"

pattern GMCVGR :: GeoMatchConstraintValue
pattern GMCVGR = GeoMatchConstraintValue' "GR"

pattern GMCVGS :: GeoMatchConstraintValue
pattern GMCVGS = GeoMatchConstraintValue' "GS"

pattern GMCVGT' :: GeoMatchConstraintValue
pattern GMCVGT' = GeoMatchConstraintValue' "GT"

pattern GMCVGU :: GeoMatchConstraintValue
pattern GMCVGU = GeoMatchConstraintValue' "GU"

pattern GMCVGW :: GeoMatchConstraintValue
pattern GMCVGW = GeoMatchConstraintValue' "GW"

pattern GMCVGY :: GeoMatchConstraintValue
pattern GMCVGY = GeoMatchConstraintValue' "GY"

pattern GMCVHK :: GeoMatchConstraintValue
pattern GMCVHK = GeoMatchConstraintValue' "HK"

pattern GMCVHM :: GeoMatchConstraintValue
pattern GMCVHM = GeoMatchConstraintValue' "HM"

pattern GMCVHN :: GeoMatchConstraintValue
pattern GMCVHN = GeoMatchConstraintValue' "HN"

pattern GMCVHR :: GeoMatchConstraintValue
pattern GMCVHR = GeoMatchConstraintValue' "HR"

pattern GMCVHT :: GeoMatchConstraintValue
pattern GMCVHT = GeoMatchConstraintValue' "HT"

pattern GMCVHU :: GeoMatchConstraintValue
pattern GMCVHU = GeoMatchConstraintValue' "HU"

pattern GMCVIE :: GeoMatchConstraintValue
pattern GMCVIE = GeoMatchConstraintValue' "IE"

pattern GMCVIL :: GeoMatchConstraintValue
pattern GMCVIL = GeoMatchConstraintValue' "IL"

pattern GMCVIM :: GeoMatchConstraintValue
pattern GMCVIM = GeoMatchConstraintValue' "IM"

pattern GMCVIN :: GeoMatchConstraintValue
pattern GMCVIN = GeoMatchConstraintValue' "IN"

pattern GMCVIO :: GeoMatchConstraintValue
pattern GMCVIO = GeoMatchConstraintValue' "IO"

pattern GMCVIQ :: GeoMatchConstraintValue
pattern GMCVIQ = GeoMatchConstraintValue' "IQ"

pattern GMCVIR :: GeoMatchConstraintValue
pattern GMCVIR = GeoMatchConstraintValue' "IR"

pattern GMCVIS :: GeoMatchConstraintValue
pattern GMCVIS = GeoMatchConstraintValue' "IS"

pattern GMCVIT :: GeoMatchConstraintValue
pattern GMCVIT = GeoMatchConstraintValue' "IT"

pattern GMCVId :: GeoMatchConstraintValue
pattern GMCVId = GeoMatchConstraintValue' "ID"

pattern GMCVJE :: GeoMatchConstraintValue
pattern GMCVJE = GeoMatchConstraintValue' "JE"

pattern GMCVJM :: GeoMatchConstraintValue
pattern GMCVJM = GeoMatchConstraintValue' "JM"

pattern GMCVJO :: GeoMatchConstraintValue
pattern GMCVJO = GeoMatchConstraintValue' "JO"

pattern GMCVJP :: GeoMatchConstraintValue
pattern GMCVJP = GeoMatchConstraintValue' "JP"

pattern GMCVKE :: GeoMatchConstraintValue
pattern GMCVKE = GeoMatchConstraintValue' "KE"

pattern GMCVKG :: GeoMatchConstraintValue
pattern GMCVKG = GeoMatchConstraintValue' "KG"

pattern GMCVKH :: GeoMatchConstraintValue
pattern GMCVKH = GeoMatchConstraintValue' "KH"

pattern GMCVKI :: GeoMatchConstraintValue
pattern GMCVKI = GeoMatchConstraintValue' "KI"

pattern GMCVKM :: GeoMatchConstraintValue
pattern GMCVKM = GeoMatchConstraintValue' "KM"

pattern GMCVKN :: GeoMatchConstraintValue
pattern GMCVKN = GeoMatchConstraintValue' "KN"

pattern GMCVKP :: GeoMatchConstraintValue
pattern GMCVKP = GeoMatchConstraintValue' "KP"

pattern GMCVKR :: GeoMatchConstraintValue
pattern GMCVKR = GeoMatchConstraintValue' "KR"

pattern GMCVKW :: GeoMatchConstraintValue
pattern GMCVKW = GeoMatchConstraintValue' "KW"

pattern GMCVKY :: GeoMatchConstraintValue
pattern GMCVKY = GeoMatchConstraintValue' "KY"

pattern GMCVKZ :: GeoMatchConstraintValue
pattern GMCVKZ = GeoMatchConstraintValue' "KZ"

pattern GMCVLA :: GeoMatchConstraintValue
pattern GMCVLA = GeoMatchConstraintValue' "LA"

pattern GMCVLB :: GeoMatchConstraintValue
pattern GMCVLB = GeoMatchConstraintValue' "LB"

pattern GMCVLC :: GeoMatchConstraintValue
pattern GMCVLC = GeoMatchConstraintValue' "LC"

pattern GMCVLI :: GeoMatchConstraintValue
pattern GMCVLI = GeoMatchConstraintValue' "LI"

pattern GMCVLK :: GeoMatchConstraintValue
pattern GMCVLK = GeoMatchConstraintValue' "LK"

pattern GMCVLR :: GeoMatchConstraintValue
pattern GMCVLR = GeoMatchConstraintValue' "LR"

pattern GMCVLS :: GeoMatchConstraintValue
pattern GMCVLS = GeoMatchConstraintValue' "LS"

pattern GMCVLT' :: GeoMatchConstraintValue
pattern GMCVLT' = GeoMatchConstraintValue' "LT"

pattern GMCVLU :: GeoMatchConstraintValue
pattern GMCVLU = GeoMatchConstraintValue' "LU"

pattern GMCVLV :: GeoMatchConstraintValue
pattern GMCVLV = GeoMatchConstraintValue' "LV"

pattern GMCVLY :: GeoMatchConstraintValue
pattern GMCVLY = GeoMatchConstraintValue' "LY"

pattern GMCVMA :: GeoMatchConstraintValue
pattern GMCVMA = GeoMatchConstraintValue' "MA"

pattern GMCVMC :: GeoMatchConstraintValue
pattern GMCVMC = GeoMatchConstraintValue' "MC"

pattern GMCVMD :: GeoMatchConstraintValue
pattern GMCVMD = GeoMatchConstraintValue' "MD"

pattern GMCVME :: GeoMatchConstraintValue
pattern GMCVME = GeoMatchConstraintValue' "ME"

pattern GMCVMF :: GeoMatchConstraintValue
pattern GMCVMF = GeoMatchConstraintValue' "MF"

pattern GMCVMG :: GeoMatchConstraintValue
pattern GMCVMG = GeoMatchConstraintValue' "MG"

pattern GMCVMH :: GeoMatchConstraintValue
pattern GMCVMH = GeoMatchConstraintValue' "MH"

pattern GMCVMK :: GeoMatchConstraintValue
pattern GMCVMK = GeoMatchConstraintValue' "MK"

pattern GMCVML :: GeoMatchConstraintValue
pattern GMCVML = GeoMatchConstraintValue' "ML"

pattern GMCVMM :: GeoMatchConstraintValue
pattern GMCVMM = GeoMatchConstraintValue' "MM"

pattern GMCVMN :: GeoMatchConstraintValue
pattern GMCVMN = GeoMatchConstraintValue' "MN"

pattern GMCVMO :: GeoMatchConstraintValue
pattern GMCVMO = GeoMatchConstraintValue' "MO"

pattern GMCVMP :: GeoMatchConstraintValue
pattern GMCVMP = GeoMatchConstraintValue' "MP"

pattern GMCVMQ :: GeoMatchConstraintValue
pattern GMCVMQ = GeoMatchConstraintValue' "MQ"

pattern GMCVMR :: GeoMatchConstraintValue
pattern GMCVMR = GeoMatchConstraintValue' "MR"

pattern GMCVMS :: GeoMatchConstraintValue
pattern GMCVMS = GeoMatchConstraintValue' "MS"

pattern GMCVMT :: GeoMatchConstraintValue
pattern GMCVMT = GeoMatchConstraintValue' "MT"

pattern GMCVMU :: GeoMatchConstraintValue
pattern GMCVMU = GeoMatchConstraintValue' "MU"

pattern GMCVMV :: GeoMatchConstraintValue
pattern GMCVMV = GeoMatchConstraintValue' "MV"

pattern GMCVMW :: GeoMatchConstraintValue
pattern GMCVMW = GeoMatchConstraintValue' "MW"

pattern GMCVMX :: GeoMatchConstraintValue
pattern GMCVMX = GeoMatchConstraintValue' "MX"

pattern GMCVMY :: GeoMatchConstraintValue
pattern GMCVMY = GeoMatchConstraintValue' "MY"

pattern GMCVMZ :: GeoMatchConstraintValue
pattern GMCVMZ = GeoMatchConstraintValue' "MZ"

pattern GMCVNA :: GeoMatchConstraintValue
pattern GMCVNA = GeoMatchConstraintValue' "NA"

pattern GMCVNC :: GeoMatchConstraintValue
pattern GMCVNC = GeoMatchConstraintValue' "NC"

pattern GMCVNE :: GeoMatchConstraintValue
pattern GMCVNE = GeoMatchConstraintValue' "NE"

pattern GMCVNF :: GeoMatchConstraintValue
pattern GMCVNF = GeoMatchConstraintValue' "NF"

pattern GMCVNG :: GeoMatchConstraintValue
pattern GMCVNG = GeoMatchConstraintValue' "NG"

pattern GMCVNI :: GeoMatchConstraintValue
pattern GMCVNI = GeoMatchConstraintValue' "NI"

pattern GMCVNL :: GeoMatchConstraintValue
pattern GMCVNL = GeoMatchConstraintValue' "NL"

pattern GMCVNO :: GeoMatchConstraintValue
pattern GMCVNO = GeoMatchConstraintValue' "NO"

pattern GMCVNP :: GeoMatchConstraintValue
pattern GMCVNP = GeoMatchConstraintValue' "NP"

pattern GMCVNR :: GeoMatchConstraintValue
pattern GMCVNR = GeoMatchConstraintValue' "NR"

pattern GMCVNU :: GeoMatchConstraintValue
pattern GMCVNU = GeoMatchConstraintValue' "NU"

pattern GMCVNZ :: GeoMatchConstraintValue
pattern GMCVNZ = GeoMatchConstraintValue' "NZ"

pattern GMCVOM :: GeoMatchConstraintValue
pattern GMCVOM = GeoMatchConstraintValue' "OM"

pattern GMCVPA :: GeoMatchConstraintValue
pattern GMCVPA = GeoMatchConstraintValue' "PA"

pattern GMCVPE :: GeoMatchConstraintValue
pattern GMCVPE = GeoMatchConstraintValue' "PE"

pattern GMCVPF :: GeoMatchConstraintValue
pattern GMCVPF = GeoMatchConstraintValue' "PF"

pattern GMCVPG :: GeoMatchConstraintValue
pattern GMCVPG = GeoMatchConstraintValue' "PG"

pattern GMCVPH :: GeoMatchConstraintValue
pattern GMCVPH = GeoMatchConstraintValue' "PH"

pattern GMCVPK :: GeoMatchConstraintValue
pattern GMCVPK = GeoMatchConstraintValue' "PK"

pattern GMCVPL :: GeoMatchConstraintValue
pattern GMCVPL = GeoMatchConstraintValue' "PL"

pattern GMCVPM :: GeoMatchConstraintValue
pattern GMCVPM = GeoMatchConstraintValue' "PM"

pattern GMCVPN :: GeoMatchConstraintValue
pattern GMCVPN = GeoMatchConstraintValue' "PN"

pattern GMCVPR :: GeoMatchConstraintValue
pattern GMCVPR = GeoMatchConstraintValue' "PR"

pattern GMCVPS :: GeoMatchConstraintValue
pattern GMCVPS = GeoMatchConstraintValue' "PS"

pattern GMCVPT :: GeoMatchConstraintValue
pattern GMCVPT = GeoMatchConstraintValue' "PT"

pattern GMCVPW :: GeoMatchConstraintValue
pattern GMCVPW = GeoMatchConstraintValue' "PW"

pattern GMCVPY :: GeoMatchConstraintValue
pattern GMCVPY = GeoMatchConstraintValue' "PY"

pattern GMCVQA :: GeoMatchConstraintValue
pattern GMCVQA = GeoMatchConstraintValue' "QA"

pattern GMCVRE :: GeoMatchConstraintValue
pattern GMCVRE = GeoMatchConstraintValue' "RE"

pattern GMCVRO :: GeoMatchConstraintValue
pattern GMCVRO = GeoMatchConstraintValue' "RO"

pattern GMCVRS :: GeoMatchConstraintValue
pattern GMCVRS = GeoMatchConstraintValue' "RS"

pattern GMCVRU :: GeoMatchConstraintValue
pattern GMCVRU = GeoMatchConstraintValue' "RU"

pattern GMCVRW :: GeoMatchConstraintValue
pattern GMCVRW = GeoMatchConstraintValue' "RW"

pattern GMCVSA :: GeoMatchConstraintValue
pattern GMCVSA = GeoMatchConstraintValue' "SA"

pattern GMCVSB :: GeoMatchConstraintValue
pattern GMCVSB = GeoMatchConstraintValue' "SB"

pattern GMCVSC :: GeoMatchConstraintValue
pattern GMCVSC = GeoMatchConstraintValue' "SC"

pattern GMCVSD :: GeoMatchConstraintValue
pattern GMCVSD = GeoMatchConstraintValue' "SD"

pattern GMCVSE :: GeoMatchConstraintValue
pattern GMCVSE = GeoMatchConstraintValue' "SE"

pattern GMCVSG :: GeoMatchConstraintValue
pattern GMCVSG = GeoMatchConstraintValue' "SG"

pattern GMCVSH :: GeoMatchConstraintValue
pattern GMCVSH = GeoMatchConstraintValue' "SH"

pattern GMCVSI :: GeoMatchConstraintValue
pattern GMCVSI = GeoMatchConstraintValue' "SI"

pattern GMCVSJ :: GeoMatchConstraintValue
pattern GMCVSJ = GeoMatchConstraintValue' "SJ"

pattern GMCVSK :: GeoMatchConstraintValue
pattern GMCVSK = GeoMatchConstraintValue' "SK"

pattern GMCVSL :: GeoMatchConstraintValue
pattern GMCVSL = GeoMatchConstraintValue' "SL"

pattern GMCVSM :: GeoMatchConstraintValue
pattern GMCVSM = GeoMatchConstraintValue' "SM"

pattern GMCVSN :: GeoMatchConstraintValue
pattern GMCVSN = GeoMatchConstraintValue' "SN"

pattern GMCVSO :: GeoMatchConstraintValue
pattern GMCVSO = GeoMatchConstraintValue' "SO"

pattern GMCVSR :: GeoMatchConstraintValue
pattern GMCVSR = GeoMatchConstraintValue' "SR"

pattern GMCVSS :: GeoMatchConstraintValue
pattern GMCVSS = GeoMatchConstraintValue' "SS"

pattern GMCVST :: GeoMatchConstraintValue
pattern GMCVST = GeoMatchConstraintValue' "ST"

pattern GMCVSV :: GeoMatchConstraintValue
pattern GMCVSV = GeoMatchConstraintValue' "SV"

pattern GMCVSX :: GeoMatchConstraintValue
pattern GMCVSX = GeoMatchConstraintValue' "SX"

pattern GMCVSY :: GeoMatchConstraintValue
pattern GMCVSY = GeoMatchConstraintValue' "SY"

pattern GMCVSZ :: GeoMatchConstraintValue
pattern GMCVSZ = GeoMatchConstraintValue' "SZ"

pattern GMCVTC :: GeoMatchConstraintValue
pattern GMCVTC = GeoMatchConstraintValue' "TC"

pattern GMCVTD :: GeoMatchConstraintValue
pattern GMCVTD = GeoMatchConstraintValue' "TD"

pattern GMCVTF :: GeoMatchConstraintValue
pattern GMCVTF = GeoMatchConstraintValue' "TF"

pattern GMCVTG :: GeoMatchConstraintValue
pattern GMCVTG = GeoMatchConstraintValue' "TG"

pattern GMCVTH :: GeoMatchConstraintValue
pattern GMCVTH = GeoMatchConstraintValue' "TH"

pattern GMCVTJ :: GeoMatchConstraintValue
pattern GMCVTJ = GeoMatchConstraintValue' "TJ"

pattern GMCVTK :: GeoMatchConstraintValue
pattern GMCVTK = GeoMatchConstraintValue' "TK"

pattern GMCVTL :: GeoMatchConstraintValue
pattern GMCVTL = GeoMatchConstraintValue' "TL"

pattern GMCVTM :: GeoMatchConstraintValue
pattern GMCVTM = GeoMatchConstraintValue' "TM"

pattern GMCVTN :: GeoMatchConstraintValue
pattern GMCVTN = GeoMatchConstraintValue' "TN"

pattern GMCVTO :: GeoMatchConstraintValue
pattern GMCVTO = GeoMatchConstraintValue' "TO"

pattern GMCVTR :: GeoMatchConstraintValue
pattern GMCVTR = GeoMatchConstraintValue' "TR"

pattern GMCVTT :: GeoMatchConstraintValue
pattern GMCVTT = GeoMatchConstraintValue' "TT"

pattern GMCVTV :: GeoMatchConstraintValue
pattern GMCVTV = GeoMatchConstraintValue' "TV"

pattern GMCVTW :: GeoMatchConstraintValue
pattern GMCVTW = GeoMatchConstraintValue' "TW"

pattern GMCVTZ :: GeoMatchConstraintValue
pattern GMCVTZ = GeoMatchConstraintValue' "TZ"

pattern GMCVUA :: GeoMatchConstraintValue
pattern GMCVUA = GeoMatchConstraintValue' "UA"

pattern GMCVUG :: GeoMatchConstraintValue
pattern GMCVUG = GeoMatchConstraintValue' "UG"

pattern GMCVUM :: GeoMatchConstraintValue
pattern GMCVUM = GeoMatchConstraintValue' "UM"

pattern GMCVUS :: GeoMatchConstraintValue
pattern GMCVUS = GeoMatchConstraintValue' "US"

pattern GMCVUY :: GeoMatchConstraintValue
pattern GMCVUY = GeoMatchConstraintValue' "UY"

pattern GMCVUZ :: GeoMatchConstraintValue
pattern GMCVUZ = GeoMatchConstraintValue' "UZ"

pattern GMCVVA :: GeoMatchConstraintValue
pattern GMCVVA = GeoMatchConstraintValue' "VA"

pattern GMCVVC :: GeoMatchConstraintValue
pattern GMCVVC = GeoMatchConstraintValue' "VC"

pattern GMCVVE :: GeoMatchConstraintValue
pattern GMCVVE = GeoMatchConstraintValue' "VE"

pattern GMCVVG :: GeoMatchConstraintValue
pattern GMCVVG = GeoMatchConstraintValue' "VG"

pattern GMCVVI :: GeoMatchConstraintValue
pattern GMCVVI = GeoMatchConstraintValue' "VI"

pattern GMCVVN :: GeoMatchConstraintValue
pattern GMCVVN = GeoMatchConstraintValue' "VN"

pattern GMCVVU :: GeoMatchConstraintValue
pattern GMCVVU = GeoMatchConstraintValue' "VU"

pattern GMCVWF :: GeoMatchConstraintValue
pattern GMCVWF = GeoMatchConstraintValue' "WF"

pattern GMCVWS :: GeoMatchConstraintValue
pattern GMCVWS = GeoMatchConstraintValue' "WS"

pattern GMCVYE :: GeoMatchConstraintValue
pattern GMCVYE = GeoMatchConstraintValue' "YE"

pattern GMCVYT :: GeoMatchConstraintValue
pattern GMCVYT = GeoMatchConstraintValue' "YT"

pattern GMCVZA :: GeoMatchConstraintValue
pattern GMCVZA = GeoMatchConstraintValue' "ZA"

pattern GMCVZM :: GeoMatchConstraintValue
pattern GMCVZM = GeoMatchConstraintValue' "ZM"

pattern GMCVZW :: GeoMatchConstraintValue
pattern GMCVZW = GeoMatchConstraintValue' "ZW"

{-# COMPLETE
  GMCVAD,
  GMCVAE,
  GMCVAF,
  GMCVAG,
  GMCVAI,
  GMCVAL,
  GMCVAM,
  GMCVAO,
  GMCVAQ,
  GMCVAR,
  GMCVAS,
  GMCVAT,
  GMCVAU,
  GMCVAW,
  GMCVAX,
  GMCVAZ,
  GMCVBA,
  GMCVBB,
  GMCVBD,
  GMCVBE,
  GMCVBF,
  GMCVBG,
  GMCVBH,
  GMCVBI,
  GMCVBJ,
  GMCVBL,
  GMCVBM,
  GMCVBN,
  GMCVBO,
  GMCVBQ,
  GMCVBR,
  GMCVBS,
  GMCVBT,
  GMCVBV,
  GMCVBW,
  GMCVBY,
  GMCVBZ,
  GMCVCA,
  GMCVCC,
  GMCVCD,
  GMCVCF,
  GMCVCG,
  GMCVCH,
  GMCVCI,
  GMCVCK,
  GMCVCL,
  GMCVCM,
  GMCVCN,
  GMCVCO,
  GMCVCR,
  GMCVCU,
  GMCVCV,
  GMCVCW,
  GMCVCX,
  GMCVCY,
  GMCVCZ,
  GMCVDE,
  GMCVDJ,
  GMCVDK,
  GMCVDM,
  GMCVDO,
  GMCVDZ,
  GMCVEC,
  GMCVEE,
  GMCVEG,
  GMCVEH,
  GMCVER,
  GMCVES,
  GMCVET,
  GMCVFI,
  GMCVFJ,
  GMCVFK,
  GMCVFM,
  GMCVFO,
  GMCVFR,
  GMCVGA,
  GMCVGB,
  GMCVGD,
  GMCVGE,
  GMCVGF,
  GMCVGG,
  GMCVGH,
  GMCVGI,
  GMCVGL,
  GMCVGM,
  GMCVGN,
  GMCVGP,
  GMCVGQ,
  GMCVGR,
  GMCVGS,
  GMCVGT',
  GMCVGU,
  GMCVGW,
  GMCVGY,
  GMCVHK,
  GMCVHM,
  GMCVHN,
  GMCVHR,
  GMCVHT,
  GMCVHU,
  GMCVIE,
  GMCVIL,
  GMCVIM,
  GMCVIN,
  GMCVIO,
  GMCVIQ,
  GMCVIR,
  GMCVIS,
  GMCVIT,
  GMCVId,
  GMCVJE,
  GMCVJM,
  GMCVJO,
  GMCVJP,
  GMCVKE,
  GMCVKG,
  GMCVKH,
  GMCVKI,
  GMCVKM,
  GMCVKN,
  GMCVKP,
  GMCVKR,
  GMCVKW,
  GMCVKY,
  GMCVKZ,
  GMCVLA,
  GMCVLB,
  GMCVLC,
  GMCVLI,
  GMCVLK,
  GMCVLR,
  GMCVLS,
  GMCVLT',
  GMCVLU,
  GMCVLV,
  GMCVLY,
  GMCVMA,
  GMCVMC,
  GMCVMD,
  GMCVME,
  GMCVMF,
  GMCVMG,
  GMCVMH,
  GMCVMK,
  GMCVML,
  GMCVMM,
  GMCVMN,
  GMCVMO,
  GMCVMP,
  GMCVMQ,
  GMCVMR,
  GMCVMS,
  GMCVMT,
  GMCVMU,
  GMCVMV,
  GMCVMW,
  GMCVMX,
  GMCVMY,
  GMCVMZ,
  GMCVNA,
  GMCVNC,
  GMCVNE,
  GMCVNF,
  GMCVNG,
  GMCVNI,
  GMCVNL,
  GMCVNO,
  GMCVNP,
  GMCVNR,
  GMCVNU,
  GMCVNZ,
  GMCVOM,
  GMCVPA,
  GMCVPE,
  GMCVPF,
  GMCVPG,
  GMCVPH,
  GMCVPK,
  GMCVPL,
  GMCVPM,
  GMCVPN,
  GMCVPR,
  GMCVPS,
  GMCVPT,
  GMCVPW,
  GMCVPY,
  GMCVQA,
  GMCVRE,
  GMCVRO,
  GMCVRS,
  GMCVRU,
  GMCVRW,
  GMCVSA,
  GMCVSB,
  GMCVSC,
  GMCVSD,
  GMCVSE,
  GMCVSG,
  GMCVSH,
  GMCVSI,
  GMCVSJ,
  GMCVSK,
  GMCVSL,
  GMCVSM,
  GMCVSN,
  GMCVSO,
  GMCVSR,
  GMCVSS,
  GMCVST,
  GMCVSV,
  GMCVSX,
  GMCVSY,
  GMCVSZ,
  GMCVTC,
  GMCVTD,
  GMCVTF,
  GMCVTG,
  GMCVTH,
  GMCVTJ,
  GMCVTK,
  GMCVTL,
  GMCVTM,
  GMCVTN,
  GMCVTO,
  GMCVTR,
  GMCVTT,
  GMCVTV,
  GMCVTW,
  GMCVTZ,
  GMCVUA,
  GMCVUG,
  GMCVUM,
  GMCVUS,
  GMCVUY,
  GMCVUZ,
  GMCVVA,
  GMCVVC,
  GMCVVE,
  GMCVVG,
  GMCVVI,
  GMCVVN,
  GMCVVU,
  GMCVWF,
  GMCVWS,
  GMCVYE,
  GMCVYT,
  GMCVZA,
  GMCVZM,
  GMCVZW,
  GeoMatchConstraintValue' #-}

instance FromText GeoMatchConstraintValue where
    parser = (GeoMatchConstraintValue' . mk) <$> takeText

instance ToText GeoMatchConstraintValue where
    toText (GeoMatchConstraintValue' ci) = original ci

-- | Represents an enum of /known/ $GeoMatchConstraintValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GeoMatchConstraintValue where
    toEnum i = case i of
        0 -> GMCVAD
        1 -> GMCVAE
        2 -> GMCVAF
        3 -> GMCVAG
        4 -> GMCVAI
        5 -> GMCVAL
        6 -> GMCVAM
        7 -> GMCVAO
        8 -> GMCVAQ
        9 -> GMCVAR
        10 -> GMCVAS
        11 -> GMCVAT
        12 -> GMCVAU
        13 -> GMCVAW
        14 -> GMCVAX
        15 -> GMCVAZ
        16 -> GMCVBA
        17 -> GMCVBB
        18 -> GMCVBD
        19 -> GMCVBE
        20 -> GMCVBF
        21 -> GMCVBG
        22 -> GMCVBH
        23 -> GMCVBI
        24 -> GMCVBJ
        25 -> GMCVBL
        26 -> GMCVBM
        27 -> GMCVBN
        28 -> GMCVBO
        29 -> GMCVBQ
        30 -> GMCVBR
        31 -> GMCVBS
        32 -> GMCVBT
        33 -> GMCVBV
        34 -> GMCVBW
        35 -> GMCVBY
        36 -> GMCVBZ
        37 -> GMCVCA
        38 -> GMCVCC
        39 -> GMCVCD
        40 -> GMCVCF
        41 -> GMCVCG
        42 -> GMCVCH
        43 -> GMCVCI
        44 -> GMCVCK
        45 -> GMCVCL
        46 -> GMCVCM
        47 -> GMCVCN
        48 -> GMCVCO
        49 -> GMCVCR
        50 -> GMCVCU
        51 -> GMCVCV
        52 -> GMCVCW
        53 -> GMCVCX
        54 -> GMCVCY
        55 -> GMCVCZ
        56 -> GMCVDE
        57 -> GMCVDJ
        58 -> GMCVDK
        59 -> GMCVDM
        60 -> GMCVDO
        61 -> GMCVDZ
        62 -> GMCVEC
        63 -> GMCVEE
        64 -> GMCVEG
        65 -> GMCVEH
        66 -> GMCVER
        67 -> GMCVES
        68 -> GMCVET
        69 -> GMCVFI
        70 -> GMCVFJ
        71 -> GMCVFK
        72 -> GMCVFM
        73 -> GMCVFO
        74 -> GMCVFR
        75 -> GMCVGA
        76 -> GMCVGB
        77 -> GMCVGD
        78 -> GMCVGE
        79 -> GMCVGF
        80 -> GMCVGG
        81 -> GMCVGH
        82 -> GMCVGI
        83 -> GMCVGL
        84 -> GMCVGM
        85 -> GMCVGN
        86 -> GMCVGP
        87 -> GMCVGQ
        88 -> GMCVGR
        89 -> GMCVGS
        90 -> GMCVGT'
        91 -> GMCVGU
        92 -> GMCVGW
        93 -> GMCVGY
        94 -> GMCVHK
        95 -> GMCVHM
        96 -> GMCVHN
        97 -> GMCVHR
        98 -> GMCVHT
        99 -> GMCVHU
        100 -> GMCVIE
        101 -> GMCVIL
        102 -> GMCVIM
        103 -> GMCVIN
        104 -> GMCVIO
        105 -> GMCVIQ
        106 -> GMCVIR
        107 -> GMCVIS
        108 -> GMCVIT
        109 -> GMCVId
        110 -> GMCVJE
        111 -> GMCVJM
        112 -> GMCVJO
        113 -> GMCVJP
        114 -> GMCVKE
        115 -> GMCVKG
        116 -> GMCVKH
        117 -> GMCVKI
        118 -> GMCVKM
        119 -> GMCVKN
        120 -> GMCVKP
        121 -> GMCVKR
        122 -> GMCVKW
        123 -> GMCVKY
        124 -> GMCVKZ
        125 -> GMCVLA
        126 -> GMCVLB
        127 -> GMCVLC
        128 -> GMCVLI
        129 -> GMCVLK
        130 -> GMCVLR
        131 -> GMCVLS
        132 -> GMCVLT'
        133 -> GMCVLU
        134 -> GMCVLV
        135 -> GMCVLY
        136 -> GMCVMA
        137 -> GMCVMC
        138 -> GMCVMD
        139 -> GMCVME
        140 -> GMCVMF
        141 -> GMCVMG
        142 -> GMCVMH
        143 -> GMCVMK
        144 -> GMCVML
        145 -> GMCVMM
        146 -> GMCVMN
        147 -> GMCVMO
        148 -> GMCVMP
        149 -> GMCVMQ
        150 -> GMCVMR
        151 -> GMCVMS
        152 -> GMCVMT
        153 -> GMCVMU
        154 -> GMCVMV
        155 -> GMCVMW
        156 -> GMCVMX
        157 -> GMCVMY
        158 -> GMCVMZ
        159 -> GMCVNA
        160 -> GMCVNC
        161 -> GMCVNE
        162 -> GMCVNF
        163 -> GMCVNG
        164 -> GMCVNI
        165 -> GMCVNL
        166 -> GMCVNO
        167 -> GMCVNP
        168 -> GMCVNR
        169 -> GMCVNU
        170 -> GMCVNZ
        171 -> GMCVOM
        172 -> GMCVPA
        173 -> GMCVPE
        174 -> GMCVPF
        175 -> GMCVPG
        176 -> GMCVPH
        177 -> GMCVPK
        178 -> GMCVPL
        179 -> GMCVPM
        180 -> GMCVPN
        181 -> GMCVPR
        182 -> GMCVPS
        183 -> GMCVPT
        184 -> GMCVPW
        185 -> GMCVPY
        186 -> GMCVQA
        187 -> GMCVRE
        188 -> GMCVRO
        189 -> GMCVRS
        190 -> GMCVRU
        191 -> GMCVRW
        192 -> GMCVSA
        193 -> GMCVSB
        194 -> GMCVSC
        195 -> GMCVSD
        196 -> GMCVSE
        197 -> GMCVSG
        198 -> GMCVSH
        199 -> GMCVSI
        200 -> GMCVSJ
        201 -> GMCVSK
        202 -> GMCVSL
        203 -> GMCVSM
        204 -> GMCVSN
        205 -> GMCVSO
        206 -> GMCVSR
        207 -> GMCVSS
        208 -> GMCVST
        209 -> GMCVSV
        210 -> GMCVSX
        211 -> GMCVSY
        212 -> GMCVSZ
        213 -> GMCVTC
        214 -> GMCVTD
        215 -> GMCVTF
        216 -> GMCVTG
        217 -> GMCVTH
        218 -> GMCVTJ
        219 -> GMCVTK
        220 -> GMCVTL
        221 -> GMCVTM
        222 -> GMCVTN
        223 -> GMCVTO
        224 -> GMCVTR
        225 -> GMCVTT
        226 -> GMCVTV
        227 -> GMCVTW
        228 -> GMCVTZ
        229 -> GMCVUA
        230 -> GMCVUG
        231 -> GMCVUM
        232 -> GMCVUS
        233 -> GMCVUY
        234 -> GMCVUZ
        235 -> GMCVVA
        236 -> GMCVVC
        237 -> GMCVVE
        238 -> GMCVVG
        239 -> GMCVVI
        240 -> GMCVVN
        241 -> GMCVVU
        242 -> GMCVWF
        243 -> GMCVWS
        244 -> GMCVYE
        245 -> GMCVYT
        246 -> GMCVZA
        247 -> GMCVZM
        248 -> GMCVZW
        _ -> (error . showText) $ "Unknown index for GeoMatchConstraintValue: " <> toText i
    fromEnum x = case x of
        GMCVAD -> 0
        GMCVAE -> 1
        GMCVAF -> 2
        GMCVAG -> 3
        GMCVAI -> 4
        GMCVAL -> 5
        GMCVAM -> 6
        GMCVAO -> 7
        GMCVAQ -> 8
        GMCVAR -> 9
        GMCVAS -> 10
        GMCVAT -> 11
        GMCVAU -> 12
        GMCVAW -> 13
        GMCVAX -> 14
        GMCVAZ -> 15
        GMCVBA -> 16
        GMCVBB -> 17
        GMCVBD -> 18
        GMCVBE -> 19
        GMCVBF -> 20
        GMCVBG -> 21
        GMCVBH -> 22
        GMCVBI -> 23
        GMCVBJ -> 24
        GMCVBL -> 25
        GMCVBM -> 26
        GMCVBN -> 27
        GMCVBO -> 28
        GMCVBQ -> 29
        GMCVBR -> 30
        GMCVBS -> 31
        GMCVBT -> 32
        GMCVBV -> 33
        GMCVBW -> 34
        GMCVBY -> 35
        GMCVBZ -> 36
        GMCVCA -> 37
        GMCVCC -> 38
        GMCVCD -> 39
        GMCVCF -> 40
        GMCVCG -> 41
        GMCVCH -> 42
        GMCVCI -> 43
        GMCVCK -> 44
        GMCVCL -> 45
        GMCVCM -> 46
        GMCVCN -> 47
        GMCVCO -> 48
        GMCVCR -> 49
        GMCVCU -> 50
        GMCVCV -> 51
        GMCVCW -> 52
        GMCVCX -> 53
        GMCVCY -> 54
        GMCVCZ -> 55
        GMCVDE -> 56
        GMCVDJ -> 57
        GMCVDK -> 58
        GMCVDM -> 59
        GMCVDO -> 60
        GMCVDZ -> 61
        GMCVEC -> 62
        GMCVEE -> 63
        GMCVEG -> 64
        GMCVEH -> 65
        GMCVER -> 66
        GMCVES -> 67
        GMCVET -> 68
        GMCVFI -> 69
        GMCVFJ -> 70
        GMCVFK -> 71
        GMCVFM -> 72
        GMCVFO -> 73
        GMCVFR -> 74
        GMCVGA -> 75
        GMCVGB -> 76
        GMCVGD -> 77
        GMCVGE -> 78
        GMCVGF -> 79
        GMCVGG -> 80
        GMCVGH -> 81
        GMCVGI -> 82
        GMCVGL -> 83
        GMCVGM -> 84
        GMCVGN -> 85
        GMCVGP -> 86
        GMCVGQ -> 87
        GMCVGR -> 88
        GMCVGS -> 89
        GMCVGT' -> 90
        GMCVGU -> 91
        GMCVGW -> 92
        GMCVGY -> 93
        GMCVHK -> 94
        GMCVHM -> 95
        GMCVHN -> 96
        GMCVHR -> 97
        GMCVHT -> 98
        GMCVHU -> 99
        GMCVIE -> 100
        GMCVIL -> 101
        GMCVIM -> 102
        GMCVIN -> 103
        GMCVIO -> 104
        GMCVIQ -> 105
        GMCVIR -> 106
        GMCVIS -> 107
        GMCVIT -> 108
        GMCVId -> 109
        GMCVJE -> 110
        GMCVJM -> 111
        GMCVJO -> 112
        GMCVJP -> 113
        GMCVKE -> 114
        GMCVKG -> 115
        GMCVKH -> 116
        GMCVKI -> 117
        GMCVKM -> 118
        GMCVKN -> 119
        GMCVKP -> 120
        GMCVKR -> 121
        GMCVKW -> 122
        GMCVKY -> 123
        GMCVKZ -> 124
        GMCVLA -> 125
        GMCVLB -> 126
        GMCVLC -> 127
        GMCVLI -> 128
        GMCVLK -> 129
        GMCVLR -> 130
        GMCVLS -> 131
        GMCVLT' -> 132
        GMCVLU -> 133
        GMCVLV -> 134
        GMCVLY -> 135
        GMCVMA -> 136
        GMCVMC -> 137
        GMCVMD -> 138
        GMCVME -> 139
        GMCVMF -> 140
        GMCVMG -> 141
        GMCVMH -> 142
        GMCVMK -> 143
        GMCVML -> 144
        GMCVMM -> 145
        GMCVMN -> 146
        GMCVMO -> 147
        GMCVMP -> 148
        GMCVMQ -> 149
        GMCVMR -> 150
        GMCVMS -> 151
        GMCVMT -> 152
        GMCVMU -> 153
        GMCVMV -> 154
        GMCVMW -> 155
        GMCVMX -> 156
        GMCVMY -> 157
        GMCVMZ -> 158
        GMCVNA -> 159
        GMCVNC -> 160
        GMCVNE -> 161
        GMCVNF -> 162
        GMCVNG -> 163
        GMCVNI -> 164
        GMCVNL -> 165
        GMCVNO -> 166
        GMCVNP -> 167
        GMCVNR -> 168
        GMCVNU -> 169
        GMCVNZ -> 170
        GMCVOM -> 171
        GMCVPA -> 172
        GMCVPE -> 173
        GMCVPF -> 174
        GMCVPG -> 175
        GMCVPH -> 176
        GMCVPK -> 177
        GMCVPL -> 178
        GMCVPM -> 179
        GMCVPN -> 180
        GMCVPR -> 181
        GMCVPS -> 182
        GMCVPT -> 183
        GMCVPW -> 184
        GMCVPY -> 185
        GMCVQA -> 186
        GMCVRE -> 187
        GMCVRO -> 188
        GMCVRS -> 189
        GMCVRU -> 190
        GMCVRW -> 191
        GMCVSA -> 192
        GMCVSB -> 193
        GMCVSC -> 194
        GMCVSD -> 195
        GMCVSE -> 196
        GMCVSG -> 197
        GMCVSH -> 198
        GMCVSI -> 199
        GMCVSJ -> 200
        GMCVSK -> 201
        GMCVSL -> 202
        GMCVSM -> 203
        GMCVSN -> 204
        GMCVSO -> 205
        GMCVSR -> 206
        GMCVSS -> 207
        GMCVST -> 208
        GMCVSV -> 209
        GMCVSX -> 210
        GMCVSY -> 211
        GMCVSZ -> 212
        GMCVTC -> 213
        GMCVTD -> 214
        GMCVTF -> 215
        GMCVTG -> 216
        GMCVTH -> 217
        GMCVTJ -> 218
        GMCVTK -> 219
        GMCVTL -> 220
        GMCVTM -> 221
        GMCVTN -> 222
        GMCVTO -> 223
        GMCVTR -> 224
        GMCVTT -> 225
        GMCVTV -> 226
        GMCVTW -> 227
        GMCVTZ -> 228
        GMCVUA -> 229
        GMCVUG -> 230
        GMCVUM -> 231
        GMCVUS -> 232
        GMCVUY -> 233
        GMCVUZ -> 234
        GMCVVA -> 235
        GMCVVC -> 236
        GMCVVE -> 237
        GMCVVG -> 238
        GMCVVI -> 239
        GMCVVN -> 240
        GMCVVU -> 241
        GMCVWF -> 242
        GMCVWS -> 243
        GMCVYE -> 244
        GMCVYT -> 245
        GMCVZA -> 246
        GMCVZM -> 247
        GMCVZW -> 248
        GeoMatchConstraintValue' name -> (error . showText) $ "Unknown GeoMatchConstraintValue: " <> original name

-- | Represents the bounds of /known/ $GeoMatchConstraintValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GeoMatchConstraintValue where
    minBound = GMCVAD
    maxBound = GMCVZW

instance Hashable     GeoMatchConstraintValue
instance NFData       GeoMatchConstraintValue
instance ToByteString GeoMatchConstraintValue
instance ToQuery      GeoMatchConstraintValue
instance ToHeader     GeoMatchConstraintValue

instance ToJSON GeoMatchConstraintValue where
    toJSON = toJSONText

instance FromJSON GeoMatchConstraintValue where
    parseJSON = parseJSONText "GeoMatchConstraintValue"
