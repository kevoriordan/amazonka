{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding (
  CloudWatchLogsEncoding (
    ..
    , Ascii
    , BIG5
    , Big5hkscs
    , CP037
    , CP1006
    , CP1026
    , CP1140
    , CP1250
    , CP1251
    , CP1252
    , CP1253
    , CP1254
    , CP1255
    , CP1256
    , CP1257
    , CP1258
    , CP424
    , CP437
    , CP500
    , CP720
    , CP737
    , CP775
    , CP850
    , CP852
    , CP855
    , CP856
    , CP857
    , CP858
    , CP860
    , CP861
    , CP862
    , CP863
    , CP864
    , CP865
    , CP866
    , CP869
    , CP874
    , CP875
    , CP932
    , CP949
    , CP950
    , EucJISX0213
    , EucJis2004
    , EucJp
    , EucKr
    , GB18030
    , GB2312
    , Gbk
    , HZ
    , ISO2022Jp
    , ISO2022Jp1
    , ISO2022Jp2
    , ISO2022Jp2004
    , ISO2022Jp3
    , ISO2022JpExt
    , ISO2022Kr
    , ISO885910
    , ISO885913
    , ISO885914
    , ISO885915
    , ISO885916
    , ISO88592
    , ISO88593
    , ISO88594
    , ISO88595
    , ISO88596
    , ISO88597
    , ISO88598
    , ISO88599
    , Johab
    , KOI8r
    , KOI8u
    , Latin1
    , MACCyrillic
    , MACGreek
    , MACIceland
    , MACLATIN2
    , MACRoman
    , MACTurkish
    , PTCP154
    , ShiftJISX0213
    , ShiftJis
    , ShiftJis2004
    , Utf16
    , Utf16Be
    , Utf16Le
    , Utf32
    , Utf32Be
    , Utf32Le
    , Utf7
    , Utf8
    , Utf8Sig
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
--
--
data CloudWatchLogsEncoding = CloudWatchLogsEncoding' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Ascii :: CloudWatchLogsEncoding
pattern Ascii = CloudWatchLogsEncoding' "ascii"

pattern BIG5 :: CloudWatchLogsEncoding
pattern BIG5 = CloudWatchLogsEncoding' "big5"

pattern Big5hkscs :: CloudWatchLogsEncoding
pattern Big5hkscs = CloudWatchLogsEncoding' "big5hkscs"

pattern CP037 :: CloudWatchLogsEncoding
pattern CP037 = CloudWatchLogsEncoding' "cp037"

pattern CP1006 :: CloudWatchLogsEncoding
pattern CP1006 = CloudWatchLogsEncoding' "cp1006"

pattern CP1026 :: CloudWatchLogsEncoding
pattern CP1026 = CloudWatchLogsEncoding' "cp1026"

pattern CP1140 :: CloudWatchLogsEncoding
pattern CP1140 = CloudWatchLogsEncoding' "cp1140"

pattern CP1250 :: CloudWatchLogsEncoding
pattern CP1250 = CloudWatchLogsEncoding' "cp1250"

pattern CP1251 :: CloudWatchLogsEncoding
pattern CP1251 = CloudWatchLogsEncoding' "cp1251"

pattern CP1252 :: CloudWatchLogsEncoding
pattern CP1252 = CloudWatchLogsEncoding' "cp1252"

pattern CP1253 :: CloudWatchLogsEncoding
pattern CP1253 = CloudWatchLogsEncoding' "cp1253"

pattern CP1254 :: CloudWatchLogsEncoding
pattern CP1254 = CloudWatchLogsEncoding' "cp1254"

pattern CP1255 :: CloudWatchLogsEncoding
pattern CP1255 = CloudWatchLogsEncoding' "cp1255"

pattern CP1256 :: CloudWatchLogsEncoding
pattern CP1256 = CloudWatchLogsEncoding' "cp1256"

pattern CP1257 :: CloudWatchLogsEncoding
pattern CP1257 = CloudWatchLogsEncoding' "cp1257"

pattern CP1258 :: CloudWatchLogsEncoding
pattern CP1258 = CloudWatchLogsEncoding' "cp1258"

pattern CP424 :: CloudWatchLogsEncoding
pattern CP424 = CloudWatchLogsEncoding' "cp424"

pattern CP437 :: CloudWatchLogsEncoding
pattern CP437 = CloudWatchLogsEncoding' "cp437"

pattern CP500 :: CloudWatchLogsEncoding
pattern CP500 = CloudWatchLogsEncoding' "cp500"

pattern CP720 :: CloudWatchLogsEncoding
pattern CP720 = CloudWatchLogsEncoding' "cp720"

pattern CP737 :: CloudWatchLogsEncoding
pattern CP737 = CloudWatchLogsEncoding' "cp737"

pattern CP775 :: CloudWatchLogsEncoding
pattern CP775 = CloudWatchLogsEncoding' "cp775"

pattern CP850 :: CloudWatchLogsEncoding
pattern CP850 = CloudWatchLogsEncoding' "cp850"

pattern CP852 :: CloudWatchLogsEncoding
pattern CP852 = CloudWatchLogsEncoding' "cp852"

pattern CP855 :: CloudWatchLogsEncoding
pattern CP855 = CloudWatchLogsEncoding' "cp855"

pattern CP856 :: CloudWatchLogsEncoding
pattern CP856 = CloudWatchLogsEncoding' "cp856"

pattern CP857 :: CloudWatchLogsEncoding
pattern CP857 = CloudWatchLogsEncoding' "cp857"

pattern CP858 :: CloudWatchLogsEncoding
pattern CP858 = CloudWatchLogsEncoding' "cp858"

pattern CP860 :: CloudWatchLogsEncoding
pattern CP860 = CloudWatchLogsEncoding' "cp860"

pattern CP861 :: CloudWatchLogsEncoding
pattern CP861 = CloudWatchLogsEncoding' "cp861"

pattern CP862 :: CloudWatchLogsEncoding
pattern CP862 = CloudWatchLogsEncoding' "cp862"

pattern CP863 :: CloudWatchLogsEncoding
pattern CP863 = CloudWatchLogsEncoding' "cp863"

pattern CP864 :: CloudWatchLogsEncoding
pattern CP864 = CloudWatchLogsEncoding' "cp864"

pattern CP865 :: CloudWatchLogsEncoding
pattern CP865 = CloudWatchLogsEncoding' "cp865"

pattern CP866 :: CloudWatchLogsEncoding
pattern CP866 = CloudWatchLogsEncoding' "cp866"

pattern CP869 :: CloudWatchLogsEncoding
pattern CP869 = CloudWatchLogsEncoding' "cp869"

pattern CP874 :: CloudWatchLogsEncoding
pattern CP874 = CloudWatchLogsEncoding' "cp874"

pattern CP875 :: CloudWatchLogsEncoding
pattern CP875 = CloudWatchLogsEncoding' "cp875"

pattern CP932 :: CloudWatchLogsEncoding
pattern CP932 = CloudWatchLogsEncoding' "cp932"

pattern CP949 :: CloudWatchLogsEncoding
pattern CP949 = CloudWatchLogsEncoding' "cp949"

pattern CP950 :: CloudWatchLogsEncoding
pattern CP950 = CloudWatchLogsEncoding' "cp950"

pattern EucJISX0213 :: CloudWatchLogsEncoding
pattern EucJISX0213 = CloudWatchLogsEncoding' "euc_jisx0213"

pattern EucJis2004 :: CloudWatchLogsEncoding
pattern EucJis2004 = CloudWatchLogsEncoding' "euc_jis_2004"

pattern EucJp :: CloudWatchLogsEncoding
pattern EucJp = CloudWatchLogsEncoding' "euc_jp"

pattern EucKr :: CloudWatchLogsEncoding
pattern EucKr = CloudWatchLogsEncoding' "euc_kr"

pattern GB18030 :: CloudWatchLogsEncoding
pattern GB18030 = CloudWatchLogsEncoding' "gb18030"

pattern GB2312 :: CloudWatchLogsEncoding
pattern GB2312 = CloudWatchLogsEncoding' "gb2312"

pattern Gbk :: CloudWatchLogsEncoding
pattern Gbk = CloudWatchLogsEncoding' "gbk"

pattern HZ :: CloudWatchLogsEncoding
pattern HZ = CloudWatchLogsEncoding' "hz"

pattern ISO2022Jp :: CloudWatchLogsEncoding
pattern ISO2022Jp = CloudWatchLogsEncoding' "iso2022_jp"

pattern ISO2022Jp1 :: CloudWatchLogsEncoding
pattern ISO2022Jp1 = CloudWatchLogsEncoding' "iso2022_jp_1"

pattern ISO2022Jp2 :: CloudWatchLogsEncoding
pattern ISO2022Jp2 = CloudWatchLogsEncoding' "iso2022_jp_2"

pattern ISO2022Jp2004 :: CloudWatchLogsEncoding
pattern ISO2022Jp2004 = CloudWatchLogsEncoding' "iso2022_jp_2004"

pattern ISO2022Jp3 :: CloudWatchLogsEncoding
pattern ISO2022Jp3 = CloudWatchLogsEncoding' "iso2022_jp_3"

pattern ISO2022JpExt :: CloudWatchLogsEncoding
pattern ISO2022JpExt = CloudWatchLogsEncoding' "iso2022_jp_ext"

pattern ISO2022Kr :: CloudWatchLogsEncoding
pattern ISO2022Kr = CloudWatchLogsEncoding' "iso2022_kr"

pattern ISO885910 :: CloudWatchLogsEncoding
pattern ISO885910 = CloudWatchLogsEncoding' "iso8859_10"

pattern ISO885913 :: CloudWatchLogsEncoding
pattern ISO885913 = CloudWatchLogsEncoding' "iso8859_13"

pattern ISO885914 :: CloudWatchLogsEncoding
pattern ISO885914 = CloudWatchLogsEncoding' "iso8859_14"

pattern ISO885915 :: CloudWatchLogsEncoding
pattern ISO885915 = CloudWatchLogsEncoding' "iso8859_15"

pattern ISO885916 :: CloudWatchLogsEncoding
pattern ISO885916 = CloudWatchLogsEncoding' "iso8859_16"

pattern ISO88592 :: CloudWatchLogsEncoding
pattern ISO88592 = CloudWatchLogsEncoding' "iso8859_2"

pattern ISO88593 :: CloudWatchLogsEncoding
pattern ISO88593 = CloudWatchLogsEncoding' "iso8859_3"

pattern ISO88594 :: CloudWatchLogsEncoding
pattern ISO88594 = CloudWatchLogsEncoding' "iso8859_4"

pattern ISO88595 :: CloudWatchLogsEncoding
pattern ISO88595 = CloudWatchLogsEncoding' "iso8859_5"

pattern ISO88596 :: CloudWatchLogsEncoding
pattern ISO88596 = CloudWatchLogsEncoding' "iso8859_6"

pattern ISO88597 :: CloudWatchLogsEncoding
pattern ISO88597 = CloudWatchLogsEncoding' "iso8859_7"

pattern ISO88598 :: CloudWatchLogsEncoding
pattern ISO88598 = CloudWatchLogsEncoding' "iso8859_8"

pattern ISO88599 :: CloudWatchLogsEncoding
pattern ISO88599 = CloudWatchLogsEncoding' "iso8859_9"

pattern Johab :: CloudWatchLogsEncoding
pattern Johab = CloudWatchLogsEncoding' "johab"

pattern KOI8r :: CloudWatchLogsEncoding
pattern KOI8r = CloudWatchLogsEncoding' "koi8_r"

pattern KOI8u :: CloudWatchLogsEncoding
pattern KOI8u = CloudWatchLogsEncoding' "koi8_u"

pattern Latin1 :: CloudWatchLogsEncoding
pattern Latin1 = CloudWatchLogsEncoding' "latin_1"

pattern MACCyrillic :: CloudWatchLogsEncoding
pattern MACCyrillic = CloudWatchLogsEncoding' "mac_cyrillic"

pattern MACGreek :: CloudWatchLogsEncoding
pattern MACGreek = CloudWatchLogsEncoding' "mac_greek"

pattern MACIceland :: CloudWatchLogsEncoding
pattern MACIceland = CloudWatchLogsEncoding' "mac_iceland"

pattern MACLATIN2 :: CloudWatchLogsEncoding
pattern MACLATIN2 = CloudWatchLogsEncoding' "mac_latin2"

pattern MACRoman :: CloudWatchLogsEncoding
pattern MACRoman = CloudWatchLogsEncoding' "mac_roman"

pattern MACTurkish :: CloudWatchLogsEncoding
pattern MACTurkish = CloudWatchLogsEncoding' "mac_turkish"

pattern PTCP154 :: CloudWatchLogsEncoding
pattern PTCP154 = CloudWatchLogsEncoding' "ptcp154"

pattern ShiftJISX0213 :: CloudWatchLogsEncoding
pattern ShiftJISX0213 = CloudWatchLogsEncoding' "shift_jisx0213"

pattern ShiftJis :: CloudWatchLogsEncoding
pattern ShiftJis = CloudWatchLogsEncoding' "shift_jis"

pattern ShiftJis2004 :: CloudWatchLogsEncoding
pattern ShiftJis2004 = CloudWatchLogsEncoding' "shift_jis_2004"

pattern Utf16 :: CloudWatchLogsEncoding
pattern Utf16 = CloudWatchLogsEncoding' "utf_16"

pattern Utf16Be :: CloudWatchLogsEncoding
pattern Utf16Be = CloudWatchLogsEncoding' "utf_16_be"

pattern Utf16Le :: CloudWatchLogsEncoding
pattern Utf16Le = CloudWatchLogsEncoding' "utf_16_le"

pattern Utf32 :: CloudWatchLogsEncoding
pattern Utf32 = CloudWatchLogsEncoding' "utf_32"

pattern Utf32Be :: CloudWatchLogsEncoding
pattern Utf32Be = CloudWatchLogsEncoding' "utf_32_be"

pattern Utf32Le :: CloudWatchLogsEncoding
pattern Utf32Le = CloudWatchLogsEncoding' "utf_32_le"

pattern Utf7 :: CloudWatchLogsEncoding
pattern Utf7 = CloudWatchLogsEncoding' "utf_7"

pattern Utf8 :: CloudWatchLogsEncoding
pattern Utf8 = CloudWatchLogsEncoding' "utf_8"

pattern Utf8Sig :: CloudWatchLogsEncoding
pattern Utf8Sig = CloudWatchLogsEncoding' "utf_8_sig"

{-# COMPLETE
  Ascii,
  BIG5,
  Big5hkscs,
  CP037,
  CP1006,
  CP1026,
  CP1140,
  CP1250,
  CP1251,
  CP1252,
  CP1253,
  CP1254,
  CP1255,
  CP1256,
  CP1257,
  CP1258,
  CP424,
  CP437,
  CP500,
  CP720,
  CP737,
  CP775,
  CP850,
  CP852,
  CP855,
  CP856,
  CP857,
  CP858,
  CP860,
  CP861,
  CP862,
  CP863,
  CP864,
  CP865,
  CP866,
  CP869,
  CP874,
  CP875,
  CP932,
  CP949,
  CP950,
  EucJISX0213,
  EucJis2004,
  EucJp,
  EucKr,
  GB18030,
  GB2312,
  Gbk,
  HZ,
  ISO2022Jp,
  ISO2022Jp1,
  ISO2022Jp2,
  ISO2022Jp2004,
  ISO2022Jp3,
  ISO2022JpExt,
  ISO2022Kr,
  ISO885910,
  ISO885913,
  ISO885914,
  ISO885915,
  ISO885916,
  ISO88592,
  ISO88593,
  ISO88594,
  ISO88595,
  ISO88596,
  ISO88597,
  ISO88598,
  ISO88599,
  Johab,
  KOI8r,
  KOI8u,
  Latin1,
  MACCyrillic,
  MACGreek,
  MACIceland,
  MACLATIN2,
  MACRoman,
  MACTurkish,
  PTCP154,
  ShiftJISX0213,
  ShiftJis,
  ShiftJis2004,
  Utf16,
  Utf16Be,
  Utf16Le,
  Utf32,
  Utf32Be,
  Utf32Le,
  Utf7,
  Utf8,
  Utf8Sig,
  CloudWatchLogsEncoding' #-}

instance FromText CloudWatchLogsEncoding where
    parser = (CloudWatchLogsEncoding' . mk) <$> takeText

instance ToText CloudWatchLogsEncoding where
    toText (CloudWatchLogsEncoding' ci) = original ci

-- | Represents an enum of /known/ $CloudWatchLogsEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CloudWatchLogsEncoding where
    toEnum i = case i of
        0 -> Ascii
        1 -> BIG5
        2 -> Big5hkscs
        3 -> CP037
        4 -> CP1006
        5 -> CP1026
        6 -> CP1140
        7 -> CP1250
        8 -> CP1251
        9 -> CP1252
        10 -> CP1253
        11 -> CP1254
        12 -> CP1255
        13 -> CP1256
        14 -> CP1257
        15 -> CP1258
        16 -> CP424
        17 -> CP437
        18 -> CP500
        19 -> CP720
        20 -> CP737
        21 -> CP775
        22 -> CP850
        23 -> CP852
        24 -> CP855
        25 -> CP856
        26 -> CP857
        27 -> CP858
        28 -> CP860
        29 -> CP861
        30 -> CP862
        31 -> CP863
        32 -> CP864
        33 -> CP865
        34 -> CP866
        35 -> CP869
        36 -> CP874
        37 -> CP875
        38 -> CP932
        39 -> CP949
        40 -> CP950
        41 -> EucJISX0213
        42 -> EucJis2004
        43 -> EucJp
        44 -> EucKr
        45 -> GB18030
        46 -> GB2312
        47 -> Gbk
        48 -> HZ
        49 -> ISO2022Jp
        50 -> ISO2022Jp1
        51 -> ISO2022Jp2
        52 -> ISO2022Jp2004
        53 -> ISO2022Jp3
        54 -> ISO2022JpExt
        55 -> ISO2022Kr
        56 -> ISO885910
        57 -> ISO885913
        58 -> ISO885914
        59 -> ISO885915
        60 -> ISO885916
        61 -> ISO88592
        62 -> ISO88593
        63 -> ISO88594
        64 -> ISO88595
        65 -> ISO88596
        66 -> ISO88597
        67 -> ISO88598
        68 -> ISO88599
        69 -> Johab
        70 -> KOI8r
        71 -> KOI8u
        72 -> Latin1
        73 -> MACCyrillic
        74 -> MACGreek
        75 -> MACIceland
        76 -> MACLATIN2
        77 -> MACRoman
        78 -> MACTurkish
        79 -> PTCP154
        80 -> ShiftJISX0213
        81 -> ShiftJis
        82 -> ShiftJis2004
        83 -> Utf16
        84 -> Utf16Be
        85 -> Utf16Le
        86 -> Utf32
        87 -> Utf32Be
        88 -> Utf32Le
        89 -> Utf7
        90 -> Utf8
        91 -> Utf8Sig
        _ -> (error . showText) $ "Unknown index for CloudWatchLogsEncoding: " <> toText i
    fromEnum x = case x of
        Ascii -> 0
        BIG5 -> 1
        Big5hkscs -> 2
        CP037 -> 3
        CP1006 -> 4
        CP1026 -> 5
        CP1140 -> 6
        CP1250 -> 7
        CP1251 -> 8
        CP1252 -> 9
        CP1253 -> 10
        CP1254 -> 11
        CP1255 -> 12
        CP1256 -> 13
        CP1257 -> 14
        CP1258 -> 15
        CP424 -> 16
        CP437 -> 17
        CP500 -> 18
        CP720 -> 19
        CP737 -> 20
        CP775 -> 21
        CP850 -> 22
        CP852 -> 23
        CP855 -> 24
        CP856 -> 25
        CP857 -> 26
        CP858 -> 27
        CP860 -> 28
        CP861 -> 29
        CP862 -> 30
        CP863 -> 31
        CP864 -> 32
        CP865 -> 33
        CP866 -> 34
        CP869 -> 35
        CP874 -> 36
        CP875 -> 37
        CP932 -> 38
        CP949 -> 39
        CP950 -> 40
        EucJISX0213 -> 41
        EucJis2004 -> 42
        EucJp -> 43
        EucKr -> 44
        GB18030 -> 45
        GB2312 -> 46
        Gbk -> 47
        HZ -> 48
        ISO2022Jp -> 49
        ISO2022Jp1 -> 50
        ISO2022Jp2 -> 51
        ISO2022Jp2004 -> 52
        ISO2022Jp3 -> 53
        ISO2022JpExt -> 54
        ISO2022Kr -> 55
        ISO885910 -> 56
        ISO885913 -> 57
        ISO885914 -> 58
        ISO885915 -> 59
        ISO885916 -> 60
        ISO88592 -> 61
        ISO88593 -> 62
        ISO88594 -> 63
        ISO88595 -> 64
        ISO88596 -> 65
        ISO88597 -> 66
        ISO88598 -> 67
        ISO88599 -> 68
        Johab -> 69
        KOI8r -> 70
        KOI8u -> 71
        Latin1 -> 72
        MACCyrillic -> 73
        MACGreek -> 74
        MACIceland -> 75
        MACLATIN2 -> 76
        MACRoman -> 77
        MACTurkish -> 78
        PTCP154 -> 79
        ShiftJISX0213 -> 80
        ShiftJis -> 81
        ShiftJis2004 -> 82
        Utf16 -> 83
        Utf16Be -> 84
        Utf16Le -> 85
        Utf32 -> 86
        Utf32Be -> 87
        Utf32Le -> 88
        Utf7 -> 89
        Utf8 -> 90
        Utf8Sig -> 91
        CloudWatchLogsEncoding' name -> (error . showText) $ "Unknown CloudWatchLogsEncoding: " <> original name

-- | Represents the bounds of /known/ $CloudWatchLogsEncoding.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CloudWatchLogsEncoding where
    minBound = Ascii
    maxBound = Utf8Sig

instance Hashable     CloudWatchLogsEncoding
instance NFData       CloudWatchLogsEncoding
instance ToByteString CloudWatchLogsEncoding
instance ToQuery      CloudWatchLogsEncoding
instance ToHeader     CloudWatchLogsEncoding

instance ToJSON CloudWatchLogsEncoding where
    toJSON = toJSONText

instance FromJSON CloudWatchLogsEncoding where
    parseJSON = parseJSONText "CloudWatchLogsEncoding"
