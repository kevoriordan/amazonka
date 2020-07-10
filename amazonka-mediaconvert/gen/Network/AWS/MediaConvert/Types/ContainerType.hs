{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ContainerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ContainerType (
  ContainerType (
    ..
    , CTCmfc
    , CTF4V
    , CTIsmv
    , CTM2TS
    , CTM3U8
    , CTMP4
    , CTMov
    , CTMpd
    , CTMxf
    , CTRaw
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
data ContainerType = ContainerType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CTCmfc :: ContainerType
pattern CTCmfc = ContainerType' "CMFC"

pattern CTF4V :: ContainerType
pattern CTF4V = ContainerType' "F4V"

pattern CTIsmv :: ContainerType
pattern CTIsmv = ContainerType' "ISMV"

pattern CTM2TS :: ContainerType
pattern CTM2TS = ContainerType' "M2TS"

pattern CTM3U8 :: ContainerType
pattern CTM3U8 = ContainerType' "M3U8"

pattern CTMP4 :: ContainerType
pattern CTMP4 = ContainerType' "MP4"

pattern CTMov :: ContainerType
pattern CTMov = ContainerType' "MOV"

pattern CTMpd :: ContainerType
pattern CTMpd = ContainerType' "MPD"

pattern CTMxf :: ContainerType
pattern CTMxf = ContainerType' "MXF"

pattern CTRaw :: ContainerType
pattern CTRaw = ContainerType' "RAW"

{-# COMPLETE
  CTCmfc,
  CTF4V,
  CTIsmv,
  CTM2TS,
  CTM3U8,
  CTMP4,
  CTMov,
  CTMpd,
  CTMxf,
  CTRaw,
  ContainerType' #-}

instance FromText ContainerType where
    parser = (ContainerType' . mk) <$> takeText

instance ToText ContainerType where
    toText (ContainerType' ci) = original ci

-- | Represents an enum of /known/ $ContainerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerType where
    toEnum i = case i of
        0 -> CTCmfc
        1 -> CTF4V
        2 -> CTIsmv
        3 -> CTM2TS
        4 -> CTM3U8
        5 -> CTMP4
        6 -> CTMov
        7 -> CTMpd
        8 -> CTMxf
        9 -> CTRaw
        _ -> (error . showText) $ "Unknown index for ContainerType: " <> toText i
    fromEnum x = case x of
        CTCmfc -> 0
        CTF4V -> 1
        CTIsmv -> 2
        CTM2TS -> 3
        CTM3U8 -> 4
        CTMP4 -> 5
        CTMov -> 6
        CTMpd -> 7
        CTMxf -> 8
        CTRaw -> 9
        ContainerType' name -> (error . showText) $ "Unknown ContainerType: " <> original name

-- | Represents the bounds of /known/ $ContainerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerType where
    minBound = CTCmfc
    maxBound = CTRaw

instance Hashable     ContainerType
instance NFData       ContainerType
instance ToByteString ContainerType
instance ToQuery      ContainerType
instance ToHeader     ContainerType

instance ToJSON ContainerType where
    toJSON = toJSONText

instance FromJSON ContainerType where
    parseJSON = parseJSONText "ContainerType"
