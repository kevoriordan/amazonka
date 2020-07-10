{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ChannelType (
  ChannelType (
    ..
    , CTADM
    , CTAPNS
    , CTAPNSSandbox
    , CTAPNSVoip
    , CTAPNSVoipSandbox
    , CTBaidu
    , CTCustom
    , CTEmail
    , CTGCM
    , CTSms
    , CTVoice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChannelType = ChannelType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CTADM :: ChannelType
pattern CTADM = ChannelType' "ADM"

pattern CTAPNS :: ChannelType
pattern CTAPNS = ChannelType' "APNS"

pattern CTAPNSSandbox :: ChannelType
pattern CTAPNSSandbox = ChannelType' "APNS_SANDBOX"

pattern CTAPNSVoip :: ChannelType
pattern CTAPNSVoip = ChannelType' "APNS_VOIP"

pattern CTAPNSVoipSandbox :: ChannelType
pattern CTAPNSVoipSandbox = ChannelType' "APNS_VOIP_SANDBOX"

pattern CTBaidu :: ChannelType
pattern CTBaidu = ChannelType' "BAIDU"

pattern CTCustom :: ChannelType
pattern CTCustom = ChannelType' "CUSTOM"

pattern CTEmail :: ChannelType
pattern CTEmail = ChannelType' "EMAIL"

pattern CTGCM :: ChannelType
pattern CTGCM = ChannelType' "GCM"

pattern CTSms :: ChannelType
pattern CTSms = ChannelType' "SMS"

pattern CTVoice :: ChannelType
pattern CTVoice = ChannelType' "VOICE"

{-# COMPLETE
  CTADM,
  CTAPNS,
  CTAPNSSandbox,
  CTAPNSVoip,
  CTAPNSVoipSandbox,
  CTBaidu,
  CTCustom,
  CTEmail,
  CTGCM,
  CTSms,
  CTVoice,
  ChannelType' #-}

instance FromText ChannelType where
    parser = (ChannelType' . mk) <$> takeText

instance ToText ChannelType where
    toText (ChannelType' ci) = original ci

-- | Represents an enum of /known/ $ChannelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelType where
    toEnum i = case i of
        0 -> CTADM
        1 -> CTAPNS
        2 -> CTAPNSSandbox
        3 -> CTAPNSVoip
        4 -> CTAPNSVoipSandbox
        5 -> CTBaidu
        6 -> CTCustom
        7 -> CTEmail
        8 -> CTGCM
        9 -> CTSms
        10 -> CTVoice
        _ -> (error . showText) $ "Unknown index for ChannelType: " <> toText i
    fromEnum x = case x of
        CTADM -> 0
        CTAPNS -> 1
        CTAPNSSandbox -> 2
        CTAPNSVoip -> 3
        CTAPNSVoipSandbox -> 4
        CTBaidu -> 5
        CTCustom -> 6
        CTEmail -> 7
        CTGCM -> 8
        CTSms -> 9
        CTVoice -> 10
        ChannelType' name -> (error . showText) $ "Unknown ChannelType: " <> original name

-- | Represents the bounds of /known/ $ChannelType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelType where
    minBound = CTADM
    maxBound = CTVoice

instance Hashable     ChannelType
instance NFData       ChannelType
instance ToByteString ChannelType
instance ToQuery      ChannelType
instance ToHeader     ChannelType

instance ToJSON ChannelType where
    toJSON = toJSONText

instance FromJSON ChannelType where
    parseJSON = parseJSONText "ChannelType"
