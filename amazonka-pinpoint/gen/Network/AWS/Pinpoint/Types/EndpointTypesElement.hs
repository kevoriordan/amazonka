{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointTypesElement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointTypesElement (
  EndpointTypesElement (
    ..
    , ADM
    , APNS
    , APNSSandbox
    , APNSVoip
    , APNSVoipSandbox
    , Baidu
    , Custom
    , Email
    , GCM
    , Sms
    , Voice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EndpointTypesElement = EndpointTypesElement' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern ADM :: EndpointTypesElement
pattern ADM = EndpointTypesElement' "ADM"

pattern APNS :: EndpointTypesElement
pattern APNS = EndpointTypesElement' "APNS"

pattern APNSSandbox :: EndpointTypesElement
pattern APNSSandbox = EndpointTypesElement' "APNS_SANDBOX"

pattern APNSVoip :: EndpointTypesElement
pattern APNSVoip = EndpointTypesElement' "APNS_VOIP"

pattern APNSVoipSandbox :: EndpointTypesElement
pattern APNSVoipSandbox = EndpointTypesElement' "APNS_VOIP_SANDBOX"

pattern Baidu :: EndpointTypesElement
pattern Baidu = EndpointTypesElement' "BAIDU"

pattern Custom :: EndpointTypesElement
pattern Custom = EndpointTypesElement' "CUSTOM"

pattern Email :: EndpointTypesElement
pattern Email = EndpointTypesElement' "EMAIL"

pattern GCM :: EndpointTypesElement
pattern GCM = EndpointTypesElement' "GCM"

pattern Sms :: EndpointTypesElement
pattern Sms = EndpointTypesElement' "SMS"

pattern Voice :: EndpointTypesElement
pattern Voice = EndpointTypesElement' "VOICE"

{-# COMPLETE
  ADM,
  APNS,
  APNSSandbox,
  APNSVoip,
  APNSVoipSandbox,
  Baidu,
  Custom,
  Email,
  GCM,
  Sms,
  Voice,
  EndpointTypesElement' #-}

instance FromText EndpointTypesElement where
    parser = (EndpointTypesElement' . mk) <$> takeText

instance ToText EndpointTypesElement where
    toText (EndpointTypesElement' ci) = original ci

-- | Represents an enum of /known/ $EndpointTypesElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndpointTypesElement where
    toEnum i = case i of
        0 -> ADM
        1 -> APNS
        2 -> APNSSandbox
        3 -> APNSVoip
        4 -> APNSVoipSandbox
        5 -> Baidu
        6 -> Custom
        7 -> Email
        8 -> GCM
        9 -> Sms
        10 -> Voice
        _ -> (error . showText) $ "Unknown index for EndpointTypesElement: " <> toText i
    fromEnum x = case x of
        ADM -> 0
        APNS -> 1
        APNSSandbox -> 2
        APNSVoip -> 3
        APNSVoipSandbox -> 4
        Baidu -> 5
        Custom -> 6
        Email -> 7
        GCM -> 8
        Sms -> 9
        Voice -> 10
        EndpointTypesElement' name -> (error . showText) $ "Unknown EndpointTypesElement: " <> original name

-- | Represents the bounds of /known/ $EndpointTypesElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndpointTypesElement where
    minBound = ADM
    maxBound = Voice

instance Hashable     EndpointTypesElement
instance NFData       EndpointTypesElement
instance ToByteString EndpointTypesElement
instance ToQuery      EndpointTypesElement
instance ToHeader     EndpointTypesElement

instance ToJSON EndpointTypesElement where
    toJSON = toJSONText

instance FromJSON EndpointTypesElement where
    parseJSON = parseJSONText "EndpointTypesElement"
