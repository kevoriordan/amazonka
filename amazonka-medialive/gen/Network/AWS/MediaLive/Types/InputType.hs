{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputType (
  InputType (
    ..
    , MP4File
    , Mediaconnect
    , RtmpPull
    , RtmpPush
    , RtpPush
    , URLPull
    , UdpPush
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for InputType
data InputType = InputType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern MP4File :: InputType
pattern MP4File = InputType' "MP4_FILE"

pattern Mediaconnect :: InputType
pattern Mediaconnect = InputType' "MEDIACONNECT"

pattern RtmpPull :: InputType
pattern RtmpPull = InputType' "RTMP_PULL"

pattern RtmpPush :: InputType
pattern RtmpPush = InputType' "RTMP_PUSH"

pattern RtpPush :: InputType
pattern RtpPush = InputType' "RTP_PUSH"

pattern URLPull :: InputType
pattern URLPull = InputType' "URL_PULL"

pattern UdpPush :: InputType
pattern UdpPush = InputType' "UDP_PUSH"

{-# COMPLETE
  MP4File,
  Mediaconnect,
  RtmpPull,
  RtmpPush,
  RtpPush,
  URLPull,
  UdpPush,
  InputType' #-}

instance FromText InputType where
    parser = (InputType' . mk) <$> takeText

instance ToText InputType where
    toText (InputType' ci) = original ci

-- | Represents an enum of /known/ $InputType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputType where
    toEnum i = case i of
        0 -> MP4File
        1 -> Mediaconnect
        2 -> RtmpPull
        3 -> RtmpPush
        4 -> RtpPush
        5 -> URLPull
        6 -> UdpPush
        _ -> (error . showText) $ "Unknown index for InputType: " <> toText i
    fromEnum x = case x of
        MP4File -> 0
        Mediaconnect -> 1
        RtmpPull -> 2
        RtmpPush -> 3
        RtpPush -> 4
        URLPull -> 5
        UdpPush -> 6
        InputType' name -> (error . showText) $ "Unknown InputType: " <> original name

-- | Represents the bounds of /known/ $InputType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputType where
    minBound = MP4File
    maxBound = UdpPush

instance Hashable     InputType
instance NFData       InputType
instance ToByteString InputType
instance ToQuery      InputType
instance ToHeader     InputType

instance ToJSON InputType where
    toJSON = toJSONText

instance FromJSON InputType where
    parseJSON = parseJSONText "InputType"
