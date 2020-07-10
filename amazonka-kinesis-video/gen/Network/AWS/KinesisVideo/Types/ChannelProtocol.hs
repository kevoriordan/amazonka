{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelProtocol
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.ChannelProtocol (
  ChannelProtocol (
    ..
    , HTTPS
    , Wss
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChannelProtocol = ChannelProtocol' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern HTTPS :: ChannelProtocol
pattern HTTPS = ChannelProtocol' "HTTPS"

pattern Wss :: ChannelProtocol
pattern Wss = ChannelProtocol' "WSS"

{-# COMPLETE
  HTTPS,
  Wss,
  ChannelProtocol' #-}

instance FromText ChannelProtocol where
    parser = (ChannelProtocol' . mk) <$> takeText

instance ToText ChannelProtocol where
    toText (ChannelProtocol' ci) = original ci

-- | Represents an enum of /known/ $ChannelProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChannelProtocol where
    toEnum i = case i of
        0 -> HTTPS
        1 -> Wss
        _ -> (error . showText) $ "Unknown index for ChannelProtocol: " <> toText i
    fromEnum x = case x of
        HTTPS -> 0
        Wss -> 1
        ChannelProtocol' name -> (error . showText) $ "Unknown ChannelProtocol: " <> original name

-- | Represents the bounds of /known/ $ChannelProtocol.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChannelProtocol where
    minBound = HTTPS
    maxBound = Wss

instance Hashable     ChannelProtocol
instance NFData       ChannelProtocol
instance ToByteString ChannelProtocol
instance ToQuery      ChannelProtocol
instance ToHeader     ChannelProtocol

instance ToJSON ChannelProtocol where
    toJSON = toJSONText

instance FromJSON ChannelProtocol where
    parseJSON = parseJSONText "ChannelProtocol"
