{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsMode (
  HlsMode (
    ..
    , Live
    , Vod
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Mode
data HlsMode = HlsMode' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Live :: HlsMode
pattern Live = HlsMode' "LIVE"

pattern Vod :: HlsMode
pattern Vod = HlsMode' "VOD"

{-# COMPLETE
  Live,
  Vod,
  HlsMode' #-}

instance FromText HlsMode where
    parser = (HlsMode' . mk) <$> takeText

instance ToText HlsMode where
    toText (HlsMode' ci) = original ci

-- | Represents an enum of /known/ $HlsMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsMode where
    toEnum i = case i of
        0 -> Live
        1 -> Vod
        _ -> (error . showText) $ "Unknown index for HlsMode: " <> toText i
    fromEnum x = case x of
        Live -> 0
        Vod -> 1
        HlsMode' name -> (error . showText) $ "Unknown HlsMode: " <> original name

-- | Represents the bounds of /known/ $HlsMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsMode where
    minBound = Live
    maxBound = Vod

instance Hashable     HlsMode
instance NFData       HlsMode
instance ToByteString HlsMode
instance ToQuery      HlsMode
instance ToHeader     HlsMode

instance ToJSON HlsMode where
    toJSON = toJSONText

instance FromJSON HlsMode where
    parseJSON = parseJSONText "HlsMode"
