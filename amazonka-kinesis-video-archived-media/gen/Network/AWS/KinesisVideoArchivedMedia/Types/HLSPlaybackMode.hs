{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSPlaybackMode (
  HLSPlaybackMode (
    ..
    , Live
    , LiveReplay
    , OnDemand
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HLSPlaybackMode = HLSPlaybackMode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Live :: HLSPlaybackMode
pattern Live = HLSPlaybackMode' "LIVE"

pattern LiveReplay :: HLSPlaybackMode
pattern LiveReplay = HLSPlaybackMode' "LIVE_REPLAY"

pattern OnDemand :: HLSPlaybackMode
pattern OnDemand = HLSPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  Live,
  LiveReplay,
  OnDemand,
  HLSPlaybackMode' #-}

instance FromText HLSPlaybackMode where
    parser = (HLSPlaybackMode' . mk) <$> takeText

instance ToText HLSPlaybackMode where
    toText (HLSPlaybackMode' ci) = original ci

-- | Represents an enum of /known/ $HLSPlaybackMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HLSPlaybackMode where
    toEnum i = case i of
        0 -> Live
        1 -> LiveReplay
        2 -> OnDemand
        _ -> (error . showText) $ "Unknown index for HLSPlaybackMode: " <> toText i
    fromEnum x = case x of
        Live -> 0
        LiveReplay -> 1
        OnDemand -> 2
        HLSPlaybackMode' name -> (error . showText) $ "Unknown HLSPlaybackMode: " <> original name

-- | Represents the bounds of /known/ $HLSPlaybackMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HLSPlaybackMode where
    minBound = Live
    maxBound = OnDemand

instance Hashable     HLSPlaybackMode
instance NFData       HLSPlaybackMode
instance ToByteString HLSPlaybackMode
instance ToQuery      HLSPlaybackMode
instance ToHeader     HLSPlaybackMode

instance ToJSON HLSPlaybackMode where
    toJSON = toJSONText
