{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsEbpPlacement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsEbpPlacement (
  M2tsEbpPlacement (
    ..
    , VideoAndAudioPids
    , VideoPid
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Ebp Placement
data M2tsEbpPlacement = M2tsEbpPlacement' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern VideoAndAudioPids :: M2tsEbpPlacement
pattern VideoAndAudioPids = M2tsEbpPlacement' "VIDEO_AND_AUDIO_PIDS"

pattern VideoPid :: M2tsEbpPlacement
pattern VideoPid = M2tsEbpPlacement' "VIDEO_PID"

{-# COMPLETE
  VideoAndAudioPids,
  VideoPid,
  M2tsEbpPlacement' #-}

instance FromText M2tsEbpPlacement where
    parser = (M2tsEbpPlacement' . mk) <$> takeText

instance ToText M2tsEbpPlacement where
    toText (M2tsEbpPlacement' ci) = original ci

-- | Represents an enum of /known/ $M2tsEbpPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsEbpPlacement where
    toEnum i = case i of
        0 -> VideoAndAudioPids
        1 -> VideoPid
        _ -> (error . showText) $ "Unknown index for M2tsEbpPlacement: " <> toText i
    fromEnum x = case x of
        VideoAndAudioPids -> 0
        VideoPid -> 1
        M2tsEbpPlacement' name -> (error . showText) $ "Unknown M2tsEbpPlacement: " <> original name

-- | Represents the bounds of /known/ $M2tsEbpPlacement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsEbpPlacement where
    minBound = VideoAndAudioPids
    maxBound = VideoPid

instance Hashable     M2tsEbpPlacement
instance NFData       M2tsEbpPlacement
instance ToByteString M2tsEbpPlacement
instance ToQuery      M2tsEbpPlacement
instance ToHeader     M2tsEbpPlacement

instance ToJSON M2tsEbpPlacement where
    toJSON = toJSONText

instance FromJSON M2tsEbpPlacement where
    parseJSON = parseJSONText "M2tsEbpPlacement"
