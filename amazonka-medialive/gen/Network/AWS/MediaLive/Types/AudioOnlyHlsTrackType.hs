{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType (
  AudioOnlyHlsTrackType (
    ..
    , AlternateAudioAutoSelect
    , AlternateAudioAutoSelectDefault
    , AlternateAudioNotAutoSelect
    , AudioOnlyVariantStream
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Only Hls Track Type
data AudioOnlyHlsTrackType = AudioOnlyHlsTrackType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern AlternateAudioAutoSelect :: AudioOnlyHlsTrackType
pattern AlternateAudioAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT"

pattern AlternateAudioAutoSelectDefault :: AudioOnlyHlsTrackType
pattern AlternateAudioAutoSelectDefault = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT"

pattern AlternateAudioNotAutoSelect :: AudioOnlyHlsTrackType
pattern AlternateAudioNotAutoSelect = AudioOnlyHlsTrackType' "ALTERNATE_AUDIO_NOT_AUTO_SELECT"

pattern AudioOnlyVariantStream :: AudioOnlyHlsTrackType
pattern AudioOnlyVariantStream = AudioOnlyHlsTrackType' "AUDIO_ONLY_VARIANT_STREAM"

{-# COMPLETE
  AlternateAudioAutoSelect,
  AlternateAudioAutoSelectDefault,
  AlternateAudioNotAutoSelect,
  AudioOnlyVariantStream,
  AudioOnlyHlsTrackType' #-}

instance FromText AudioOnlyHlsTrackType where
    parser = (AudioOnlyHlsTrackType' . mk) <$> takeText

instance ToText AudioOnlyHlsTrackType where
    toText (AudioOnlyHlsTrackType' ci) = original ci

-- | Represents an enum of /known/ $AudioOnlyHlsTrackType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioOnlyHlsTrackType where
    toEnum i = case i of
        0 -> AlternateAudioAutoSelect
        1 -> AlternateAudioAutoSelectDefault
        2 -> AlternateAudioNotAutoSelect
        3 -> AudioOnlyVariantStream
        _ -> (error . showText) $ "Unknown index for AudioOnlyHlsTrackType: " <> toText i
    fromEnum x = case x of
        AlternateAudioAutoSelect -> 0
        AlternateAudioAutoSelectDefault -> 1
        AlternateAudioNotAutoSelect -> 2
        AudioOnlyVariantStream -> 3
        AudioOnlyHlsTrackType' name -> (error . showText) $ "Unknown AudioOnlyHlsTrackType: " <> original name

-- | Represents the bounds of /known/ $AudioOnlyHlsTrackType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioOnlyHlsTrackType where
    minBound = AlternateAudioAutoSelect
    maxBound = AudioOnlyVariantStream

instance Hashable     AudioOnlyHlsTrackType
instance NFData       AudioOnlyHlsTrackType
instance ToByteString AudioOnlyHlsTrackType
instance ToQuery      AudioOnlyHlsTrackType
instance ToHeader     AudioOnlyHlsTrackType

instance ToJSON AudioOnlyHlsTrackType where
    toJSON = toJSONText

instance FromJSON AudioOnlyHlsTrackType where
    parseJSON = parseJSONText "AudioOnlyHlsTrackType"
