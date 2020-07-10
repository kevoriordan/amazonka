{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.MediaFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.MediaFormat (
  MediaFormat (
    ..
    , Flac
    , MP3
    , MP4
    , Wav
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MediaFormat = MediaFormat' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Flac :: MediaFormat
pattern Flac = MediaFormat' "flac"

pattern MP3 :: MediaFormat
pattern MP3 = MediaFormat' "mp3"

pattern MP4 :: MediaFormat
pattern MP4 = MediaFormat' "mp4"

pattern Wav :: MediaFormat
pattern Wav = MediaFormat' "wav"

{-# COMPLETE
  Flac,
  MP3,
  MP4,
  Wav,
  MediaFormat' #-}

instance FromText MediaFormat where
    parser = (MediaFormat' . mk) <$> takeText

instance ToText MediaFormat where
    toText (MediaFormat' ci) = original ci

-- | Represents an enum of /known/ $MediaFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MediaFormat where
    toEnum i = case i of
        0 -> Flac
        1 -> MP3
        2 -> MP4
        3 -> Wav
        _ -> (error . showText) $ "Unknown index for MediaFormat: " <> toText i
    fromEnum x = case x of
        Flac -> 0
        MP3 -> 1
        MP4 -> 2
        Wav -> 3
        MediaFormat' name -> (error . showText) $ "Unknown MediaFormat: " <> original name

-- | Represents the bounds of /known/ $MediaFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MediaFormat where
    minBound = Flac
    maxBound = Wav

instance Hashable     MediaFormat
instance NFData       MediaFormat
instance ToByteString MediaFormat
instance ToQuery      MediaFormat
instance ToHeader     MediaFormat

instance ToJSON MediaFormat where
    toJSON = toJSONText

instance FromJSON MediaFormat where
    parseJSON = parseJSONText "MediaFormat"
