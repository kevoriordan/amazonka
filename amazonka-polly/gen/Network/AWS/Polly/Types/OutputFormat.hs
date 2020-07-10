{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.OutputFormat (
  OutputFormat (
    ..
    , JSON
    , MP3
    , OggVorbis
    , Pcm
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OutputFormat = OutputFormat' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern JSON :: OutputFormat
pattern JSON = OutputFormat' "json"

pattern MP3 :: OutputFormat
pattern MP3 = OutputFormat' "mp3"

pattern OggVorbis :: OutputFormat
pattern OggVorbis = OutputFormat' "ogg_vorbis"

pattern Pcm :: OutputFormat
pattern Pcm = OutputFormat' "pcm"

{-# COMPLETE
  JSON,
  MP3,
  OggVorbis,
  Pcm,
  OutputFormat' #-}

instance FromText OutputFormat where
    parser = (OutputFormat' . mk) <$> takeText

instance ToText OutputFormat where
    toText (OutputFormat' ci) = original ci

-- | Represents an enum of /known/ $OutputFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OutputFormat where
    toEnum i = case i of
        0 -> JSON
        1 -> MP3
        2 -> OggVorbis
        3 -> Pcm
        _ -> (error . showText) $ "Unknown index for OutputFormat: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        MP3 -> 1
        OggVorbis -> 2
        Pcm -> 3
        OutputFormat' name -> (error . showText) $ "Unknown OutputFormat: " <> original name

-- | Represents the bounds of /known/ $OutputFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OutputFormat where
    minBound = JSON
    maxBound = Pcm

instance Hashable     OutputFormat
instance NFData       OutputFormat
instance ToByteString OutputFormat
instance ToQuery      OutputFormat
instance ToHeader     OutputFormat

instance ToJSON OutputFormat where
    toJSON = toJSONText

instance FromJSON OutputFormat where
    parseJSON = parseJSONText "OutputFormat"
