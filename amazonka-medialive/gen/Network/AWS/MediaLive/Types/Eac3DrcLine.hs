{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3DrcLine (
  Eac3DrcLine (
    ..
    , EDLFilmLight
    , EDLFilmStandard
    , EDLMusicLight
    , EDLMusicStandard
    , EDLNone
    , EDLSpeech
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Drc Line
data Eac3DrcLine = Eac3DrcLine' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern EDLFilmLight :: Eac3DrcLine
pattern EDLFilmLight = Eac3DrcLine' "FILM_LIGHT"

pattern EDLFilmStandard :: Eac3DrcLine
pattern EDLFilmStandard = Eac3DrcLine' "FILM_STANDARD"

pattern EDLMusicLight :: Eac3DrcLine
pattern EDLMusicLight = Eac3DrcLine' "MUSIC_LIGHT"

pattern EDLMusicStandard :: Eac3DrcLine
pattern EDLMusicStandard = Eac3DrcLine' "MUSIC_STANDARD"

pattern EDLNone :: Eac3DrcLine
pattern EDLNone = Eac3DrcLine' "NONE"

pattern EDLSpeech :: Eac3DrcLine
pattern EDLSpeech = Eac3DrcLine' "SPEECH"

{-# COMPLETE
  EDLFilmLight,
  EDLFilmStandard,
  EDLMusicLight,
  EDLMusicStandard,
  EDLNone,
  EDLSpeech,
  Eac3DrcLine' #-}

instance FromText Eac3DrcLine where
    parser = (Eac3DrcLine' . mk) <$> takeText

instance ToText Eac3DrcLine where
    toText (Eac3DrcLine' ci) = original ci

-- | Represents an enum of /known/ $Eac3DrcLine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3DrcLine where
    toEnum i = case i of
        0 -> EDLFilmLight
        1 -> EDLFilmStandard
        2 -> EDLMusicLight
        3 -> EDLMusicStandard
        4 -> EDLNone
        5 -> EDLSpeech
        _ -> (error . showText) $ "Unknown index for Eac3DrcLine: " <> toText i
    fromEnum x = case x of
        EDLFilmLight -> 0
        EDLFilmStandard -> 1
        EDLMusicLight -> 2
        EDLMusicStandard -> 3
        EDLNone -> 4
        EDLSpeech -> 5
        Eac3DrcLine' name -> (error . showText) $ "Unknown Eac3DrcLine: " <> original name

-- | Represents the bounds of /known/ $Eac3DrcLine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3DrcLine where
    minBound = EDLFilmLight
    maxBound = EDLSpeech

instance Hashable     Eac3DrcLine
instance NFData       Eac3DrcLine
instance ToByteString Eac3DrcLine
instance ToQuery      Eac3DrcLine
instance ToHeader     Eac3DrcLine

instance ToJSON Eac3DrcLine where
    toJSON = toJSONText

instance FromJSON Eac3DrcLine where
    parseJSON = parseJSONText "Eac3DrcLine"
