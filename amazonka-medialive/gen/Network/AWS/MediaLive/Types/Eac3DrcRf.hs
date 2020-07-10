{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcRf
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3DrcRf (
  Eac3DrcRf (
    ..
    , EDRFilmLight
    , EDRFilmStandard
    , EDRMusicLight
    , EDRMusicStandard
    , EDRNone
    , EDRSpeech
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Eac3 Drc Rf
data Eac3DrcRf = Eac3DrcRf' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern EDRFilmLight :: Eac3DrcRf
pattern EDRFilmLight = Eac3DrcRf' "FILM_LIGHT"

pattern EDRFilmStandard :: Eac3DrcRf
pattern EDRFilmStandard = Eac3DrcRf' "FILM_STANDARD"

pattern EDRMusicLight :: Eac3DrcRf
pattern EDRMusicLight = Eac3DrcRf' "MUSIC_LIGHT"

pattern EDRMusicStandard :: Eac3DrcRf
pattern EDRMusicStandard = Eac3DrcRf' "MUSIC_STANDARD"

pattern EDRNone :: Eac3DrcRf
pattern EDRNone = Eac3DrcRf' "NONE"

pattern EDRSpeech :: Eac3DrcRf
pattern EDRSpeech = Eac3DrcRf' "SPEECH"

{-# COMPLETE
  EDRFilmLight,
  EDRFilmStandard,
  EDRMusicLight,
  EDRMusicStandard,
  EDRNone,
  EDRSpeech,
  Eac3DrcRf' #-}

instance FromText Eac3DrcRf where
    parser = (Eac3DrcRf' . mk) <$> takeText

instance ToText Eac3DrcRf where
    toText (Eac3DrcRf' ci) = original ci

-- | Represents an enum of /known/ $Eac3DrcRf.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3DrcRf where
    toEnum i = case i of
        0 -> EDRFilmLight
        1 -> EDRFilmStandard
        2 -> EDRMusicLight
        3 -> EDRMusicStandard
        4 -> EDRNone
        5 -> EDRSpeech
        _ -> (error . showText) $ "Unknown index for Eac3DrcRf: " <> toText i
    fromEnum x = case x of
        EDRFilmLight -> 0
        EDRFilmStandard -> 1
        EDRMusicLight -> 2
        EDRMusicStandard -> 3
        EDRNone -> 4
        EDRSpeech -> 5
        Eac3DrcRf' name -> (error . showText) $ "Unknown Eac3DrcRf: " <> original name

-- | Represents the bounds of /known/ $Eac3DrcRf.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3DrcRf where
    minBound = EDRFilmLight
    maxBound = EDRSpeech

instance Hashable     Eac3DrcRf
instance NFData       Eac3DrcRf
instance ToByteString Eac3DrcRf
instance ToQuery      Eac3DrcRf
instance ToHeader     Eac3DrcRf

instance ToJSON Eac3DrcRf where
    toJSON = toJSONText

instance FromJSON Eac3DrcRf where
    parseJSON = parseJSONText "Eac3DrcRf"
