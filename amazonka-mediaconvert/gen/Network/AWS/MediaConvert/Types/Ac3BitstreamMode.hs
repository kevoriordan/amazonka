{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3BitstreamMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Ac3BitstreamMode (
  Ac3BitstreamMode (
    ..
    , ABMCommentary
    , ABMCompleteMain
    , ABMDialogue
    , ABMEmergency
    , ABMHearingImpaired
    , ABMMusicAndEffects
    , ABMVisuallyImpaired
    , ABMVoiceOver
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify the bitstream mode for the AC-3 stream that the encoder emits. For more information about the AC3 bitstream mode, see ATSC A/52-2012 (Annex E).
data Ac3BitstreamMode = Ac3BitstreamMode' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ABMCommentary :: Ac3BitstreamMode
pattern ABMCommentary = Ac3BitstreamMode' "COMMENTARY"

pattern ABMCompleteMain :: Ac3BitstreamMode
pattern ABMCompleteMain = Ac3BitstreamMode' "COMPLETE_MAIN"

pattern ABMDialogue :: Ac3BitstreamMode
pattern ABMDialogue = Ac3BitstreamMode' "DIALOGUE"

pattern ABMEmergency :: Ac3BitstreamMode
pattern ABMEmergency = Ac3BitstreamMode' "EMERGENCY"

pattern ABMHearingImpaired :: Ac3BitstreamMode
pattern ABMHearingImpaired = Ac3BitstreamMode' "HEARING_IMPAIRED"

pattern ABMMusicAndEffects :: Ac3BitstreamMode
pattern ABMMusicAndEffects = Ac3BitstreamMode' "MUSIC_AND_EFFECTS"

pattern ABMVisuallyImpaired :: Ac3BitstreamMode
pattern ABMVisuallyImpaired = Ac3BitstreamMode' "VISUALLY_IMPAIRED"

pattern ABMVoiceOver :: Ac3BitstreamMode
pattern ABMVoiceOver = Ac3BitstreamMode' "VOICE_OVER"

{-# COMPLETE
  ABMCommentary,
  ABMCompleteMain,
  ABMDialogue,
  ABMEmergency,
  ABMHearingImpaired,
  ABMMusicAndEffects,
  ABMVisuallyImpaired,
  ABMVoiceOver,
  Ac3BitstreamMode' #-}

instance FromText Ac3BitstreamMode where
    parser = (Ac3BitstreamMode' . mk) <$> takeText

instance ToText Ac3BitstreamMode where
    toText (Ac3BitstreamMode' ci) = original ci

-- | Represents an enum of /known/ $Ac3BitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Ac3BitstreamMode where
    toEnum i = case i of
        0 -> ABMCommentary
        1 -> ABMCompleteMain
        2 -> ABMDialogue
        3 -> ABMEmergency
        4 -> ABMHearingImpaired
        5 -> ABMMusicAndEffects
        6 -> ABMVisuallyImpaired
        7 -> ABMVoiceOver
        _ -> (error . showText) $ "Unknown index for Ac3BitstreamMode: " <> toText i
    fromEnum x = case x of
        ABMCommentary -> 0
        ABMCompleteMain -> 1
        ABMDialogue -> 2
        ABMEmergency -> 3
        ABMHearingImpaired -> 4
        ABMMusicAndEffects -> 5
        ABMVisuallyImpaired -> 6
        ABMVoiceOver -> 7
        Ac3BitstreamMode' name -> (error . showText) $ "Unknown Ac3BitstreamMode: " <> original name

-- | Represents the bounds of /known/ $Ac3BitstreamMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Ac3BitstreamMode where
    minBound = ABMCommentary
    maxBound = ABMVoiceOver

instance Hashable     Ac3BitstreamMode
instance NFData       Ac3BitstreamMode
instance ToByteString Ac3BitstreamMode
instance ToQuery      Ac3BitstreamMode
instance ToHeader     Ac3BitstreamMode

instance ToJSON Ac3BitstreamMode where
    toJSON = toJSONText

instance FromJSON Ac3BitstreamMode where
    parseJSON = parseJSONText "Ac3BitstreamMode"
