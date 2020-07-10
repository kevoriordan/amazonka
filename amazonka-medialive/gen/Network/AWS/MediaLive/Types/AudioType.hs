{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioType (
  AudioType (
    ..
    , CleanEffects
    , HearingImpaired
    , Undefined
    , VisualImpairedCommentary
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Type
data AudioType = AudioType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern CleanEffects :: AudioType
pattern CleanEffects = AudioType' "CLEAN_EFFECTS"

pattern HearingImpaired :: AudioType
pattern HearingImpaired = AudioType' "HEARING_IMPAIRED"

pattern Undefined :: AudioType
pattern Undefined = AudioType' "UNDEFINED"

pattern VisualImpairedCommentary :: AudioType
pattern VisualImpairedCommentary = AudioType' "VISUAL_IMPAIRED_COMMENTARY"

{-# COMPLETE
  CleanEffects,
  HearingImpaired,
  Undefined,
  VisualImpairedCommentary,
  AudioType' #-}

instance FromText AudioType where
    parser = (AudioType' . mk) <$> takeText

instance ToText AudioType where
    toText (AudioType' ci) = original ci

-- | Represents an enum of /known/ $AudioType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioType where
    toEnum i = case i of
        0 -> CleanEffects
        1 -> HearingImpaired
        2 -> Undefined
        3 -> VisualImpairedCommentary
        _ -> (error . showText) $ "Unknown index for AudioType: " <> toText i
    fromEnum x = case x of
        CleanEffects -> 0
        HearingImpaired -> 1
        Undefined -> 2
        VisualImpairedCommentary -> 3
        AudioType' name -> (error . showText) $ "Unknown AudioType: " <> original name

-- | Represents the bounds of /known/ $AudioType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioType where
    minBound = CleanEffects
    maxBound = VisualImpairedCommentary

instance Hashable     AudioType
instance NFData       AudioType
instance ToByteString AudioType
instance ToQuery      AudioType
instance ToHeader     AudioType

instance ToJSON AudioType where
    toJSON = toJSONText

instance FromJSON AudioType where
    parseJSON = parseJSONText "AudioType"
