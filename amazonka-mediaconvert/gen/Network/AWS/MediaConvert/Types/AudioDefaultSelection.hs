{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioDefaultSelection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioDefaultSelection (
  AudioDefaultSelection (
    ..
    , ADSDefault
    , ADSNotDefault
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
data AudioDefaultSelection = AudioDefaultSelection' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ADSDefault :: AudioDefaultSelection
pattern ADSDefault = AudioDefaultSelection' "DEFAULT"

pattern ADSNotDefault :: AudioDefaultSelection
pattern ADSNotDefault = AudioDefaultSelection' "NOT_DEFAULT"

{-# COMPLETE
  ADSDefault,
  ADSNotDefault,
  AudioDefaultSelection' #-}

instance FromText AudioDefaultSelection where
    parser = (AudioDefaultSelection' . mk) <$> takeText

instance ToText AudioDefaultSelection where
    toText (AudioDefaultSelection' ci) = original ci

-- | Represents an enum of /known/ $AudioDefaultSelection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioDefaultSelection where
    toEnum i = case i of
        0 -> ADSDefault
        1 -> ADSNotDefault
        _ -> (error . showText) $ "Unknown index for AudioDefaultSelection: " <> toText i
    fromEnum x = case x of
        ADSDefault -> 0
        ADSNotDefault -> 1
        AudioDefaultSelection' name -> (error . showText) $ "Unknown AudioDefaultSelection: " <> original name

-- | Represents the bounds of /known/ $AudioDefaultSelection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioDefaultSelection where
    minBound = ADSDefault
    maxBound = ADSNotDefault

instance Hashable     AudioDefaultSelection
instance NFData       AudioDefaultSelection
instance ToByteString AudioDefaultSelection
instance ToQuery      AudioDefaultSelection
instance ToHeader     AudioDefaultSelection

instance ToJSON AudioDefaultSelection where
    toJSON = toJSONText

instance FromJSON AudioDefaultSelection where
    parseJSON = parseJSONText "AudioDefaultSelection"
