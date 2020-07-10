{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioLanguageSelectionPolicy (
  AudioLanguageSelectionPolicy (
    ..
    , Loose
    , Strict
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Language Selection Policy
data AudioLanguageSelectionPolicy = AudioLanguageSelectionPolicy' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Loose :: AudioLanguageSelectionPolicy
pattern Loose = AudioLanguageSelectionPolicy' "LOOSE"

pattern Strict :: AudioLanguageSelectionPolicy
pattern Strict = AudioLanguageSelectionPolicy' "STRICT"

{-# COMPLETE
  Loose,
  Strict,
  AudioLanguageSelectionPolicy' #-}

instance FromText AudioLanguageSelectionPolicy where
    parser = (AudioLanguageSelectionPolicy' . mk) <$> takeText

instance ToText AudioLanguageSelectionPolicy where
    toText (AudioLanguageSelectionPolicy' ci) = original ci

-- | Represents an enum of /known/ $AudioLanguageSelectionPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioLanguageSelectionPolicy where
    toEnum i = case i of
        0 -> Loose
        1 -> Strict
        _ -> (error . showText) $ "Unknown index for AudioLanguageSelectionPolicy: " <> toText i
    fromEnum x = case x of
        Loose -> 0
        Strict -> 1
        AudioLanguageSelectionPolicy' name -> (error . showText) $ "Unknown AudioLanguageSelectionPolicy: " <> original name

-- | Represents the bounds of /known/ $AudioLanguageSelectionPolicy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioLanguageSelectionPolicy where
    minBound = Loose
    maxBound = Strict

instance Hashable     AudioLanguageSelectionPolicy
instance NFData       AudioLanguageSelectionPolicy
instance ToByteString AudioLanguageSelectionPolicy
instance ToQuery      AudioLanguageSelectionPolicy
instance ToHeader     AudioLanguageSelectionPolicy

instance ToJSON AudioLanguageSelectionPolicy where
    toJSON = toJSONText

instance FromJSON AudioLanguageSelectionPolicy where
    parseJSON = parseJSONText "AudioLanguageSelectionPolicy"
