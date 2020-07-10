{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioLanguageCodeControl (
  AudioLanguageCodeControl (
    ..
    , FollowInput
    , UseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
data AudioLanguageCodeControl = AudioLanguageCodeControl' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern FollowInput :: AudioLanguageCodeControl
pattern FollowInput = AudioLanguageCodeControl' "FOLLOW_INPUT"

pattern UseConfigured :: AudioLanguageCodeControl
pattern UseConfigured = AudioLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  FollowInput,
  UseConfigured,
  AudioLanguageCodeControl' #-}

instance FromText AudioLanguageCodeControl where
    parser = (AudioLanguageCodeControl' . mk) <$> takeText

instance ToText AudioLanguageCodeControl where
    toText (AudioLanguageCodeControl' ci) = original ci

-- | Represents an enum of /known/ $AudioLanguageCodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioLanguageCodeControl where
    toEnum i = case i of
        0 -> FollowInput
        1 -> UseConfigured
        _ -> (error . showText) $ "Unknown index for AudioLanguageCodeControl: " <> toText i
    fromEnum x = case x of
        FollowInput -> 0
        UseConfigured -> 1
        AudioLanguageCodeControl' name -> (error . showText) $ "Unknown AudioLanguageCodeControl: " <> original name

-- | Represents the bounds of /known/ $AudioLanguageCodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioLanguageCodeControl where
    minBound = FollowInput
    maxBound = UseConfigured

instance Hashable     AudioLanguageCodeControl
instance NFData       AudioLanguageCodeControl
instance ToByteString AudioLanguageCodeControl
instance ToQuery      AudioLanguageCodeControl
instance ToHeader     AudioLanguageCodeControl

instance ToJSON AudioLanguageCodeControl where
    toJSON = toJSONText

instance FromJSON AudioLanguageCodeControl where
    parseJSON = parseJSONText "AudioLanguageCodeControl"
