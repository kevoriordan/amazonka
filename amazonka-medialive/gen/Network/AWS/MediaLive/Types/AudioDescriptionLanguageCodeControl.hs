{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl (
  AudioDescriptionLanguageCodeControl (
    ..
    , ADLCCFollowInput
    , ADLCCUseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Audio Description Language Code Control
data AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern ADLCCFollowInput :: AudioDescriptionLanguageCodeControl
pattern ADLCCFollowInput = AudioDescriptionLanguageCodeControl' "FOLLOW_INPUT"

pattern ADLCCUseConfigured :: AudioDescriptionLanguageCodeControl
pattern ADLCCUseConfigured = AudioDescriptionLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  ADLCCFollowInput,
  ADLCCUseConfigured,
  AudioDescriptionLanguageCodeControl' #-}

instance FromText AudioDescriptionLanguageCodeControl where
    parser = (AudioDescriptionLanguageCodeControl' . mk) <$> takeText

instance ToText AudioDescriptionLanguageCodeControl where
    toText (AudioDescriptionLanguageCodeControl' ci) = original ci

-- | Represents an enum of /known/ $AudioDescriptionLanguageCodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioDescriptionLanguageCodeControl where
    toEnum i = case i of
        0 -> ADLCCFollowInput
        1 -> ADLCCUseConfigured
        _ -> (error . showText) $ "Unknown index for AudioDescriptionLanguageCodeControl: " <> toText i
    fromEnum x = case x of
        ADLCCFollowInput -> 0
        ADLCCUseConfigured -> 1
        AudioDescriptionLanguageCodeControl' name -> (error . showText) $ "Unknown AudioDescriptionLanguageCodeControl: " <> original name

-- | Represents the bounds of /known/ $AudioDescriptionLanguageCodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioDescriptionLanguageCodeControl where
    minBound = ADLCCFollowInput
    maxBound = ADLCCUseConfigured

instance Hashable     AudioDescriptionLanguageCodeControl
instance NFData       AudioDescriptionLanguageCodeControl
instance ToByteString AudioDescriptionLanguageCodeControl
instance ToQuery      AudioDescriptionLanguageCodeControl
instance ToHeader     AudioDescriptionLanguageCodeControl

instance ToJSON AudioDescriptionLanguageCodeControl where
    toJSON = toJSONText

instance FromJSON AudioDescriptionLanguageCodeControl where
    parseJSON = parseJSONText "AudioDescriptionLanguageCodeControl"
