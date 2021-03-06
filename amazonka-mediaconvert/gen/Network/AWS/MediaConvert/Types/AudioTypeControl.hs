{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioTypeControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioTypeControl (
  AudioTypeControl (
    ..
    , ATCFollowInput
    , ATCUseConfigured
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
data AudioTypeControl = AudioTypeControl' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ATCFollowInput :: AudioTypeControl
pattern ATCFollowInput = AudioTypeControl' "FOLLOW_INPUT"

pattern ATCUseConfigured :: AudioTypeControl
pattern ATCUseConfigured = AudioTypeControl' "USE_CONFIGURED"

{-# COMPLETE
  ATCFollowInput,
  ATCUseConfigured,
  AudioTypeControl' #-}

instance FromText AudioTypeControl where
    parser = (AudioTypeControl' . mk) <$> takeText

instance ToText AudioTypeControl where
    toText (AudioTypeControl' ci) = original ci

-- | Represents an enum of /known/ $AudioTypeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AudioTypeControl where
    toEnum i = case i of
        0 -> ATCFollowInput
        1 -> ATCUseConfigured
        _ -> (error . showText) $ "Unknown index for AudioTypeControl: " <> toText i
    fromEnum x = case x of
        ATCFollowInput -> 0
        ATCUseConfigured -> 1
        AudioTypeControl' name -> (error . showText) $ "Unknown AudioTypeControl: " <> original name

-- | Represents the bounds of /known/ $AudioTypeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AudioTypeControl where
    minBound = ATCFollowInput
    maxBound = ATCUseConfigured

instance Hashable     AudioTypeControl
instance NFData       AudioTypeControl
instance ToByteString AudioTypeControl
instance ToQuery      AudioTypeControl
instance ToHeader     AudioTypeControl

instance ToJSON AudioTypeControl where
    toJSON = toJSONText

instance FromJSON AudioTypeControl where
    parseJSON = parseJSONText "AudioTypeControl"
