{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix (
  AacAudioDescriptionBroadcasterMix (
    ..
    , AADBMBroadcasterMixedAd
    , AADBMNormal
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
data AacAudioDescriptionBroadcasterMix = AacAudioDescriptionBroadcasterMix' (CI
                                                                               Text)
                                           deriving (Eq, Ord, Read, Show, Data,
                                                     Typeable, Generic)

pattern AADBMBroadcasterMixedAd :: AacAudioDescriptionBroadcasterMix
pattern AADBMBroadcasterMixedAd = AacAudioDescriptionBroadcasterMix' "BROADCASTER_MIXED_AD"

pattern AADBMNormal :: AacAudioDescriptionBroadcasterMix
pattern AADBMNormal = AacAudioDescriptionBroadcasterMix' "NORMAL"

{-# COMPLETE
  AADBMBroadcasterMixedAd,
  AADBMNormal,
  AacAudioDescriptionBroadcasterMix' #-}

instance FromText AacAudioDescriptionBroadcasterMix where
    parser = (AacAudioDescriptionBroadcasterMix' . mk) <$> takeText

instance ToText AacAudioDescriptionBroadcasterMix where
    toText (AacAudioDescriptionBroadcasterMix' ci) = original ci

-- | Represents an enum of /known/ $AacAudioDescriptionBroadcasterMix.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AacAudioDescriptionBroadcasterMix where
    toEnum i = case i of
        0 -> AADBMBroadcasterMixedAd
        1 -> AADBMNormal
        _ -> (error . showText) $ "Unknown index for AacAudioDescriptionBroadcasterMix: " <> toText i
    fromEnum x = case x of
        AADBMBroadcasterMixedAd -> 0
        AADBMNormal -> 1
        AacAudioDescriptionBroadcasterMix' name -> (error . showText) $ "Unknown AacAudioDescriptionBroadcasterMix: " <> original name

-- | Represents the bounds of /known/ $AacAudioDescriptionBroadcasterMix.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AacAudioDescriptionBroadcasterMix where
    minBound = AADBMBroadcasterMixedAd
    maxBound = AADBMNormal

instance Hashable     AacAudioDescriptionBroadcasterMix
instance NFData       AacAudioDescriptionBroadcasterMix
instance ToByteString AacAudioDescriptionBroadcasterMix
instance ToQuery      AacAudioDescriptionBroadcasterMix
instance ToHeader     AacAudioDescriptionBroadcasterMix

instance ToJSON AacAudioDescriptionBroadcasterMix where
    toJSON = toJSONText

instance FromJSON AacAudioDescriptionBroadcasterMix where
    parseJSON = parseJSONText "AacAudioDescriptionBroadcasterMix"
