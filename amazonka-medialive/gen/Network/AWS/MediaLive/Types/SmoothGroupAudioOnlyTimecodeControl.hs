{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl (
  SmoothGroupAudioOnlyTimecodeControl (
    ..
    , SGAOTCPassthrough
    , SGAOTCUseConfiguredClock
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Smooth Group Audio Only Timecode Control
data SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern SGAOTCPassthrough :: SmoothGroupAudioOnlyTimecodeControl
pattern SGAOTCPassthrough = SmoothGroupAudioOnlyTimecodeControl' "PASSTHROUGH"

pattern SGAOTCUseConfiguredClock :: SmoothGroupAudioOnlyTimecodeControl
pattern SGAOTCUseConfiguredClock = SmoothGroupAudioOnlyTimecodeControl' "USE_CONFIGURED_CLOCK"

{-# COMPLETE
  SGAOTCPassthrough,
  SGAOTCUseConfiguredClock,
  SmoothGroupAudioOnlyTimecodeControl' #-}

instance FromText SmoothGroupAudioOnlyTimecodeControl where
    parser = (SmoothGroupAudioOnlyTimecodeControl' . mk) <$> takeText

instance ToText SmoothGroupAudioOnlyTimecodeControl where
    toText (SmoothGroupAudioOnlyTimecodeControl' ci) = original ci

-- | Represents an enum of /known/ $SmoothGroupAudioOnlyTimecodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SmoothGroupAudioOnlyTimecodeControl where
    toEnum i = case i of
        0 -> SGAOTCPassthrough
        1 -> SGAOTCUseConfiguredClock
        _ -> (error . showText) $ "Unknown index for SmoothGroupAudioOnlyTimecodeControl: " <> toText i
    fromEnum x = case x of
        SGAOTCPassthrough -> 0
        SGAOTCUseConfiguredClock -> 1
        SmoothGroupAudioOnlyTimecodeControl' name -> (error . showText) $ "Unknown SmoothGroupAudioOnlyTimecodeControl: " <> original name

-- | Represents the bounds of /known/ $SmoothGroupAudioOnlyTimecodeControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SmoothGroupAudioOnlyTimecodeControl where
    minBound = SGAOTCPassthrough
    maxBound = SGAOTCUseConfiguredClock

instance Hashable     SmoothGroupAudioOnlyTimecodeControl
instance NFData       SmoothGroupAudioOnlyTimecodeControl
instance ToByteString SmoothGroupAudioOnlyTimecodeControl
instance ToQuery      SmoothGroupAudioOnlyTimecodeControl
instance ToHeader     SmoothGroupAudioOnlyTimecodeControl

instance ToJSON SmoothGroupAudioOnlyTimecodeControl where
    toJSON = toJSONText

instance FromJSON SmoothGroupAudioOnlyTimecodeControl where
    parseJSON = parseJSONText "SmoothGroupAudioOnlyTimecodeControl"
