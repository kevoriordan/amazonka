{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265SceneChangeDetect (
  H265SceneChangeDetect (
    ..
    , HSCDDisabled
    , HSCDEnabled
    , HSCDTransitionDetection
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
data H265SceneChangeDetect = H265SceneChangeDetect' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern HSCDDisabled :: H265SceneChangeDetect
pattern HSCDDisabled = H265SceneChangeDetect' "DISABLED"

pattern HSCDEnabled :: H265SceneChangeDetect
pattern HSCDEnabled = H265SceneChangeDetect' "ENABLED"

pattern HSCDTransitionDetection :: H265SceneChangeDetect
pattern HSCDTransitionDetection = H265SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  HSCDDisabled,
  HSCDEnabled,
  HSCDTransitionDetection,
  H265SceneChangeDetect' #-}

instance FromText H265SceneChangeDetect where
    parser = (H265SceneChangeDetect' . mk) <$> takeText

instance ToText H265SceneChangeDetect where
    toText (H265SceneChangeDetect' ci) = original ci

-- | Represents an enum of /known/ $H265SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265SceneChangeDetect where
    toEnum i = case i of
        0 -> HSCDDisabled
        1 -> HSCDEnabled
        2 -> HSCDTransitionDetection
        _ -> (error . showText) $ "Unknown index for H265SceneChangeDetect: " <> toText i
    fromEnum x = case x of
        HSCDDisabled -> 0
        HSCDEnabled -> 1
        HSCDTransitionDetection -> 2
        H265SceneChangeDetect' name -> (error . showText) $ "Unknown H265SceneChangeDetect: " <> original name

-- | Represents the bounds of /known/ $H265SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265SceneChangeDetect where
    minBound = HSCDDisabled
    maxBound = HSCDTransitionDetection

instance Hashable     H265SceneChangeDetect
instance NFData       H265SceneChangeDetect
instance ToByteString H265SceneChangeDetect
instance ToQuery      H265SceneChangeDetect
instance ToHeader     H265SceneChangeDetect

instance ToJSON H265SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H265SceneChangeDetect where
    parseJSON = parseJSONText "H265SceneChangeDetect"
