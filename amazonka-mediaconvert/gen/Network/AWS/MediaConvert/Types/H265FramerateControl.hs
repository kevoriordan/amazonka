{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265FramerateControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265FramerateControl (
  H265FramerateControl (
    ..
    , HFCInitializeFromSource
    , HFCSpecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job sepecification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
data H265FramerateControl = H265FramerateControl' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HFCInitializeFromSource :: H265FramerateControl
pattern HFCInitializeFromSource = H265FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern HFCSpecified :: H265FramerateControl
pattern HFCSpecified = H265FramerateControl' "SPECIFIED"

{-# COMPLETE
  HFCInitializeFromSource,
  HFCSpecified,
  H265FramerateControl' #-}

instance FromText H265FramerateControl where
    parser = (H265FramerateControl' . mk) <$> takeText

instance ToText H265FramerateControl where
    toText (H265FramerateControl' ci) = original ci

-- | Represents an enum of /known/ $H265FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265FramerateControl where
    toEnum i = case i of
        0 -> HFCInitializeFromSource
        1 -> HFCSpecified
        _ -> (error . showText) $ "Unknown index for H265FramerateControl: " <> toText i
    fromEnum x = case x of
        HFCInitializeFromSource -> 0
        HFCSpecified -> 1
        H265FramerateControl' name -> (error . showText) $ "Unknown H265FramerateControl: " <> original name

-- | Represents the bounds of /known/ $H265FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265FramerateControl where
    minBound = HFCInitializeFromSource
    maxBound = HFCSpecified

instance Hashable     H265FramerateControl
instance NFData       H265FramerateControl
instance ToByteString H265FramerateControl
instance ToQuery      H265FramerateControl
instance ToHeader     H265FramerateControl

instance ToJSON H265FramerateControl where
    toJSON = toJSONText

instance FromJSON H265FramerateControl where
    parseJSON = parseJSONText "H265FramerateControl"
