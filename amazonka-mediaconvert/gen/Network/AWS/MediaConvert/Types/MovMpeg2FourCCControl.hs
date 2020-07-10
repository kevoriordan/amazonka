{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl (
  MovMpeg2FourCCControl (
    ..
    , Mpeg
    , Xdcam
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
data MovMpeg2FourCCControl = MovMpeg2FourCCControl' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Mpeg :: MovMpeg2FourCCControl
pattern Mpeg = MovMpeg2FourCCControl' "MPEG"

pattern Xdcam :: MovMpeg2FourCCControl
pattern Xdcam = MovMpeg2FourCCControl' "XDCAM"

{-# COMPLETE
  Mpeg,
  Xdcam,
  MovMpeg2FourCCControl' #-}

instance FromText MovMpeg2FourCCControl where
    parser = (MovMpeg2FourCCControl' . mk) <$> takeText

instance ToText MovMpeg2FourCCControl where
    toText (MovMpeg2FourCCControl' ci) = original ci

-- | Represents an enum of /known/ $MovMpeg2FourCCControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MovMpeg2FourCCControl where
    toEnum i = case i of
        0 -> Mpeg
        1 -> Xdcam
        _ -> (error . showText) $ "Unknown index for MovMpeg2FourCCControl: " <> toText i
    fromEnum x = case x of
        Mpeg -> 0
        Xdcam -> 1
        MovMpeg2FourCCControl' name -> (error . showText) $ "Unknown MovMpeg2FourCCControl: " <> original name

-- | Represents the bounds of /known/ $MovMpeg2FourCCControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MovMpeg2FourCCControl where
    minBound = Mpeg
    maxBound = Xdcam

instance Hashable     MovMpeg2FourCCControl
instance NFData       MovMpeg2FourCCControl
instance ToByteString MovMpeg2FourCCControl
instance ToQuery      MovMpeg2FourCCControl
instance ToHeader     MovMpeg2FourCCControl

instance ToJSON MovMpeg2FourCCControl where
    toJSON = toJSONText

instance FromJSON MovMpeg2FourCCControl where
    parseJSON = parseJSONText "MovMpeg2FourCCControl"
