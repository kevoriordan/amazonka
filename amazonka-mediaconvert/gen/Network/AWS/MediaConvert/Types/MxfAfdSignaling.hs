{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfAfdSignaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MxfAfdSignaling (
  MxfAfdSignaling (
    ..
    , CopyFromVideo
    , NoCopy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
data MxfAfdSignaling = MxfAfdSignaling' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CopyFromVideo :: MxfAfdSignaling
pattern CopyFromVideo = MxfAfdSignaling' "COPY_FROM_VIDEO"

pattern NoCopy :: MxfAfdSignaling
pattern NoCopy = MxfAfdSignaling' "NO_COPY"

{-# COMPLETE
  CopyFromVideo,
  NoCopy,
  MxfAfdSignaling' #-}

instance FromText MxfAfdSignaling where
    parser = (MxfAfdSignaling' . mk) <$> takeText

instance ToText MxfAfdSignaling where
    toText (MxfAfdSignaling' ci) = original ci

-- | Represents an enum of /known/ $MxfAfdSignaling.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MxfAfdSignaling where
    toEnum i = case i of
        0 -> CopyFromVideo
        1 -> NoCopy
        _ -> (error . showText) $ "Unknown index for MxfAfdSignaling: " <> toText i
    fromEnum x = case x of
        CopyFromVideo -> 0
        NoCopy -> 1
        MxfAfdSignaling' name -> (error . showText) $ "Unknown MxfAfdSignaling: " <> original name

-- | Represents the bounds of /known/ $MxfAfdSignaling.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MxfAfdSignaling where
    minBound = CopyFromVideo
    maxBound = NoCopy

instance Hashable     MxfAfdSignaling
instance NFData       MxfAfdSignaling
instance ToByteString MxfAfdSignaling
instance ToQuery      MxfAfdSignaling
instance ToHeader     MxfAfdSignaling

instance ToJSON MxfAfdSignaling where
    toJSON = toJSONText

instance FromJSON MxfAfdSignaling where
    parseJSON = parseJSONText "MxfAfdSignaling"
