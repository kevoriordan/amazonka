{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpace
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoSelectorColorSpace (
  VideoSelectorColorSpace (
    ..
    , Follow
    , Rec601
    , Rec709
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Video Selector Color Space
data VideoSelectorColorSpace = VideoSelectorColorSpace' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Follow :: VideoSelectorColorSpace
pattern Follow = VideoSelectorColorSpace' "FOLLOW"

pattern Rec601 :: VideoSelectorColorSpace
pattern Rec601 = VideoSelectorColorSpace' "REC_601"

pattern Rec709 :: VideoSelectorColorSpace
pattern Rec709 = VideoSelectorColorSpace' "REC_709"

{-# COMPLETE
  Follow,
  Rec601,
  Rec709,
  VideoSelectorColorSpace' #-}

instance FromText VideoSelectorColorSpace where
    parser = (VideoSelectorColorSpace' . mk) <$> takeText

instance ToText VideoSelectorColorSpace where
    toText (VideoSelectorColorSpace' ci) = original ci

-- | Represents an enum of /known/ $VideoSelectorColorSpace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VideoSelectorColorSpace where
    toEnum i = case i of
        0 -> Follow
        1 -> Rec601
        2 -> Rec709
        _ -> (error . showText) $ "Unknown index for VideoSelectorColorSpace: " <> toText i
    fromEnum x = case x of
        Follow -> 0
        Rec601 -> 1
        Rec709 -> 2
        VideoSelectorColorSpace' name -> (error . showText) $ "Unknown VideoSelectorColorSpace: " <> original name

-- | Represents the bounds of /known/ $VideoSelectorColorSpace.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VideoSelectorColorSpace where
    minBound = Follow
    maxBound = Rec709

instance Hashable     VideoSelectorColorSpace
instance NFData       VideoSelectorColorSpace
instance ToByteString VideoSelectorColorSpace
instance ToQuery      VideoSelectorColorSpace
instance ToHeader     VideoSelectorColorSpace

instance ToJSON VideoSelectorColorSpace where
    toJSON = toJSONText

instance FromJSON VideoSelectorColorSpace where
    parseJSON = parseJSONText "VideoSelectorColorSpace"
