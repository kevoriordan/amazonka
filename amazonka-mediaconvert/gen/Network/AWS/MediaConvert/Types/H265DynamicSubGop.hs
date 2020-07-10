{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265DynamicSubGop
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265DynamicSubGop (
  H265DynamicSubGop (
    ..
    , Adaptive
    , Static
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose Adaptive to improve subjective video quality for high-motion content. This will cause the service to use fewer B-frames (which infer information based on other frames) for high-motion portions of the video and more B-frames for low-motion portions. The maximum number of B-frames is limited by the value you provide for the setting B frames between reference frames (numberBFramesBetweenReferenceFrames).
data H265DynamicSubGop = H265DynamicSubGop' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Adaptive :: H265DynamicSubGop
pattern Adaptive = H265DynamicSubGop' "ADAPTIVE"

pattern Static :: H265DynamicSubGop
pattern Static = H265DynamicSubGop' "STATIC"

{-# COMPLETE
  Adaptive,
  Static,
  H265DynamicSubGop' #-}

instance FromText H265DynamicSubGop where
    parser = (H265DynamicSubGop' . mk) <$> takeText

instance ToText H265DynamicSubGop where
    toText (H265DynamicSubGop' ci) = original ci

-- | Represents an enum of /known/ $H265DynamicSubGop.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265DynamicSubGop where
    toEnum i = case i of
        0 -> Adaptive
        1 -> Static
        _ -> (error . showText) $ "Unknown index for H265DynamicSubGop: " <> toText i
    fromEnum x = case x of
        Adaptive -> 0
        Static -> 1
        H265DynamicSubGop' name -> (error . showText) $ "Unknown H265DynamicSubGop: " <> original name

-- | Represents the bounds of /known/ $H265DynamicSubGop.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265DynamicSubGop where
    minBound = Adaptive
    maxBound = Static

instance Hashable     H265DynamicSubGop
instance NFData       H265DynamicSubGop
instance ToByteString H265DynamicSubGop
instance ToQuery      H265DynamicSubGop
instance ToHeader     H265DynamicSubGop

instance ToJSON H265DynamicSubGop where
    toJSON = toJSONText

instance FromJSON H265DynamicSubGop where
    parseJSON = parseJSONText "H265DynamicSubGop"
