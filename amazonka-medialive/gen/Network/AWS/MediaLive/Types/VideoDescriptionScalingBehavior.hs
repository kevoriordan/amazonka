{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior (
  VideoDescriptionScalingBehavior (
    ..
    , Default
    , StretchToOutput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Video Description Scaling Behavior
data VideoDescriptionScalingBehavior = VideoDescriptionScalingBehavior' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern Default :: VideoDescriptionScalingBehavior
pattern Default = VideoDescriptionScalingBehavior' "DEFAULT"

pattern StretchToOutput :: VideoDescriptionScalingBehavior
pattern StretchToOutput = VideoDescriptionScalingBehavior' "STRETCH_TO_OUTPUT"

{-# COMPLETE
  Default,
  StretchToOutput,
  VideoDescriptionScalingBehavior' #-}

instance FromText VideoDescriptionScalingBehavior where
    parser = (VideoDescriptionScalingBehavior' . mk) <$> takeText

instance ToText VideoDescriptionScalingBehavior where
    toText (VideoDescriptionScalingBehavior' ci) = original ci

-- | Represents an enum of /known/ $VideoDescriptionScalingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VideoDescriptionScalingBehavior where
    toEnum i = case i of
        0 -> Default
        1 -> StretchToOutput
        _ -> (error . showText) $ "Unknown index for VideoDescriptionScalingBehavior: " <> toText i
    fromEnum x = case x of
        Default -> 0
        StretchToOutput -> 1
        VideoDescriptionScalingBehavior' name -> (error . showText) $ "Unknown VideoDescriptionScalingBehavior: " <> original name

-- | Represents the bounds of /known/ $VideoDescriptionScalingBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VideoDescriptionScalingBehavior where
    minBound = Default
    maxBound = StretchToOutput

instance Hashable     VideoDescriptionScalingBehavior
instance NFData       VideoDescriptionScalingBehavior
instance ToByteString VideoDescriptionScalingBehavior
instance ToQuery      VideoDescriptionScalingBehavior
instance ToHeader     VideoDescriptionScalingBehavior

instance ToJSON VideoDescriptionScalingBehavior where
    toJSON = toJSONText

instance FromJSON VideoDescriptionScalingBehavior where
    parseJSON = parseJSONText "VideoDescriptionScalingBehavior"
