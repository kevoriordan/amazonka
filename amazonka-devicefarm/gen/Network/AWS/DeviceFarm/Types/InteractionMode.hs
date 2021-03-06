{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.InteractionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.InteractionMode (
  InteractionMode (
    ..
    , Interactive
    , NoVideo
    , VideoOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InteractionMode = InteractionMode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Interactive :: InteractionMode
pattern Interactive = InteractionMode' "INTERACTIVE"

pattern NoVideo :: InteractionMode
pattern NoVideo = InteractionMode' "NO_VIDEO"

pattern VideoOnly :: InteractionMode
pattern VideoOnly = InteractionMode' "VIDEO_ONLY"

{-# COMPLETE
  Interactive,
  NoVideo,
  VideoOnly,
  InteractionMode' #-}

instance FromText InteractionMode where
    parser = (InteractionMode' . mk) <$> takeText

instance ToText InteractionMode where
    toText (InteractionMode' ci) = original ci

-- | Represents an enum of /known/ $InteractionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InteractionMode where
    toEnum i = case i of
        0 -> Interactive
        1 -> NoVideo
        2 -> VideoOnly
        _ -> (error . showText) $ "Unknown index for InteractionMode: " <> toText i
    fromEnum x = case x of
        Interactive -> 0
        NoVideo -> 1
        VideoOnly -> 2
        InteractionMode' name -> (error . showText) $ "Unknown InteractionMode: " <> original name

-- | Represents the bounds of /known/ $InteractionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InteractionMode where
    minBound = Interactive
    maxBound = VideoOnly

instance Hashable     InteractionMode
instance NFData       InteractionMode
instance ToByteString InteractionMode
instance ToQuery      InteractionMode
instance ToHeader     InteractionMode

instance ToJSON InteractionMode where
    toJSON = toJSONText

instance FromJSON InteractionMode where
    parseJSON = parseJSONText "InteractionMode"
