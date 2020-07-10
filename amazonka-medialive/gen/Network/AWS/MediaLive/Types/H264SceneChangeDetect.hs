{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264SceneChangeDetect (
  H264SceneChangeDetect (
    ..
    , HDisabled
    , HEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Scene Change Detect
data H264SceneChangeDetect = H264SceneChangeDetect' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern HDisabled :: H264SceneChangeDetect
pattern HDisabled = H264SceneChangeDetect' "DISABLED"

pattern HEnabled :: H264SceneChangeDetect
pattern HEnabled = H264SceneChangeDetect' "ENABLED"

{-# COMPLETE
  HDisabled,
  HEnabled,
  H264SceneChangeDetect' #-}

instance FromText H264SceneChangeDetect where
    parser = (H264SceneChangeDetect' . mk) <$> takeText

instance ToText H264SceneChangeDetect where
    toText (H264SceneChangeDetect' ci) = original ci

-- | Represents an enum of /known/ $H264SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264SceneChangeDetect where
    toEnum i = case i of
        0 -> HDisabled
        1 -> HEnabled
        _ -> (error . showText) $ "Unknown index for H264SceneChangeDetect: " <> toText i
    fromEnum x = case x of
        HDisabled -> 0
        HEnabled -> 1
        H264SceneChangeDetect' name -> (error . showText) $ "Unknown H264SceneChangeDetect: " <> original name

-- | Represents the bounds of /known/ $H264SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264SceneChangeDetect where
    minBound = HDisabled
    maxBound = HEnabled

instance Hashable     H264SceneChangeDetect
instance NFData       H264SceneChangeDetect
instance ToByteString H264SceneChangeDetect
instance ToQuery      H264SceneChangeDetect
instance ToHeader     H264SceneChangeDetect

instance ToJSON H264SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H264SceneChangeDetect where
    parseJSON = parseJSONText "H264SceneChangeDetect"
