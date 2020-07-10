{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect (
  Mpeg2SceneChangeDetect (
    ..
    , MSCDDisabled
    , MSCDEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
data Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern MSCDDisabled :: Mpeg2SceneChangeDetect
pattern MSCDDisabled = Mpeg2SceneChangeDetect' "DISABLED"

pattern MSCDEnabled :: Mpeg2SceneChangeDetect
pattern MSCDEnabled = Mpeg2SceneChangeDetect' "ENABLED"

{-# COMPLETE
  MSCDDisabled,
  MSCDEnabled,
  Mpeg2SceneChangeDetect' #-}

instance FromText Mpeg2SceneChangeDetect where
    parser = (Mpeg2SceneChangeDetect' . mk) <$> takeText

instance ToText Mpeg2SceneChangeDetect where
    toText (Mpeg2SceneChangeDetect' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2SceneChangeDetect where
    toEnum i = case i of
        0 -> MSCDDisabled
        1 -> MSCDEnabled
        _ -> (error . showText) $ "Unknown index for Mpeg2SceneChangeDetect: " <> toText i
    fromEnum x = case x of
        MSCDDisabled -> 0
        MSCDEnabled -> 1
        Mpeg2SceneChangeDetect' name -> (error . showText) $ "Unknown Mpeg2SceneChangeDetect: " <> original name

-- | Represents the bounds of /known/ $Mpeg2SceneChangeDetect.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2SceneChangeDetect where
    minBound = MSCDDisabled
    maxBound = MSCDEnabled

instance Hashable     Mpeg2SceneChangeDetect
instance NFData       Mpeg2SceneChangeDetect
instance ToByteString Mpeg2SceneChangeDetect
instance ToQuery      Mpeg2SceneChangeDetect
instance ToHeader     Mpeg2SceneChangeDetect

instance ToJSON Mpeg2SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON Mpeg2SceneChangeDetect where
    parseJSON = parseJSONText "Mpeg2SceneChangeDetect"
