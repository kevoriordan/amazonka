{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.Platform
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.Platform (
  Platform (
    ..
    , Android
    , Javascript
    , Linux
    , OSx
    , Objc
    , Swift
    , Windows
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Developer desktop or target mobile app or website platform. 
--
--
data Platform = Platform' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Android :: Platform
pattern Android = Platform' "ANDROID"

pattern Javascript :: Platform
pattern Javascript = Platform' "JAVASCRIPT"

pattern Linux :: Platform
pattern Linux = Platform' "LINUX"

pattern OSx :: Platform
pattern OSx = Platform' "OSX"

pattern Objc :: Platform
pattern Objc = Platform' "OBJC"

pattern Swift :: Platform
pattern Swift = Platform' "SWIFT"

pattern Windows :: Platform
pattern Windows = Platform' "WINDOWS"

{-# COMPLETE
  Android,
  Javascript,
  Linux,
  OSx,
  Objc,
  Swift,
  Windows,
  Platform' #-}

instance FromText Platform where
    parser = (Platform' . mk) <$> takeText

instance ToText Platform where
    toText (Platform' ci) = original ci

-- | Represents an enum of /known/ $Platform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Platform where
    toEnum i = case i of
        0 -> Android
        1 -> Javascript
        2 -> Linux
        3 -> OSx
        4 -> Objc
        5 -> Swift
        6 -> Windows
        _ -> (error . showText) $ "Unknown index for Platform: " <> toText i
    fromEnum x = case x of
        Android -> 0
        Javascript -> 1
        Linux -> 2
        OSx -> 3
        Objc -> 4
        Swift -> 5
        Windows -> 6
        Platform' name -> (error . showText) $ "Unknown Platform: " <> original name

-- | Represents the bounds of /known/ $Platform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Platform where
    minBound = Android
    maxBound = Windows

instance Hashable     Platform
instance NFData       Platform
instance ToByteString Platform
instance ToQuery      Platform
instance ToHeader     Platform

instance ToJSON Platform where
    toJSON = toJSONText

instance FromJSON Platform where
    parseJSON = parseJSONText "Platform"
