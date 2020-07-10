{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstancePlatform
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstancePlatform (
  InstancePlatform (
    ..
    , LinuxUnix
    , Windows
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstancePlatform = InstancePlatform' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern LinuxUnix :: InstancePlatform
pattern LinuxUnix = InstancePlatform' "LINUX_UNIX"

pattern Windows :: InstancePlatform
pattern Windows = InstancePlatform' "WINDOWS"

{-# COMPLETE
  LinuxUnix,
  Windows,
  InstancePlatform' #-}

instance FromText InstancePlatform where
    parser = (InstancePlatform' . mk) <$> takeText

instance ToText InstancePlatform where
    toText (InstancePlatform' ci) = original ci

-- | Represents an enum of /known/ $InstancePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstancePlatform where
    toEnum i = case i of
        0 -> LinuxUnix
        1 -> Windows
        _ -> (error . showText) $ "Unknown index for InstancePlatform: " <> toText i
    fromEnum x = case x of
        LinuxUnix -> 0
        Windows -> 1
        InstancePlatform' name -> (error . showText) $ "Unknown InstancePlatform: " <> original name

-- | Represents the bounds of /known/ $InstancePlatform.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstancePlatform where
    minBound = LinuxUnix
    maxBound = Windows

instance Hashable     InstancePlatform
instance NFData       InstancePlatform
instance ToByteString InstancePlatform
instance ToQuery      InstancePlatform
instance ToHeader     InstancePlatform

instance FromJSON InstancePlatform where
    parseJSON = parseJSONText "InstancePlatform"
