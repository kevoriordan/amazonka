{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.PlatformType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.PlatformType (
  PlatformType (
    ..
    , AmazonLinux
    , Debian
    , Ubuntu
    , WindowsServer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PlatformType = PlatformType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AmazonLinux :: PlatformType
pattern AmazonLinux = PlatformType' "AMAZON_LINUX"

pattern Debian :: PlatformType
pattern Debian = PlatformType' "DEBIAN"

pattern Ubuntu :: PlatformType
pattern Ubuntu = PlatformType' "UBUNTU"

pattern WindowsServer :: PlatformType
pattern WindowsServer = PlatformType' "WINDOWS_SERVER"

{-# COMPLETE
  AmazonLinux,
  Debian,
  Ubuntu,
  WindowsServer,
  PlatformType' #-}

instance FromText PlatformType where
    parser = (PlatformType' . mk) <$> takeText

instance ToText PlatformType where
    toText (PlatformType' ci) = original ci

-- | Represents an enum of /known/ $PlatformType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PlatformType where
    toEnum i = case i of
        0 -> AmazonLinux
        1 -> Debian
        2 -> Ubuntu
        3 -> WindowsServer
        _ -> (error . showText) $ "Unknown index for PlatformType: " <> toText i
    fromEnum x = case x of
        AmazonLinux -> 0
        Debian -> 1
        Ubuntu -> 2
        WindowsServer -> 3
        PlatformType' name -> (error . showText) $ "Unknown PlatformType: " <> original name

-- | Represents the bounds of /known/ $PlatformType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PlatformType where
    minBound = AmazonLinux
    maxBound = WindowsServer

instance Hashable     PlatformType
instance NFData       PlatformType
instance ToByteString PlatformType
instance ToQuery      PlatformType
instance ToHeader     PlatformType

instance FromJSON PlatformType where
    parseJSON = parseJSONText "PlatformType"
