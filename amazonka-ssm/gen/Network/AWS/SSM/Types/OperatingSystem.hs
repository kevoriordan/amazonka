{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OperatingSystem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OperatingSystem (
  OperatingSystem (
    ..
    , AmazonLinux
    , AmazonLinux2
    , Centos
    , RedhatEnterpriseLinux
    , Suse
    , Ubuntu
    , Windows
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperatingSystem = OperatingSystem' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AmazonLinux :: OperatingSystem
pattern AmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern AmazonLinux2 :: OperatingSystem
pattern AmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

pattern Centos :: OperatingSystem
pattern Centos = OperatingSystem' "CENTOS"

pattern RedhatEnterpriseLinux :: OperatingSystem
pattern RedhatEnterpriseLinux = OperatingSystem' "REDHAT_ENTERPRISE_LINUX"

pattern Suse :: OperatingSystem
pattern Suse = OperatingSystem' "SUSE"

pattern Ubuntu :: OperatingSystem
pattern Ubuntu = OperatingSystem' "UBUNTU"

pattern Windows :: OperatingSystem
pattern Windows = OperatingSystem' "WINDOWS"

{-# COMPLETE
  AmazonLinux,
  AmazonLinux2,
  Centos,
  RedhatEnterpriseLinux,
  Suse,
  Ubuntu,
  Windows,
  OperatingSystem' #-}

instance FromText OperatingSystem where
    parser = (OperatingSystem' . mk) <$> takeText

instance ToText OperatingSystem where
    toText (OperatingSystem' ci) = original ci

-- | Represents an enum of /known/ $OperatingSystem.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperatingSystem where
    toEnum i = case i of
        0 -> AmazonLinux
        1 -> AmazonLinux2
        2 -> Centos
        3 -> RedhatEnterpriseLinux
        4 -> Suse
        5 -> Ubuntu
        6 -> Windows
        _ -> (error . showText) $ "Unknown index for OperatingSystem: " <> toText i
    fromEnum x = case x of
        AmazonLinux -> 0
        AmazonLinux2 -> 1
        Centos -> 2
        RedhatEnterpriseLinux -> 3
        Suse -> 4
        Ubuntu -> 5
        Windows -> 6
        OperatingSystem' name -> (error . showText) $ "Unknown OperatingSystem: " <> original name

-- | Represents the bounds of /known/ $OperatingSystem.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperatingSystem where
    minBound = AmazonLinux
    maxBound = Windows

instance Hashable     OperatingSystem
instance NFData       OperatingSystem
instance ToByteString OperatingSystem
instance ToQuery      OperatingSystem
instance ToHeader     OperatingSystem

instance ToJSON OperatingSystem where
    toJSON = toJSONText

instance FromJSON OperatingSystem where
    parseJSON = parseJSONText "OperatingSystem"
