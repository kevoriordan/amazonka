{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.EnvironmentType (
  EnvironmentType (
    ..
    , ArmContainer
    , LinuxContainer
    , LinuxGpuContainer
    , WindowsContainer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EnvironmentType = EnvironmentType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ArmContainer :: EnvironmentType
pattern ArmContainer = EnvironmentType' "ARM_CONTAINER"

pattern LinuxContainer :: EnvironmentType
pattern LinuxContainer = EnvironmentType' "LINUX_CONTAINER"

pattern LinuxGpuContainer :: EnvironmentType
pattern LinuxGpuContainer = EnvironmentType' "LINUX_GPU_CONTAINER"

pattern WindowsContainer :: EnvironmentType
pattern WindowsContainer = EnvironmentType' "WINDOWS_CONTAINER"

{-# COMPLETE
  ArmContainer,
  LinuxContainer,
  LinuxGpuContainer,
  WindowsContainer,
  EnvironmentType' #-}

instance FromText EnvironmentType where
    parser = (EnvironmentType' . mk) <$> takeText

instance ToText EnvironmentType where
    toText (EnvironmentType' ci) = original ci

-- | Represents an enum of /known/ $EnvironmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EnvironmentType where
    toEnum i = case i of
        0 -> ArmContainer
        1 -> LinuxContainer
        2 -> LinuxGpuContainer
        3 -> WindowsContainer
        _ -> (error . showText) $ "Unknown index for EnvironmentType: " <> toText i
    fromEnum x = case x of
        ArmContainer -> 0
        LinuxContainer -> 1
        LinuxGpuContainer -> 2
        WindowsContainer -> 3
        EnvironmentType' name -> (error . showText) $ "Unknown EnvironmentType: " <> original name

-- | Represents the bounds of /known/ $EnvironmentType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EnvironmentType where
    minBound = ArmContainer
    maxBound = WindowsContainer

instance Hashable     EnvironmentType
instance NFData       EnvironmentType
instance ToByteString EnvironmentType
instance ToQuery      EnvironmentType
instance ToHeader     EnvironmentType

instance ToJSON EnvironmentType where
    toJSON = toJSONText

instance FromJSON EnvironmentType where
    parseJSON = parseJSONText "EnvironmentType"
