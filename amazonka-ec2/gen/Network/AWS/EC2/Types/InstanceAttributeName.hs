{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceAttributeName (
  InstanceAttributeName (
    ..
    , IANBlockDeviceMapping
    , IANDisableAPITermination
    , IANEBSOptimized
    , IANEnaSupport
    , IANGroupSet
    , IANInstanceInitiatedShutdownBehavior
    , IANInstanceType
    , IANKernel
    , IANProductCodes
    , IANRAMDisk
    , IANRootDeviceName
    , IANSRIOVNetSupport
    , IANSourceDestCheck
    , IANUserData
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data InstanceAttributeName = InstanceAttributeName' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern IANBlockDeviceMapping :: InstanceAttributeName
pattern IANBlockDeviceMapping = InstanceAttributeName' "blockDeviceMapping"

pattern IANDisableAPITermination :: InstanceAttributeName
pattern IANDisableAPITermination = InstanceAttributeName' "disableApiTermination"

pattern IANEBSOptimized :: InstanceAttributeName
pattern IANEBSOptimized = InstanceAttributeName' "ebsOptimized"

pattern IANEnaSupport :: InstanceAttributeName
pattern IANEnaSupport = InstanceAttributeName' "enaSupport"

pattern IANGroupSet :: InstanceAttributeName
pattern IANGroupSet = InstanceAttributeName' "groupSet"

pattern IANInstanceInitiatedShutdownBehavior :: InstanceAttributeName
pattern IANInstanceInitiatedShutdownBehavior = InstanceAttributeName' "instanceInitiatedShutdownBehavior"

pattern IANInstanceType :: InstanceAttributeName
pattern IANInstanceType = InstanceAttributeName' "instanceType"

pattern IANKernel :: InstanceAttributeName
pattern IANKernel = InstanceAttributeName' "kernel"

pattern IANProductCodes :: InstanceAttributeName
pattern IANProductCodes = InstanceAttributeName' "productCodes"

pattern IANRAMDisk :: InstanceAttributeName
pattern IANRAMDisk = InstanceAttributeName' "ramdisk"

pattern IANRootDeviceName :: InstanceAttributeName
pattern IANRootDeviceName = InstanceAttributeName' "rootDeviceName"

pattern IANSRIOVNetSupport :: InstanceAttributeName
pattern IANSRIOVNetSupport = InstanceAttributeName' "sriovNetSupport"

pattern IANSourceDestCheck :: InstanceAttributeName
pattern IANSourceDestCheck = InstanceAttributeName' "sourceDestCheck"

pattern IANUserData :: InstanceAttributeName
pattern IANUserData = InstanceAttributeName' "userData"

{-# COMPLETE
  IANBlockDeviceMapping,
  IANDisableAPITermination,
  IANEBSOptimized,
  IANEnaSupport,
  IANGroupSet,
  IANInstanceInitiatedShutdownBehavior,
  IANInstanceType,
  IANKernel,
  IANProductCodes,
  IANRAMDisk,
  IANRootDeviceName,
  IANSRIOVNetSupport,
  IANSourceDestCheck,
  IANUserData,
  InstanceAttributeName' #-}

instance FromText InstanceAttributeName where
    parser = (InstanceAttributeName' . mk) <$> takeText

instance ToText InstanceAttributeName where
    toText (InstanceAttributeName' ci) = original ci

-- | Represents an enum of /known/ $InstanceAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceAttributeName where
    toEnum i = case i of
        0 -> IANBlockDeviceMapping
        1 -> IANDisableAPITermination
        2 -> IANEBSOptimized
        3 -> IANEnaSupport
        4 -> IANGroupSet
        5 -> IANInstanceInitiatedShutdownBehavior
        6 -> IANInstanceType
        7 -> IANKernel
        8 -> IANProductCodes
        9 -> IANRAMDisk
        10 -> IANRootDeviceName
        11 -> IANSRIOVNetSupport
        12 -> IANSourceDestCheck
        13 -> IANUserData
        _ -> (error . showText) $ "Unknown index for InstanceAttributeName: " <> toText i
    fromEnum x = case x of
        IANBlockDeviceMapping -> 0
        IANDisableAPITermination -> 1
        IANEBSOptimized -> 2
        IANEnaSupport -> 3
        IANGroupSet -> 4
        IANInstanceInitiatedShutdownBehavior -> 5
        IANInstanceType -> 6
        IANKernel -> 7
        IANProductCodes -> 8
        IANRAMDisk -> 9
        IANRootDeviceName -> 10
        IANSRIOVNetSupport -> 11
        IANSourceDestCheck -> 12
        IANUserData -> 13
        InstanceAttributeName' name -> (error . showText) $ "Unknown InstanceAttributeName: " <> original name

-- | Represents the bounds of /known/ $InstanceAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceAttributeName where
    minBound = IANBlockDeviceMapping
    maxBound = IANUserData

instance Hashable     InstanceAttributeName
instance NFData       InstanceAttributeName
instance ToByteString InstanceAttributeName
instance ToQuery      InstanceAttributeName
instance ToHeader     InstanceAttributeName
