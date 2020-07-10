{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImageAttributeName (
  ImageAttributeName (
    ..
    , BlockDeviceMapping
    , Description
    , Kernel
    , LaunchPermission
    , ProductCodes
    , RAMDisk
    , SRIOVNetSupport
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageAttributeName = ImageAttributeName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern BlockDeviceMapping :: ImageAttributeName
pattern BlockDeviceMapping = ImageAttributeName' "blockDeviceMapping"

pattern Description :: ImageAttributeName
pattern Description = ImageAttributeName' "description"

pattern Kernel :: ImageAttributeName
pattern Kernel = ImageAttributeName' "kernel"

pattern LaunchPermission :: ImageAttributeName
pattern LaunchPermission = ImageAttributeName' "launchPermission"

pattern ProductCodes :: ImageAttributeName
pattern ProductCodes = ImageAttributeName' "productCodes"

pattern RAMDisk :: ImageAttributeName
pattern RAMDisk = ImageAttributeName' "ramdisk"

pattern SRIOVNetSupport :: ImageAttributeName
pattern SRIOVNetSupport = ImageAttributeName' "sriovNetSupport"

{-# COMPLETE
  BlockDeviceMapping,
  Description,
  Kernel,
  LaunchPermission,
  ProductCodes,
  RAMDisk,
  SRIOVNetSupport,
  ImageAttributeName' #-}

instance FromText ImageAttributeName where
    parser = (ImageAttributeName' . mk) <$> takeText

instance ToText ImageAttributeName where
    toText (ImageAttributeName' ci) = original ci

-- | Represents an enum of /known/ $ImageAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageAttributeName where
    toEnum i = case i of
        0 -> BlockDeviceMapping
        1 -> Description
        2 -> Kernel
        3 -> LaunchPermission
        4 -> ProductCodes
        5 -> RAMDisk
        6 -> SRIOVNetSupport
        _ -> (error . showText) $ "Unknown index for ImageAttributeName: " <> toText i
    fromEnum x = case x of
        BlockDeviceMapping -> 0
        Description -> 1
        Kernel -> 2
        LaunchPermission -> 3
        ProductCodes -> 4
        RAMDisk -> 5
        SRIOVNetSupport -> 6
        ImageAttributeName' name -> (error . showText) $ "Unknown ImageAttributeName: " <> original name

-- | Represents the bounds of /known/ $ImageAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageAttributeName where
    minBound = BlockDeviceMapping
    maxBound = SRIOVNetSupport

instance Hashable     ImageAttributeName
instance NFData       ImageAttributeName
instance ToByteString ImageAttributeName
instance ToQuery      ImageAttributeName
instance ToHeader     ImageAttributeName
