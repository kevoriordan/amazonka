{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImageTypeValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImageTypeValues (
  ImageTypeValues (
    ..
    , ITVKernel
    , ITVMachine
    , ITVRAMDisk
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ImageTypeValues = ImageTypeValues' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ITVKernel :: ImageTypeValues
pattern ITVKernel = ImageTypeValues' "kernel"

pattern ITVMachine :: ImageTypeValues
pattern ITVMachine = ImageTypeValues' "machine"

pattern ITVRAMDisk :: ImageTypeValues
pattern ITVRAMDisk = ImageTypeValues' "ramdisk"

{-# COMPLETE
  ITVKernel,
  ITVMachine,
  ITVRAMDisk,
  ImageTypeValues' #-}

instance FromText ImageTypeValues where
    parser = (ImageTypeValues' . mk) <$> takeText

instance ToText ImageTypeValues where
    toText (ImageTypeValues' ci) = original ci

-- | Represents an enum of /known/ $ImageTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ImageTypeValues where
    toEnum i = case i of
        0 -> ITVKernel
        1 -> ITVMachine
        2 -> ITVRAMDisk
        _ -> (error . showText) $ "Unknown index for ImageTypeValues: " <> toText i
    fromEnum x = case x of
        ITVKernel -> 0
        ITVMachine -> 1
        ITVRAMDisk -> 2
        ImageTypeValues' name -> (error . showText) $ "Unknown ImageTypeValues: " <> original name

-- | Represents the bounds of /known/ $ImageTypeValues.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ImageTypeValues where
    minBound = ITVKernel
    maxBound = ITVRAMDisk

instance Hashable     ImageTypeValues
instance NFData       ImageTypeValues
instance ToByteString ImageTypeValues
instance ToQuery      ImageTypeValues
instance ToHeader     ImageTypeValues

instance FromXML ImageTypeValues where
    parseXML = parseXMLText "ImageTypeValues"
