{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskImageFormat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskImageFormat (
  DiskImageFormat (
    ..
    , Raw
    , VHD
    , VMDK
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DiskImageFormat = DiskImageFormat' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Raw :: DiskImageFormat
pattern Raw = DiskImageFormat' "RAW"

pattern VHD :: DiskImageFormat
pattern VHD = DiskImageFormat' "VHD"

pattern VMDK :: DiskImageFormat
pattern VMDK = DiskImageFormat' "VMDK"

{-# COMPLETE
  Raw,
  VHD,
  VMDK,
  DiskImageFormat' #-}

instance FromText DiskImageFormat where
    parser = (DiskImageFormat' . mk) <$> takeText

instance ToText DiskImageFormat where
    toText (DiskImageFormat' ci) = original ci

-- | Represents an enum of /known/ $DiskImageFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DiskImageFormat where
    toEnum i = case i of
        0 -> Raw
        1 -> VHD
        2 -> VMDK
        _ -> (error . showText) $ "Unknown index for DiskImageFormat: " <> toText i
    fromEnum x = case x of
        Raw -> 0
        VHD -> 1
        VMDK -> 2
        DiskImageFormat' name -> (error . showText) $ "Unknown DiskImageFormat: " <> original name

-- | Represents the bounds of /known/ $DiskImageFormat.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DiskImageFormat where
    minBound = Raw
    maxBound = VMDK

instance Hashable     DiskImageFormat
instance NFData       DiskImageFormat
instance ToByteString DiskImageFormat
instance ToQuery      DiskImageFormat
instance ToHeader     DiskImageFormat

instance FromXML DiskImageFormat where
    parseXML = parseXMLText "DiskImageFormat"
