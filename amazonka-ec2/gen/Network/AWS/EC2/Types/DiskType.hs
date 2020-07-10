{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskType (
  DiskType (
    ..
    , Hdd
    , Ssd
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DiskType = DiskType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Hdd :: DiskType
pattern Hdd = DiskType' "hdd"

pattern Ssd :: DiskType
pattern Ssd = DiskType' "ssd"

{-# COMPLETE
  Hdd,
  Ssd,
  DiskType' #-}

instance FromText DiskType where
    parser = (DiskType' . mk) <$> takeText

instance ToText DiskType where
    toText (DiskType' ci) = original ci

-- | Represents an enum of /known/ $DiskType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DiskType where
    toEnum i = case i of
        0 -> Hdd
        1 -> Ssd
        _ -> (error . showText) $ "Unknown index for DiskType: " <> toText i
    fromEnum x = case x of
        Hdd -> 0
        Ssd -> 1
        DiskType' name -> (error . showText) $ "Unknown DiskType: " <> original name

-- | Represents the bounds of /known/ $DiskType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DiskType where
    minBound = Hdd
    maxBound = Ssd

instance Hashable     DiskType
instance NFData       DiskType
instance ToByteString DiskType
instance ToQuery      DiskType
instance ToHeader     DiskType

instance FromXML DiskType where
    parseXML = parseXMLText "DiskType"
