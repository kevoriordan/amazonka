{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeType (
  VolumeType (
    ..
    , GP2
    , IO1
    , SC1
    , ST1
    , Standard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeType = VolumeType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern GP2 :: VolumeType
pattern GP2 = VolumeType' "gp2"

pattern IO1 :: VolumeType
pattern IO1 = VolumeType' "io1"

pattern SC1 :: VolumeType
pattern SC1 = VolumeType' "sc1"

pattern ST1 :: VolumeType
pattern ST1 = VolumeType' "st1"

pattern Standard :: VolumeType
pattern Standard = VolumeType' "standard"

{-# COMPLETE
  GP2,
  IO1,
  SC1,
  ST1,
  Standard,
  VolumeType' #-}

instance FromText VolumeType where
    parser = (VolumeType' . mk) <$> takeText

instance ToText VolumeType where
    toText (VolumeType' ci) = original ci

-- | Represents an enum of /known/ $VolumeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeType where
    toEnum i = case i of
        0 -> GP2
        1 -> IO1
        2 -> SC1
        3 -> ST1
        4 -> Standard
        _ -> (error . showText) $ "Unknown index for VolumeType: " <> toText i
    fromEnum x = case x of
        GP2 -> 0
        IO1 -> 1
        SC1 -> 2
        ST1 -> 3
        Standard -> 4
        VolumeType' name -> (error . showText) $ "Unknown VolumeType: " <> original name

-- | Represents the bounds of /known/ $VolumeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeType where
    minBound = GP2
    maxBound = Standard

instance Hashable     VolumeType
instance NFData       VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance FromXML VolumeType where
    parseXML = parseXMLText "VolumeType"
