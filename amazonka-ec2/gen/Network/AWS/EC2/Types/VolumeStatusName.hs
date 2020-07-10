{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeStatusName (
  VolumeStatusName (
    ..
    , IOEnabled
    , IOPerformance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeStatusName = VolumeStatusName' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern IOEnabled :: VolumeStatusName
pattern IOEnabled = VolumeStatusName' "io-enabled"

pattern IOPerformance :: VolumeStatusName
pattern IOPerformance = VolumeStatusName' "io-performance"

{-# COMPLETE
  IOEnabled,
  IOPerformance,
  VolumeStatusName' #-}

instance FromText VolumeStatusName where
    parser = (VolumeStatusName' . mk) <$> takeText

instance ToText VolumeStatusName where
    toText (VolumeStatusName' ci) = original ci

-- | Represents an enum of /known/ $VolumeStatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VolumeStatusName where
    toEnum i = case i of
        0 -> IOEnabled
        1 -> IOPerformance
        _ -> (error . showText) $ "Unknown index for VolumeStatusName: " <> toText i
    fromEnum x = case x of
        IOEnabled -> 0
        IOPerformance -> 1
        VolumeStatusName' name -> (error . showText) $ "Unknown VolumeStatusName: " <> original name

-- | Represents the bounds of /known/ $VolumeStatusName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VolumeStatusName where
    minBound = IOEnabled
    maxBound = IOPerformance

instance Hashable     VolumeStatusName
instance NFData       VolumeStatusName
instance ToByteString VolumeStatusName
instance ToQuery      VolumeStatusName
instance ToHeader     VolumeStatusName

instance FromXML VolumeStatusName where
    parseXML = parseXMLText "VolumeStatusName"
