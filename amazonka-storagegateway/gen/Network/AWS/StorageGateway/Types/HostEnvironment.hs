{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.HostEnvironment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.HostEnvironment (
  HostEnvironment (
    ..
    , EC2
    , HyperV
    , Kvm
    , Other
    , VMware
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HostEnvironment = HostEnvironment' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern EC2 :: HostEnvironment
pattern EC2 = HostEnvironment' "EC2"

pattern HyperV :: HostEnvironment
pattern HyperV = HostEnvironment' "HYPER-V"

pattern Kvm :: HostEnvironment
pattern Kvm = HostEnvironment' "KVM"

pattern Other :: HostEnvironment
pattern Other = HostEnvironment' "OTHER"

pattern VMware :: HostEnvironment
pattern VMware = HostEnvironment' "VMWARE"

{-# COMPLETE
  EC2,
  HyperV,
  Kvm,
  Other,
  VMware,
  HostEnvironment' #-}

instance FromText HostEnvironment where
    parser = (HostEnvironment' . mk) <$> takeText

instance ToText HostEnvironment where
    toText (HostEnvironment' ci) = original ci

-- | Represents an enum of /known/ $HostEnvironment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HostEnvironment where
    toEnum i = case i of
        0 -> EC2
        1 -> HyperV
        2 -> Kvm
        3 -> Other
        4 -> VMware
        _ -> (error . showText) $ "Unknown index for HostEnvironment: " <> toText i
    fromEnum x = case x of
        EC2 -> 0
        HyperV -> 1
        Kvm -> 2
        Other -> 3
        VMware -> 4
        HostEnvironment' name -> (error . showText) $ "Unknown HostEnvironment: " <> original name

-- | Represents the bounds of /known/ $HostEnvironment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HostEnvironment where
    minBound = EC2
    maxBound = VMware

instance Hashable     HostEnvironment
instance NFData       HostEnvironment
instance ToByteString HostEnvironment
instance ToQuery      HostEnvironment
instance ToHeader     HostEnvironment

instance FromJSON HostEnvironment where
    parseJSON = parseJSONText "HostEnvironment"
