{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VPCAttributeName (
  VPCAttributeName (
    ..
    , EnableDNSHostnames
    , EnableDNSSupport
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCAttributeName = VPCAttributeName' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern EnableDNSHostnames :: VPCAttributeName
pattern EnableDNSHostnames = VPCAttributeName' "enableDnsHostnames"

pattern EnableDNSSupport :: VPCAttributeName
pattern EnableDNSSupport = VPCAttributeName' "enableDnsSupport"

{-# COMPLETE
  EnableDNSHostnames,
  EnableDNSSupport,
  VPCAttributeName' #-}

instance FromText VPCAttributeName where
    parser = (VPCAttributeName' . mk) <$> takeText

instance ToText VPCAttributeName where
    toText (VPCAttributeName' ci) = original ci

-- | Represents an enum of /known/ $VPCAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum VPCAttributeName where
    toEnum i = case i of
        0 -> EnableDNSHostnames
        1 -> EnableDNSSupport
        _ -> (error . showText) $ "Unknown index for VPCAttributeName: " <> toText i
    fromEnum x = case x of
        EnableDNSHostnames -> 0
        EnableDNSSupport -> 1
        VPCAttributeName' name -> (error . showText) $ "Unknown VPCAttributeName: " <> original name

-- | Represents the bounds of /known/ $VPCAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded VPCAttributeName where
    minBound = EnableDNSHostnames
    maxBound = EnableDNSSupport

instance Hashable     VPCAttributeName
instance NFData       VPCAttributeName
instance ToByteString VPCAttributeName
instance ToQuery      VPCAttributeName
instance ToHeader     VPCAttributeName
