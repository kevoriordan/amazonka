{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfacePermissionStateCode (
  NetworkInterfacePermissionStateCode (
    ..
    , NIPSCGranted
    , NIPSCPending
    , NIPSCRevoked
    , NIPSCRevoking
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfacePermissionStateCode = NetworkInterfacePermissionStateCode' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern NIPSCGranted :: NetworkInterfacePermissionStateCode
pattern NIPSCGranted = NetworkInterfacePermissionStateCode' "granted"

pattern NIPSCPending :: NetworkInterfacePermissionStateCode
pattern NIPSCPending = NetworkInterfacePermissionStateCode' "pending"

pattern NIPSCRevoked :: NetworkInterfacePermissionStateCode
pattern NIPSCRevoked = NetworkInterfacePermissionStateCode' "revoked"

pattern NIPSCRevoking :: NetworkInterfacePermissionStateCode
pattern NIPSCRevoking = NetworkInterfacePermissionStateCode' "revoking"

{-# COMPLETE
  NIPSCGranted,
  NIPSCPending,
  NIPSCRevoked,
  NIPSCRevoking,
  NetworkInterfacePermissionStateCode' #-}

instance FromText NetworkInterfacePermissionStateCode where
    parser = (NetworkInterfacePermissionStateCode' . mk) <$> takeText

instance ToText NetworkInterfacePermissionStateCode where
    toText (NetworkInterfacePermissionStateCode' ci) = original ci

-- | Represents an enum of /known/ $NetworkInterfacePermissionStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkInterfacePermissionStateCode where
    toEnum i = case i of
        0 -> NIPSCGranted
        1 -> NIPSCPending
        2 -> NIPSCRevoked
        3 -> NIPSCRevoking
        _ -> (error . showText) $ "Unknown index for NetworkInterfacePermissionStateCode: " <> toText i
    fromEnum x = case x of
        NIPSCGranted -> 0
        NIPSCPending -> 1
        NIPSCRevoked -> 2
        NIPSCRevoking -> 3
        NetworkInterfacePermissionStateCode' name -> (error . showText) $ "Unknown NetworkInterfacePermissionStateCode: " <> original name

-- | Represents the bounds of /known/ $NetworkInterfacePermissionStateCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkInterfacePermissionStateCode where
    minBound = NIPSCGranted
    maxBound = NIPSCRevoking

instance Hashable     NetworkInterfacePermissionStateCode
instance NFData       NetworkInterfacePermissionStateCode
instance ToByteString NetworkInterfacePermissionStateCode
instance ToQuery      NetworkInterfacePermissionStateCode
instance ToHeader     NetworkInterfacePermissionStateCode

instance FromXML NetworkInterfacePermissionStateCode where
    parseXML = parseXMLText "NetworkInterfacePermissionStateCode"
