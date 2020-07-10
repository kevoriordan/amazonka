{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.LastUpdateStatusReasonCode (
  LastUpdateStatusReasonCode (
    ..
    , LUSRCEniLimitExceeded
    , LUSRCInsufficientRolePermissions
    , LUSRCInternalError
    , LUSRCInvalidConfiguration
    , LUSRCInvalidSecurityGroup
    , LUSRCInvalidSubnet
    , LUSRCSubnetOutOfIPAddresses
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LastUpdateStatusReasonCode = LastUpdateStatusReasonCode' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern LUSRCEniLimitExceeded :: LastUpdateStatusReasonCode
pattern LUSRCEniLimitExceeded = LastUpdateStatusReasonCode' "EniLimitExceeded"

pattern LUSRCInsufficientRolePermissions :: LastUpdateStatusReasonCode
pattern LUSRCInsufficientRolePermissions = LastUpdateStatusReasonCode' "InsufficientRolePermissions"

pattern LUSRCInternalError :: LastUpdateStatusReasonCode
pattern LUSRCInternalError = LastUpdateStatusReasonCode' "InternalError"

pattern LUSRCInvalidConfiguration :: LastUpdateStatusReasonCode
pattern LUSRCInvalidConfiguration = LastUpdateStatusReasonCode' "InvalidConfiguration"

pattern LUSRCInvalidSecurityGroup :: LastUpdateStatusReasonCode
pattern LUSRCInvalidSecurityGroup = LastUpdateStatusReasonCode' "InvalidSecurityGroup"

pattern LUSRCInvalidSubnet :: LastUpdateStatusReasonCode
pattern LUSRCInvalidSubnet = LastUpdateStatusReasonCode' "InvalidSubnet"

pattern LUSRCSubnetOutOfIPAddresses :: LastUpdateStatusReasonCode
pattern LUSRCSubnetOutOfIPAddresses = LastUpdateStatusReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  LUSRCEniLimitExceeded,
  LUSRCInsufficientRolePermissions,
  LUSRCInternalError,
  LUSRCInvalidConfiguration,
  LUSRCInvalidSecurityGroup,
  LUSRCInvalidSubnet,
  LUSRCSubnetOutOfIPAddresses,
  LastUpdateStatusReasonCode' #-}

instance FromText LastUpdateStatusReasonCode where
    parser = (LastUpdateStatusReasonCode' . mk) <$> takeText

instance ToText LastUpdateStatusReasonCode where
    toText (LastUpdateStatusReasonCode' ci) = original ci

-- | Represents an enum of /known/ $LastUpdateStatusReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LastUpdateStatusReasonCode where
    toEnum i = case i of
        0 -> LUSRCEniLimitExceeded
        1 -> LUSRCInsufficientRolePermissions
        2 -> LUSRCInternalError
        3 -> LUSRCInvalidConfiguration
        4 -> LUSRCInvalidSecurityGroup
        5 -> LUSRCInvalidSubnet
        6 -> LUSRCSubnetOutOfIPAddresses
        _ -> (error . showText) $ "Unknown index for LastUpdateStatusReasonCode: " <> toText i
    fromEnum x = case x of
        LUSRCEniLimitExceeded -> 0
        LUSRCInsufficientRolePermissions -> 1
        LUSRCInternalError -> 2
        LUSRCInvalidConfiguration -> 3
        LUSRCInvalidSecurityGroup -> 4
        LUSRCInvalidSubnet -> 5
        LUSRCSubnetOutOfIPAddresses -> 6
        LastUpdateStatusReasonCode' name -> (error . showText) $ "Unknown LastUpdateStatusReasonCode: " <> original name

-- | Represents the bounds of /known/ $LastUpdateStatusReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LastUpdateStatusReasonCode where
    minBound = LUSRCEniLimitExceeded
    maxBound = LUSRCSubnetOutOfIPAddresses

instance Hashable     LastUpdateStatusReasonCode
instance NFData       LastUpdateStatusReasonCode
instance ToByteString LastUpdateStatusReasonCode
instance ToQuery      LastUpdateStatusReasonCode
instance ToHeader     LastUpdateStatusReasonCode

instance FromJSON LastUpdateStatusReasonCode where
    parseJSON = parseJSONText "LastUpdateStatusReasonCode"
