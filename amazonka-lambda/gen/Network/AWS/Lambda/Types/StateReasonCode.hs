{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.StateReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.StateReasonCode (
  StateReasonCode (
    ..
    , Creating
    , EniLimitExceeded
    , Idle
    , InsufficientRolePermissions
    , InternalError
    , InvalidConfiguration
    , InvalidSecurityGroup
    , InvalidSubnet
    , Restoring
    , SubnetOutOfIPAddresses
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StateReasonCode = StateReasonCode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Creating :: StateReasonCode
pattern Creating = StateReasonCode' "Creating"

pattern EniLimitExceeded :: StateReasonCode
pattern EniLimitExceeded = StateReasonCode' "EniLimitExceeded"

pattern Idle :: StateReasonCode
pattern Idle = StateReasonCode' "Idle"

pattern InsufficientRolePermissions :: StateReasonCode
pattern InsufficientRolePermissions = StateReasonCode' "InsufficientRolePermissions"

pattern InternalError :: StateReasonCode
pattern InternalError = StateReasonCode' "InternalError"

pattern InvalidConfiguration :: StateReasonCode
pattern InvalidConfiguration = StateReasonCode' "InvalidConfiguration"

pattern InvalidSecurityGroup :: StateReasonCode
pattern InvalidSecurityGroup = StateReasonCode' "InvalidSecurityGroup"

pattern InvalidSubnet :: StateReasonCode
pattern InvalidSubnet = StateReasonCode' "InvalidSubnet"

pattern Restoring :: StateReasonCode
pattern Restoring = StateReasonCode' "Restoring"

pattern SubnetOutOfIPAddresses :: StateReasonCode
pattern SubnetOutOfIPAddresses = StateReasonCode' "SubnetOutOfIPAddresses"

{-# COMPLETE
  Creating,
  EniLimitExceeded,
  Idle,
  InsufficientRolePermissions,
  InternalError,
  InvalidConfiguration,
  InvalidSecurityGroup,
  InvalidSubnet,
  Restoring,
  SubnetOutOfIPAddresses,
  StateReasonCode' #-}

instance FromText StateReasonCode where
    parser = (StateReasonCode' . mk) <$> takeText

instance ToText StateReasonCode where
    toText (StateReasonCode' ci) = original ci

-- | Represents an enum of /known/ $StateReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StateReasonCode where
    toEnum i = case i of
        0 -> Creating
        1 -> EniLimitExceeded
        2 -> Idle
        3 -> InsufficientRolePermissions
        4 -> InternalError
        5 -> InvalidConfiguration
        6 -> InvalidSecurityGroup
        7 -> InvalidSubnet
        8 -> Restoring
        9 -> SubnetOutOfIPAddresses
        _ -> (error . showText) $ "Unknown index for StateReasonCode: " <> toText i
    fromEnum x = case x of
        Creating -> 0
        EniLimitExceeded -> 1
        Idle -> 2
        InsufficientRolePermissions -> 3
        InternalError -> 4
        InvalidConfiguration -> 5
        InvalidSecurityGroup -> 6
        InvalidSubnet -> 7
        Restoring -> 8
        SubnetOutOfIPAddresses -> 9
        StateReasonCode' name -> (error . showText) $ "Unknown StateReasonCode: " <> original name

-- | Represents the bounds of /known/ $StateReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StateReasonCode where
    minBound = Creating
    maxBound = SubnetOutOfIPAddresses

instance Hashable     StateReasonCode
instance NFData       StateReasonCode
instance ToByteString StateReasonCode
instance ToQuery      StateReasonCode
instance ToHeader     StateReasonCode

instance FromJSON StateReasonCode where
    parseJSON = parseJSONText "StateReasonCode"
