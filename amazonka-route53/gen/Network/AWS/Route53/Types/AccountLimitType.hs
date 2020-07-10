{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.AccountLimitType (
  AccountLimitType (
    ..
    , MaxHealthChecksByOwner
    , MaxHostedZonesByOwner
    , MaxReusableDelegationSetsByOwner
    , MaxTrafficPoliciesByOwner
    , MaxTrafficPolicyInstancesByOwner
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data AccountLimitType = AccountLimitType' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern MaxHealthChecksByOwner :: AccountLimitType
pattern MaxHealthChecksByOwner = AccountLimitType' "MAX_HEALTH_CHECKS_BY_OWNER"

pattern MaxHostedZonesByOwner :: AccountLimitType
pattern MaxHostedZonesByOwner = AccountLimitType' "MAX_HOSTED_ZONES_BY_OWNER"

pattern MaxReusableDelegationSetsByOwner :: AccountLimitType
pattern MaxReusableDelegationSetsByOwner = AccountLimitType' "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER"

pattern MaxTrafficPoliciesByOwner :: AccountLimitType
pattern MaxTrafficPoliciesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICIES_BY_OWNER"

pattern MaxTrafficPolicyInstancesByOwner :: AccountLimitType
pattern MaxTrafficPolicyInstancesByOwner = AccountLimitType' "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER"

{-# COMPLETE
  MaxHealthChecksByOwner,
  MaxHostedZonesByOwner,
  MaxReusableDelegationSetsByOwner,
  MaxTrafficPoliciesByOwner,
  MaxTrafficPolicyInstancesByOwner,
  AccountLimitType' #-}

instance FromText AccountLimitType where
    parser = (AccountLimitType' . mk) <$> takeText

instance ToText AccountLimitType where
    toText (AccountLimitType' ci) = original ci

-- | Represents an enum of /known/ $AccountLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccountLimitType where
    toEnum i = case i of
        0 -> MaxHealthChecksByOwner
        1 -> MaxHostedZonesByOwner
        2 -> MaxReusableDelegationSetsByOwner
        3 -> MaxTrafficPoliciesByOwner
        4 -> MaxTrafficPolicyInstancesByOwner
        _ -> (error . showText) $ "Unknown index for AccountLimitType: " <> toText i
    fromEnum x = case x of
        MaxHealthChecksByOwner -> 0
        MaxHostedZonesByOwner -> 1
        MaxReusableDelegationSetsByOwner -> 2
        MaxTrafficPoliciesByOwner -> 3
        MaxTrafficPolicyInstancesByOwner -> 4
        AccountLimitType' name -> (error . showText) $ "Unknown AccountLimitType: " <> original name

-- | Represents the bounds of /known/ $AccountLimitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccountLimitType where
    minBound = MaxHealthChecksByOwner
    maxBound = MaxTrafficPolicyInstancesByOwner

instance Hashable     AccountLimitType
instance NFData       AccountLimitType
instance ToByteString AccountLimitType
instance ToQuery      AccountLimitType
instance ToHeader     AccountLimitType

instance FromXML AccountLimitType where
    parseXML = parseXMLText "AccountLimitType"

instance ToXML AccountLimitType where
    toXML = toXMLText
