{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUsageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyUsageType (
  PolicyUsageType (
    ..
    , PermissionsBoundary
    , PermissionsPolicy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The policy usage type that indicates whether the policy is used as a permissions policy or as the permissions boundary for an entity.
--
--
-- For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
data PolicyUsageType = PolicyUsageType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern PermissionsBoundary :: PolicyUsageType
pattern PermissionsBoundary = PolicyUsageType' "PermissionsBoundary"

pattern PermissionsPolicy :: PolicyUsageType
pattern PermissionsPolicy = PolicyUsageType' "PermissionsPolicy"

{-# COMPLETE
  PermissionsBoundary,
  PermissionsPolicy,
  PolicyUsageType' #-}

instance FromText PolicyUsageType where
    parser = (PolicyUsageType' . mk) <$> takeText

instance ToText PolicyUsageType where
    toText (PolicyUsageType' ci) = original ci

-- | Represents an enum of /known/ $PolicyUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyUsageType where
    toEnum i = case i of
        0 -> PermissionsBoundary
        1 -> PermissionsPolicy
        _ -> (error . showText) $ "Unknown index for PolicyUsageType: " <> toText i
    fromEnum x = case x of
        PermissionsBoundary -> 0
        PermissionsPolicy -> 1
        PolicyUsageType' name -> (error . showText) $ "Unknown PolicyUsageType: " <> original name

-- | Represents the bounds of /known/ $PolicyUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyUsageType where
    minBound = PermissionsBoundary
    maxBound = PermissionsPolicy

instance Hashable     PolicyUsageType
instance NFData       PolicyUsageType
instance ToByteString PolicyUsageType
instance ToQuery      PolicyUsageType
instance ToHeader     PolicyUsageType
