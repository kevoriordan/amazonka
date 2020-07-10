{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SummaryKeyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.SummaryKeyType (
  SummaryKeyType (
    ..
    , AccessKeysPerUserQuota
    , AccountAccessKeysPresent
    , AccountMFAEnabled
    , AccountSigningCertificatesPresent
    , AttachedPoliciesPerGroupQuota
    , AttachedPoliciesPerRoleQuota
    , AttachedPoliciesPerUserQuota
    , GlobalEndpointTokenVersion
    , GroupPolicySizeQuota
    , Groups
    , GroupsPerUserQuota
    , GroupsQuota
    , MFADevices
    , MFADevicesInUse
    , Policies
    , PoliciesQuota
    , PolicySizeQuota
    , PolicyVersionsInUse
    , PolicyVersionsInUseQuota
    , ServerCertificates
    , ServerCertificatesQuota
    , SigningCertificatesPerUserQuota
    , UserPolicySizeQuota
    , Users
    , UsersQuota
    , VersionsPerPolicyQuota
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SummaryKeyType = SummaryKeyType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AccessKeysPerUserQuota :: SummaryKeyType
pattern AccessKeysPerUserQuota = SummaryKeyType' "AccessKeysPerUserQuota"

pattern AccountAccessKeysPresent :: SummaryKeyType
pattern AccountAccessKeysPresent = SummaryKeyType' "AccountAccessKeysPresent"

pattern AccountMFAEnabled :: SummaryKeyType
pattern AccountMFAEnabled = SummaryKeyType' "AccountMFAEnabled"

pattern AccountSigningCertificatesPresent :: SummaryKeyType
pattern AccountSigningCertificatesPresent = SummaryKeyType' "AccountSigningCertificatesPresent"

pattern AttachedPoliciesPerGroupQuota :: SummaryKeyType
pattern AttachedPoliciesPerGroupQuota = SummaryKeyType' "AttachedPoliciesPerGroupQuota"

pattern AttachedPoliciesPerRoleQuota :: SummaryKeyType
pattern AttachedPoliciesPerRoleQuota = SummaryKeyType' "AttachedPoliciesPerRoleQuota"

pattern AttachedPoliciesPerUserQuota :: SummaryKeyType
pattern AttachedPoliciesPerUserQuota = SummaryKeyType' "AttachedPoliciesPerUserQuota"

pattern GlobalEndpointTokenVersion :: SummaryKeyType
pattern GlobalEndpointTokenVersion = SummaryKeyType' "GlobalEndpointTokenVersion"

pattern GroupPolicySizeQuota :: SummaryKeyType
pattern GroupPolicySizeQuota = SummaryKeyType' "GroupPolicySizeQuota"

pattern Groups :: SummaryKeyType
pattern Groups = SummaryKeyType' "Groups"

pattern GroupsPerUserQuota :: SummaryKeyType
pattern GroupsPerUserQuota = SummaryKeyType' "GroupsPerUserQuota"

pattern GroupsQuota :: SummaryKeyType
pattern GroupsQuota = SummaryKeyType' "GroupsQuota"

pattern MFADevices :: SummaryKeyType
pattern MFADevices = SummaryKeyType' "MFADevices"

pattern MFADevicesInUse :: SummaryKeyType
pattern MFADevicesInUse = SummaryKeyType' "MFADevicesInUse"

pattern Policies :: SummaryKeyType
pattern Policies = SummaryKeyType' "Policies"

pattern PoliciesQuota :: SummaryKeyType
pattern PoliciesQuota = SummaryKeyType' "PoliciesQuota"

pattern PolicySizeQuota :: SummaryKeyType
pattern PolicySizeQuota = SummaryKeyType' "PolicySizeQuota"

pattern PolicyVersionsInUse :: SummaryKeyType
pattern PolicyVersionsInUse = SummaryKeyType' "PolicyVersionsInUse"

pattern PolicyVersionsInUseQuota :: SummaryKeyType
pattern PolicyVersionsInUseQuota = SummaryKeyType' "PolicyVersionsInUseQuota"

pattern ServerCertificates :: SummaryKeyType
pattern ServerCertificates = SummaryKeyType' "ServerCertificates"

pattern ServerCertificatesQuota :: SummaryKeyType
pattern ServerCertificatesQuota = SummaryKeyType' "ServerCertificatesQuota"

pattern SigningCertificatesPerUserQuota :: SummaryKeyType
pattern SigningCertificatesPerUserQuota = SummaryKeyType' "SigningCertificatesPerUserQuota"

pattern UserPolicySizeQuota :: SummaryKeyType
pattern UserPolicySizeQuota = SummaryKeyType' "UserPolicySizeQuota"

pattern Users :: SummaryKeyType
pattern Users = SummaryKeyType' "Users"

pattern UsersQuota :: SummaryKeyType
pattern UsersQuota = SummaryKeyType' "UsersQuota"

pattern VersionsPerPolicyQuota :: SummaryKeyType
pattern VersionsPerPolicyQuota = SummaryKeyType' "VersionsPerPolicyQuota"

{-# COMPLETE
  AccessKeysPerUserQuota,
  AccountAccessKeysPresent,
  AccountMFAEnabled,
  AccountSigningCertificatesPresent,
  AttachedPoliciesPerGroupQuota,
  AttachedPoliciesPerRoleQuota,
  AttachedPoliciesPerUserQuota,
  GlobalEndpointTokenVersion,
  GroupPolicySizeQuota,
  Groups,
  GroupsPerUserQuota,
  GroupsQuota,
  MFADevices,
  MFADevicesInUse,
  Policies,
  PoliciesQuota,
  PolicySizeQuota,
  PolicyVersionsInUse,
  PolicyVersionsInUseQuota,
  ServerCertificates,
  ServerCertificatesQuota,
  SigningCertificatesPerUserQuota,
  UserPolicySizeQuota,
  Users,
  UsersQuota,
  VersionsPerPolicyQuota,
  SummaryKeyType' #-}

instance FromText SummaryKeyType where
    parser = (SummaryKeyType' . mk) <$> takeText

instance ToText SummaryKeyType where
    toText (SummaryKeyType' ci) = original ci

-- | Represents an enum of /known/ $SummaryKeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SummaryKeyType where
    toEnum i = case i of
        0 -> AccessKeysPerUserQuota
        1 -> AccountAccessKeysPresent
        2 -> AccountMFAEnabled
        3 -> AccountSigningCertificatesPresent
        4 -> AttachedPoliciesPerGroupQuota
        5 -> AttachedPoliciesPerRoleQuota
        6 -> AttachedPoliciesPerUserQuota
        7 -> GlobalEndpointTokenVersion
        8 -> GroupPolicySizeQuota
        9 -> Groups
        10 -> GroupsPerUserQuota
        11 -> GroupsQuota
        12 -> MFADevices
        13 -> MFADevicesInUse
        14 -> Policies
        15 -> PoliciesQuota
        16 -> PolicySizeQuota
        17 -> PolicyVersionsInUse
        18 -> PolicyVersionsInUseQuota
        19 -> ServerCertificates
        20 -> ServerCertificatesQuota
        21 -> SigningCertificatesPerUserQuota
        22 -> UserPolicySizeQuota
        23 -> Users
        24 -> UsersQuota
        25 -> VersionsPerPolicyQuota
        _ -> (error . showText) $ "Unknown index for SummaryKeyType: " <> toText i
    fromEnum x = case x of
        AccessKeysPerUserQuota -> 0
        AccountAccessKeysPresent -> 1
        AccountMFAEnabled -> 2
        AccountSigningCertificatesPresent -> 3
        AttachedPoliciesPerGroupQuota -> 4
        AttachedPoliciesPerRoleQuota -> 5
        AttachedPoliciesPerUserQuota -> 6
        GlobalEndpointTokenVersion -> 7
        GroupPolicySizeQuota -> 8
        Groups -> 9
        GroupsPerUserQuota -> 10
        GroupsQuota -> 11
        MFADevices -> 12
        MFADevicesInUse -> 13
        Policies -> 14
        PoliciesQuota -> 15
        PolicySizeQuota -> 16
        PolicyVersionsInUse -> 17
        PolicyVersionsInUseQuota -> 18
        ServerCertificates -> 19
        ServerCertificatesQuota -> 20
        SigningCertificatesPerUserQuota -> 21
        UserPolicySizeQuota -> 22
        Users -> 23
        UsersQuota -> 24
        VersionsPerPolicyQuota -> 25
        SummaryKeyType' name -> (error . showText) $ "Unknown SummaryKeyType: " <> original name

-- | Represents the bounds of /known/ $SummaryKeyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SummaryKeyType where
    minBound = AccessKeysPerUserQuota
    maxBound = VersionsPerPolicyQuota

instance Hashable     SummaryKeyType
instance NFData       SummaryKeyType
instance ToByteString SummaryKeyType
instance ToQuery      SummaryKeyType
instance ToHeader     SummaryKeyType

instance FromXML SummaryKeyType where
    parseXML = parseXMLText "SummaryKeyType"
