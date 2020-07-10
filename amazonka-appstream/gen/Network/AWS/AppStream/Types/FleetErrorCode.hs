{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.FleetErrorCode (
  FleetErrorCode (
    ..
    , DomainJoinErrorAccessDenied
    , DomainJoinErrorDsMachineAccountQuotaExceeded
    , DomainJoinErrorFileNotFound
    , DomainJoinErrorInvalidParameter
    , DomainJoinErrorLogonFailure
    , DomainJoinErrorMoreData
    , DomainJoinErrorNoSuchDomain
    , DomainJoinErrorNotSupported
    , DomainJoinInternalServiceError
    , DomainJoinNerrInvalidWorkgroupName
    , DomainJoinNerrPasswordExpired
    , DomainJoinNerrWorkstationNotStarted
    , IAMServiceRoleIsMissing
    , IAMServiceRoleMissingDescribeSecurityGroupsAction
    , IAMServiceRoleMissingDescribeSubnetAction
    , IAMServiceRoleMissingEniCreateAction
    , IAMServiceRoleMissingEniDeleteAction
    , IAMServiceRoleMissingEniDescribeAction
    , IgwNotAttached
    , ImageNotFound
    , InternalServiceError
    , InvalidSubnetConfiguration
    , MachineRoleIsMissing
    , NetworkInterfaceLimitExceeded
    , SecurityGroupsNotFound
    , StsDisabledInRegion
    , SubnetHasInsufficientIPAddresses
    , SubnetNotFound
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FleetErrorCode = FleetErrorCode' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DomainJoinErrorAccessDenied :: FleetErrorCode
pattern DomainJoinErrorAccessDenied = FleetErrorCode' "DOMAIN_JOIN_ERROR_ACCESS_DENIED"

pattern DomainJoinErrorDsMachineAccountQuotaExceeded :: FleetErrorCode
pattern DomainJoinErrorDsMachineAccountQuotaExceeded = FleetErrorCode' "DOMAIN_JOIN_ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED"

pattern DomainJoinErrorFileNotFound :: FleetErrorCode
pattern DomainJoinErrorFileNotFound = FleetErrorCode' "DOMAIN_JOIN_ERROR_FILE_NOT_FOUND"

pattern DomainJoinErrorInvalidParameter :: FleetErrorCode
pattern DomainJoinErrorInvalidParameter = FleetErrorCode' "DOMAIN_JOIN_ERROR_INVALID_PARAMETER"

pattern DomainJoinErrorLogonFailure :: FleetErrorCode
pattern DomainJoinErrorLogonFailure = FleetErrorCode' "DOMAIN_JOIN_ERROR_LOGON_FAILURE"

pattern DomainJoinErrorMoreData :: FleetErrorCode
pattern DomainJoinErrorMoreData = FleetErrorCode' "DOMAIN_JOIN_ERROR_MORE_DATA"

pattern DomainJoinErrorNoSuchDomain :: FleetErrorCode
pattern DomainJoinErrorNoSuchDomain = FleetErrorCode' "DOMAIN_JOIN_ERROR_NO_SUCH_DOMAIN"

pattern DomainJoinErrorNotSupported :: FleetErrorCode
pattern DomainJoinErrorNotSupported = FleetErrorCode' "DOMAIN_JOIN_ERROR_NOT_SUPPORTED"

pattern DomainJoinInternalServiceError :: FleetErrorCode
pattern DomainJoinInternalServiceError = FleetErrorCode' "DOMAIN_JOIN_INTERNAL_SERVICE_ERROR"

pattern DomainJoinNerrInvalidWorkgroupName :: FleetErrorCode
pattern DomainJoinNerrInvalidWorkgroupName = FleetErrorCode' "DOMAIN_JOIN_NERR_INVALID_WORKGROUP_NAME"

pattern DomainJoinNerrPasswordExpired :: FleetErrorCode
pattern DomainJoinNerrPasswordExpired = FleetErrorCode' "DOMAIN_JOIN_NERR_PASSWORD_EXPIRED"

pattern DomainJoinNerrWorkstationNotStarted :: FleetErrorCode
pattern DomainJoinNerrWorkstationNotStarted = FleetErrorCode' "DOMAIN_JOIN_NERR_WORKSTATION_NOT_STARTED"

pattern IAMServiceRoleIsMissing :: FleetErrorCode
pattern IAMServiceRoleIsMissing = FleetErrorCode' "IAM_SERVICE_ROLE_IS_MISSING"

pattern IAMServiceRoleMissingDescribeSecurityGroupsAction :: FleetErrorCode
pattern IAMServiceRoleMissingDescribeSecurityGroupsAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SECURITY_GROUPS_ACTION"

pattern IAMServiceRoleMissingDescribeSubnetAction :: FleetErrorCode
pattern IAMServiceRoleMissingDescribeSubnetAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_DESCRIBE_SUBNET_ACTION"

pattern IAMServiceRoleMissingEniCreateAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniCreateAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_CREATE_ACTION"

pattern IAMServiceRoleMissingEniDeleteAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniDeleteAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DELETE_ACTION"

pattern IAMServiceRoleMissingEniDescribeAction :: FleetErrorCode
pattern IAMServiceRoleMissingEniDescribeAction = FleetErrorCode' "IAM_SERVICE_ROLE_MISSING_ENI_DESCRIBE_ACTION"

pattern IgwNotAttached :: FleetErrorCode
pattern IgwNotAttached = FleetErrorCode' "IGW_NOT_ATTACHED"

pattern ImageNotFound :: FleetErrorCode
pattern ImageNotFound = FleetErrorCode' "IMAGE_NOT_FOUND"

pattern InternalServiceError :: FleetErrorCode
pattern InternalServiceError = FleetErrorCode' "INTERNAL_SERVICE_ERROR"

pattern InvalidSubnetConfiguration :: FleetErrorCode
pattern InvalidSubnetConfiguration = FleetErrorCode' "INVALID_SUBNET_CONFIGURATION"

pattern MachineRoleIsMissing :: FleetErrorCode
pattern MachineRoleIsMissing = FleetErrorCode' "MACHINE_ROLE_IS_MISSING"

pattern NetworkInterfaceLimitExceeded :: FleetErrorCode
pattern NetworkInterfaceLimitExceeded = FleetErrorCode' "NETWORK_INTERFACE_LIMIT_EXCEEDED"

pattern SecurityGroupsNotFound :: FleetErrorCode
pattern SecurityGroupsNotFound = FleetErrorCode' "SECURITY_GROUPS_NOT_FOUND"

pattern StsDisabledInRegion :: FleetErrorCode
pattern StsDisabledInRegion = FleetErrorCode' "STS_DISABLED_IN_REGION"

pattern SubnetHasInsufficientIPAddresses :: FleetErrorCode
pattern SubnetHasInsufficientIPAddresses = FleetErrorCode' "SUBNET_HAS_INSUFFICIENT_IP_ADDRESSES"

pattern SubnetNotFound :: FleetErrorCode
pattern SubnetNotFound = FleetErrorCode' "SUBNET_NOT_FOUND"

{-# COMPLETE
  DomainJoinErrorAccessDenied,
  DomainJoinErrorDsMachineAccountQuotaExceeded,
  DomainJoinErrorFileNotFound,
  DomainJoinErrorInvalidParameter,
  DomainJoinErrorLogonFailure,
  DomainJoinErrorMoreData,
  DomainJoinErrorNoSuchDomain,
  DomainJoinErrorNotSupported,
  DomainJoinInternalServiceError,
  DomainJoinNerrInvalidWorkgroupName,
  DomainJoinNerrPasswordExpired,
  DomainJoinNerrWorkstationNotStarted,
  IAMServiceRoleIsMissing,
  IAMServiceRoleMissingDescribeSecurityGroupsAction,
  IAMServiceRoleMissingDescribeSubnetAction,
  IAMServiceRoleMissingEniCreateAction,
  IAMServiceRoleMissingEniDeleteAction,
  IAMServiceRoleMissingEniDescribeAction,
  IgwNotAttached,
  ImageNotFound,
  InternalServiceError,
  InvalidSubnetConfiguration,
  MachineRoleIsMissing,
  NetworkInterfaceLimitExceeded,
  SecurityGroupsNotFound,
  StsDisabledInRegion,
  SubnetHasInsufficientIPAddresses,
  SubnetNotFound,
  FleetErrorCode' #-}

instance FromText FleetErrorCode where
    parser = (FleetErrorCode' . mk) <$> takeText

instance ToText FleetErrorCode where
    toText (FleetErrorCode' ci) = original ci

-- | Represents an enum of /known/ $FleetErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetErrorCode where
    toEnum i = case i of
        0 -> DomainJoinErrorAccessDenied
        1 -> DomainJoinErrorDsMachineAccountQuotaExceeded
        2 -> DomainJoinErrorFileNotFound
        3 -> DomainJoinErrorInvalidParameter
        4 -> DomainJoinErrorLogonFailure
        5 -> DomainJoinErrorMoreData
        6 -> DomainJoinErrorNoSuchDomain
        7 -> DomainJoinErrorNotSupported
        8 -> DomainJoinInternalServiceError
        9 -> DomainJoinNerrInvalidWorkgroupName
        10 -> DomainJoinNerrPasswordExpired
        11 -> DomainJoinNerrWorkstationNotStarted
        12 -> IAMServiceRoleIsMissing
        13 -> IAMServiceRoleMissingDescribeSecurityGroupsAction
        14 -> IAMServiceRoleMissingDescribeSubnetAction
        15 -> IAMServiceRoleMissingEniCreateAction
        16 -> IAMServiceRoleMissingEniDeleteAction
        17 -> IAMServiceRoleMissingEniDescribeAction
        18 -> IgwNotAttached
        19 -> ImageNotFound
        20 -> InternalServiceError
        21 -> InvalidSubnetConfiguration
        22 -> MachineRoleIsMissing
        23 -> NetworkInterfaceLimitExceeded
        24 -> SecurityGroupsNotFound
        25 -> StsDisabledInRegion
        26 -> SubnetHasInsufficientIPAddresses
        27 -> SubnetNotFound
        _ -> (error . showText) $ "Unknown index for FleetErrorCode: " <> toText i
    fromEnum x = case x of
        DomainJoinErrorAccessDenied -> 0
        DomainJoinErrorDsMachineAccountQuotaExceeded -> 1
        DomainJoinErrorFileNotFound -> 2
        DomainJoinErrorInvalidParameter -> 3
        DomainJoinErrorLogonFailure -> 4
        DomainJoinErrorMoreData -> 5
        DomainJoinErrorNoSuchDomain -> 6
        DomainJoinErrorNotSupported -> 7
        DomainJoinInternalServiceError -> 8
        DomainJoinNerrInvalidWorkgroupName -> 9
        DomainJoinNerrPasswordExpired -> 10
        DomainJoinNerrWorkstationNotStarted -> 11
        IAMServiceRoleIsMissing -> 12
        IAMServiceRoleMissingDescribeSecurityGroupsAction -> 13
        IAMServiceRoleMissingDescribeSubnetAction -> 14
        IAMServiceRoleMissingEniCreateAction -> 15
        IAMServiceRoleMissingEniDeleteAction -> 16
        IAMServiceRoleMissingEniDescribeAction -> 17
        IgwNotAttached -> 18
        ImageNotFound -> 19
        InternalServiceError -> 20
        InvalidSubnetConfiguration -> 21
        MachineRoleIsMissing -> 22
        NetworkInterfaceLimitExceeded -> 23
        SecurityGroupsNotFound -> 24
        StsDisabledInRegion -> 25
        SubnetHasInsufficientIPAddresses -> 26
        SubnetNotFound -> 27
        FleetErrorCode' name -> (error . showText) $ "Unknown FleetErrorCode: " <> original name

-- | Represents the bounds of /known/ $FleetErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetErrorCode where
    minBound = DomainJoinErrorAccessDenied
    maxBound = SubnetNotFound

instance Hashable     FleetErrorCode
instance NFData       FleetErrorCode
instance ToByteString FleetErrorCode
instance ToQuery      FleetErrorCode
instance ToHeader     FleetErrorCode

instance FromJSON FleetErrorCode where
    parseJSON = parseJSONText "FleetErrorCode"
