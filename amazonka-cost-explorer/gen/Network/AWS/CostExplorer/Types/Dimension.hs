{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Dimension
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Dimension (
  Dimension (
    ..
    , DimensionAZ
    , DimensionBillingEntity
    , DimensionCacheEngine
    , DimensionDatabaseEngine
    , DimensionDeploymentOption
    , DimensionInstanceType
    , DimensionInstanceTypeFamily
    , DimensionLegalEntityName
    , DimensionLinkedAccount
    , DimensionLinkedAccountName
    , DimensionOperatingSystem
    , DimensionOperation
    , DimensionPaymentOption
    , DimensionPlatform
    , DimensionPurchaseType
    , DimensionRecordType
    , DimensionRegion
    , DimensionReservationId
    , DimensionResourceId
    , DimensionRightsizingType
    , DimensionSavingsPlanARN
    , DimensionSavingsPlansType
    , DimensionScope
    , DimensionService
    , DimensionServiceCode
    , DimensionSubscriptionId
    , DimensionTenancy
    , DimensionUsageType
    , DimensionUsageTypeGroup
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Dimension = Dimension' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern DimensionAZ :: Dimension
pattern DimensionAZ = Dimension' "AZ"

pattern DimensionBillingEntity :: Dimension
pattern DimensionBillingEntity = Dimension' "BILLING_ENTITY"

pattern DimensionCacheEngine :: Dimension
pattern DimensionCacheEngine = Dimension' "CACHE_ENGINE"

pattern DimensionDatabaseEngine :: Dimension
pattern DimensionDatabaseEngine = Dimension' "DATABASE_ENGINE"

pattern DimensionDeploymentOption :: Dimension
pattern DimensionDeploymentOption = Dimension' "DEPLOYMENT_OPTION"

pattern DimensionInstanceType :: Dimension
pattern DimensionInstanceType = Dimension' "INSTANCE_TYPE"

pattern DimensionInstanceTypeFamily :: Dimension
pattern DimensionInstanceTypeFamily = Dimension' "INSTANCE_TYPE_FAMILY"

pattern DimensionLegalEntityName :: Dimension
pattern DimensionLegalEntityName = Dimension' "LEGAL_ENTITY_NAME"

pattern DimensionLinkedAccount :: Dimension
pattern DimensionLinkedAccount = Dimension' "LINKED_ACCOUNT"

pattern DimensionLinkedAccountName :: Dimension
pattern DimensionLinkedAccountName = Dimension' "LINKED_ACCOUNT_NAME"

pattern DimensionOperatingSystem :: Dimension
pattern DimensionOperatingSystem = Dimension' "OPERATING_SYSTEM"

pattern DimensionOperation :: Dimension
pattern DimensionOperation = Dimension' "OPERATION"

pattern DimensionPaymentOption :: Dimension
pattern DimensionPaymentOption = Dimension' "PAYMENT_OPTION"

pattern DimensionPlatform :: Dimension
pattern DimensionPlatform = Dimension' "PLATFORM"

pattern DimensionPurchaseType :: Dimension
pattern DimensionPurchaseType = Dimension' "PURCHASE_TYPE"

pattern DimensionRecordType :: Dimension
pattern DimensionRecordType = Dimension' "RECORD_TYPE"

pattern DimensionRegion :: Dimension
pattern DimensionRegion = Dimension' "REGION"

pattern DimensionReservationId :: Dimension
pattern DimensionReservationId = Dimension' "RESERVATION_ID"

pattern DimensionResourceId :: Dimension
pattern DimensionResourceId = Dimension' "RESOURCE_ID"

pattern DimensionRightsizingType :: Dimension
pattern DimensionRightsizingType = Dimension' "RIGHTSIZING_TYPE"

pattern DimensionSavingsPlanARN :: Dimension
pattern DimensionSavingsPlanARN = Dimension' "SAVINGS_PLAN_ARN"

pattern DimensionSavingsPlansType :: Dimension
pattern DimensionSavingsPlansType = Dimension' "SAVINGS_PLANS_TYPE"

pattern DimensionScope :: Dimension
pattern DimensionScope = Dimension' "SCOPE"

pattern DimensionService :: Dimension
pattern DimensionService = Dimension' "SERVICE"

pattern DimensionServiceCode :: Dimension
pattern DimensionServiceCode = Dimension' "SERVICE_CODE"

pattern DimensionSubscriptionId :: Dimension
pattern DimensionSubscriptionId = Dimension' "SUBSCRIPTION_ID"

pattern DimensionTenancy :: Dimension
pattern DimensionTenancy = Dimension' "TENANCY"

pattern DimensionUsageType :: Dimension
pattern DimensionUsageType = Dimension' "USAGE_TYPE"

pattern DimensionUsageTypeGroup :: Dimension
pattern DimensionUsageTypeGroup = Dimension' "USAGE_TYPE_GROUP"

{-# COMPLETE
  DimensionAZ,
  DimensionBillingEntity,
  DimensionCacheEngine,
  DimensionDatabaseEngine,
  DimensionDeploymentOption,
  DimensionInstanceType,
  DimensionInstanceTypeFamily,
  DimensionLegalEntityName,
  DimensionLinkedAccount,
  DimensionLinkedAccountName,
  DimensionOperatingSystem,
  DimensionOperation,
  DimensionPaymentOption,
  DimensionPlatform,
  DimensionPurchaseType,
  DimensionRecordType,
  DimensionRegion,
  DimensionReservationId,
  DimensionResourceId,
  DimensionRightsizingType,
  DimensionSavingsPlanARN,
  DimensionSavingsPlansType,
  DimensionScope,
  DimensionService,
  DimensionServiceCode,
  DimensionSubscriptionId,
  DimensionTenancy,
  DimensionUsageType,
  DimensionUsageTypeGroup,
  Dimension' #-}

instance FromText Dimension where
    parser = (Dimension' . mk) <$> takeText

instance ToText Dimension where
    toText (Dimension' ci) = original ci

-- | Represents an enum of /known/ $Dimension.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Dimension where
    toEnum i = case i of
        0 -> DimensionAZ
        1 -> DimensionBillingEntity
        2 -> DimensionCacheEngine
        3 -> DimensionDatabaseEngine
        4 -> DimensionDeploymentOption
        5 -> DimensionInstanceType
        6 -> DimensionInstanceTypeFamily
        7 -> DimensionLegalEntityName
        8 -> DimensionLinkedAccount
        9 -> DimensionLinkedAccountName
        10 -> DimensionOperatingSystem
        11 -> DimensionOperation
        12 -> DimensionPaymentOption
        13 -> DimensionPlatform
        14 -> DimensionPurchaseType
        15 -> DimensionRecordType
        16 -> DimensionRegion
        17 -> DimensionReservationId
        18 -> DimensionResourceId
        19 -> DimensionRightsizingType
        20 -> DimensionSavingsPlanARN
        21 -> DimensionSavingsPlansType
        22 -> DimensionScope
        23 -> DimensionService
        24 -> DimensionServiceCode
        25 -> DimensionSubscriptionId
        26 -> DimensionTenancy
        27 -> DimensionUsageType
        28 -> DimensionUsageTypeGroup
        _ -> (error . showText) $ "Unknown index for Dimension: " <> toText i
    fromEnum x = case x of
        DimensionAZ -> 0
        DimensionBillingEntity -> 1
        DimensionCacheEngine -> 2
        DimensionDatabaseEngine -> 3
        DimensionDeploymentOption -> 4
        DimensionInstanceType -> 5
        DimensionInstanceTypeFamily -> 6
        DimensionLegalEntityName -> 7
        DimensionLinkedAccount -> 8
        DimensionLinkedAccountName -> 9
        DimensionOperatingSystem -> 10
        DimensionOperation -> 11
        DimensionPaymentOption -> 12
        DimensionPlatform -> 13
        DimensionPurchaseType -> 14
        DimensionRecordType -> 15
        DimensionRegion -> 16
        DimensionReservationId -> 17
        DimensionResourceId -> 18
        DimensionRightsizingType -> 19
        DimensionSavingsPlanARN -> 20
        DimensionSavingsPlansType -> 21
        DimensionScope -> 22
        DimensionService -> 23
        DimensionServiceCode -> 24
        DimensionSubscriptionId -> 25
        DimensionTenancy -> 26
        DimensionUsageType -> 27
        DimensionUsageTypeGroup -> 28
        Dimension' name -> (error . showText) $ "Unknown Dimension: " <> original name

-- | Represents the bounds of /known/ $Dimension.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Dimension where
    minBound = DimensionAZ
    maxBound = DimensionUsageTypeGroup

instance Hashable     Dimension
instance NFData       Dimension
instance ToByteString Dimension
instance ToQuery      Dimension
instance ToHeader     Dimension

instance ToJSON Dimension where
    toJSON = toJSONText

instance FromJSON Dimension where
    parseJSON = parseJSONText "Dimension"
