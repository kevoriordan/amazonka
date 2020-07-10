{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceAnalytics.Types.DataSetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceAnalytics.Types.DataSetType (
  DataSetType (
    ..
    , CustomerProfileByGeography
    , CustomerProfileByIndustry
    , CustomerProfileByRevenue
    , CustomerSubscriberAnnualSubscriptions
    , CustomerSubscriberHourlyMonthlySubscriptions
    , DailyBusinessCanceledProductSubscribers
    , DailyBusinessFees
    , DailyBusinessFreeTrialConversions
    , DailyBusinessNewInstances
    , DailyBusinessNewProductSubscribers
    , DailyBusinessUsageByInstanceType
    , DisbursedAmountByAgeOfDisbursedFunds
    , DisbursedAmountByAgeOfPastDueFunds
    , DisbursedAmountByAgeOfUncollectedFunds
    , DisbursedAmountByCustomerGeo
    , DisbursedAmountByInstanceHours
    , DisbursedAmountByProduct
    , DisbursedAmountByProductWithUncollectedFunds
    , DisbursedAmountByUncollectedFundsBreakdown
    , MonthlyRevenueAnnualSubscriptions
    , MonthlyRevenueBillingAndRevenueData
    , MonthlyRevenueFieldDemonstrationUsage
    , MonthlyRevenueFlexiblePaymentSchedule
    , SalesCompensationBilledRevenue
    , UsSalesAndUseTaxRecords
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DataSetType = DataSetType' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern CustomerProfileByGeography :: DataSetType
pattern CustomerProfileByGeography = DataSetType' "customer_profile_by_geography"

pattern CustomerProfileByIndustry :: DataSetType
pattern CustomerProfileByIndustry = DataSetType' "customer_profile_by_industry"

pattern CustomerProfileByRevenue :: DataSetType
pattern CustomerProfileByRevenue = DataSetType' "customer_profile_by_revenue"

pattern CustomerSubscriberAnnualSubscriptions :: DataSetType
pattern CustomerSubscriberAnnualSubscriptions = DataSetType' "customer_subscriber_annual_subscriptions"

pattern CustomerSubscriberHourlyMonthlySubscriptions :: DataSetType
pattern CustomerSubscriberHourlyMonthlySubscriptions = DataSetType' "customer_subscriber_hourly_monthly_subscriptions"

pattern DailyBusinessCanceledProductSubscribers :: DataSetType
pattern DailyBusinessCanceledProductSubscribers = DataSetType' "daily_business_canceled_product_subscribers"

pattern DailyBusinessFees :: DataSetType
pattern DailyBusinessFees = DataSetType' "daily_business_fees"

pattern DailyBusinessFreeTrialConversions :: DataSetType
pattern DailyBusinessFreeTrialConversions = DataSetType' "daily_business_free_trial_conversions"

pattern DailyBusinessNewInstances :: DataSetType
pattern DailyBusinessNewInstances = DataSetType' "daily_business_new_instances"

pattern DailyBusinessNewProductSubscribers :: DataSetType
pattern DailyBusinessNewProductSubscribers = DataSetType' "daily_business_new_product_subscribers"

pattern DailyBusinessUsageByInstanceType :: DataSetType
pattern DailyBusinessUsageByInstanceType = DataSetType' "daily_business_usage_by_instance_type"

pattern DisbursedAmountByAgeOfDisbursedFunds :: DataSetType
pattern DisbursedAmountByAgeOfDisbursedFunds = DataSetType' "disbursed_amount_by_age_of_disbursed_funds"

pattern DisbursedAmountByAgeOfPastDueFunds :: DataSetType
pattern DisbursedAmountByAgeOfPastDueFunds = DataSetType' "disbursed_amount_by_age_of_past_due_funds"

pattern DisbursedAmountByAgeOfUncollectedFunds :: DataSetType
pattern DisbursedAmountByAgeOfUncollectedFunds = DataSetType' "disbursed_amount_by_age_of_uncollected_funds"

pattern DisbursedAmountByCustomerGeo :: DataSetType
pattern DisbursedAmountByCustomerGeo = DataSetType' "disbursed_amount_by_customer_geo"

pattern DisbursedAmountByInstanceHours :: DataSetType
pattern DisbursedAmountByInstanceHours = DataSetType' "disbursed_amount_by_instance_hours"

pattern DisbursedAmountByProduct :: DataSetType
pattern DisbursedAmountByProduct = DataSetType' "disbursed_amount_by_product"

pattern DisbursedAmountByProductWithUncollectedFunds :: DataSetType
pattern DisbursedAmountByProductWithUncollectedFunds = DataSetType' "disbursed_amount_by_product_with_uncollected_funds"

pattern DisbursedAmountByUncollectedFundsBreakdown :: DataSetType
pattern DisbursedAmountByUncollectedFundsBreakdown = DataSetType' "disbursed_amount_by_uncollected_funds_breakdown"

pattern MonthlyRevenueAnnualSubscriptions :: DataSetType
pattern MonthlyRevenueAnnualSubscriptions = DataSetType' "monthly_revenue_annual_subscriptions"

pattern MonthlyRevenueBillingAndRevenueData :: DataSetType
pattern MonthlyRevenueBillingAndRevenueData = DataSetType' "monthly_revenue_billing_and_revenue_data"

pattern MonthlyRevenueFieldDemonstrationUsage :: DataSetType
pattern MonthlyRevenueFieldDemonstrationUsage = DataSetType' "monthly_revenue_field_demonstration_usage"

pattern MonthlyRevenueFlexiblePaymentSchedule :: DataSetType
pattern MonthlyRevenueFlexiblePaymentSchedule = DataSetType' "monthly_revenue_flexible_payment_schedule"

pattern SalesCompensationBilledRevenue :: DataSetType
pattern SalesCompensationBilledRevenue = DataSetType' "sales_compensation_billed_revenue"

pattern UsSalesAndUseTaxRecords :: DataSetType
pattern UsSalesAndUseTaxRecords = DataSetType' "us_sales_and_use_tax_records"

{-# COMPLETE
  CustomerProfileByGeography,
  CustomerProfileByIndustry,
  CustomerProfileByRevenue,
  CustomerSubscriberAnnualSubscriptions,
  CustomerSubscriberHourlyMonthlySubscriptions,
  DailyBusinessCanceledProductSubscribers,
  DailyBusinessFees,
  DailyBusinessFreeTrialConversions,
  DailyBusinessNewInstances,
  DailyBusinessNewProductSubscribers,
  DailyBusinessUsageByInstanceType,
  DisbursedAmountByAgeOfDisbursedFunds,
  DisbursedAmountByAgeOfPastDueFunds,
  DisbursedAmountByAgeOfUncollectedFunds,
  DisbursedAmountByCustomerGeo,
  DisbursedAmountByInstanceHours,
  DisbursedAmountByProduct,
  DisbursedAmountByProductWithUncollectedFunds,
  DisbursedAmountByUncollectedFundsBreakdown,
  MonthlyRevenueAnnualSubscriptions,
  MonthlyRevenueBillingAndRevenueData,
  MonthlyRevenueFieldDemonstrationUsage,
  MonthlyRevenueFlexiblePaymentSchedule,
  SalesCompensationBilledRevenue,
  UsSalesAndUseTaxRecords,
  DataSetType' #-}

instance FromText DataSetType where
    parser = (DataSetType' . mk) <$> takeText

instance ToText DataSetType where
    toText (DataSetType' ci) = original ci

-- | Represents an enum of /known/ $DataSetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DataSetType where
    toEnum i = case i of
        0 -> CustomerProfileByGeography
        1 -> CustomerProfileByIndustry
        2 -> CustomerProfileByRevenue
        3 -> CustomerSubscriberAnnualSubscriptions
        4 -> CustomerSubscriberHourlyMonthlySubscriptions
        5 -> DailyBusinessCanceledProductSubscribers
        6 -> DailyBusinessFees
        7 -> DailyBusinessFreeTrialConversions
        8 -> DailyBusinessNewInstances
        9 -> DailyBusinessNewProductSubscribers
        10 -> DailyBusinessUsageByInstanceType
        11 -> DisbursedAmountByAgeOfDisbursedFunds
        12 -> DisbursedAmountByAgeOfPastDueFunds
        13 -> DisbursedAmountByAgeOfUncollectedFunds
        14 -> DisbursedAmountByCustomerGeo
        15 -> DisbursedAmountByInstanceHours
        16 -> DisbursedAmountByProduct
        17 -> DisbursedAmountByProductWithUncollectedFunds
        18 -> DisbursedAmountByUncollectedFundsBreakdown
        19 -> MonthlyRevenueAnnualSubscriptions
        20 -> MonthlyRevenueBillingAndRevenueData
        21 -> MonthlyRevenueFieldDemonstrationUsage
        22 -> MonthlyRevenueFlexiblePaymentSchedule
        23 -> SalesCompensationBilledRevenue
        24 -> UsSalesAndUseTaxRecords
        _ -> (error . showText) $ "Unknown index for DataSetType: " <> toText i
    fromEnum x = case x of
        CustomerProfileByGeography -> 0
        CustomerProfileByIndustry -> 1
        CustomerProfileByRevenue -> 2
        CustomerSubscriberAnnualSubscriptions -> 3
        CustomerSubscriberHourlyMonthlySubscriptions -> 4
        DailyBusinessCanceledProductSubscribers -> 5
        DailyBusinessFees -> 6
        DailyBusinessFreeTrialConversions -> 7
        DailyBusinessNewInstances -> 8
        DailyBusinessNewProductSubscribers -> 9
        DailyBusinessUsageByInstanceType -> 10
        DisbursedAmountByAgeOfDisbursedFunds -> 11
        DisbursedAmountByAgeOfPastDueFunds -> 12
        DisbursedAmountByAgeOfUncollectedFunds -> 13
        DisbursedAmountByCustomerGeo -> 14
        DisbursedAmountByInstanceHours -> 15
        DisbursedAmountByProduct -> 16
        DisbursedAmountByProductWithUncollectedFunds -> 17
        DisbursedAmountByUncollectedFundsBreakdown -> 18
        MonthlyRevenueAnnualSubscriptions -> 19
        MonthlyRevenueBillingAndRevenueData -> 20
        MonthlyRevenueFieldDemonstrationUsage -> 21
        MonthlyRevenueFlexiblePaymentSchedule -> 22
        SalesCompensationBilledRevenue -> 23
        UsSalesAndUseTaxRecords -> 24
        DataSetType' name -> (error . showText) $ "Unknown DataSetType: " <> original name

-- | Represents the bounds of /known/ $DataSetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DataSetType where
    minBound = CustomerProfileByGeography
    maxBound = UsSalesAndUseTaxRecords

instance Hashable     DataSetType
instance NFData       DataSetType
instance ToByteString DataSetType
instance ToQuery      DataSetType
instance ToHeader     DataSetType

instance ToJSON DataSetType where
    toJSON = toJSONText
