{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Product (
    module Network.AWS.CostExplorer.Types.CostCategory,
    module Network.AWS.CostExplorer.Types.CostCategoryReference,
    module Network.AWS.CostExplorer.Types.CostCategoryRule,
    module Network.AWS.CostExplorer.Types.CostCategoryValues,
    module Network.AWS.CostExplorer.Types.Coverage,
    module Network.AWS.CostExplorer.Types.CoverageByTime,
    module Network.AWS.CostExplorer.Types.CoverageCost,
    module Network.AWS.CostExplorer.Types.CoverageHours,
    module Network.AWS.CostExplorer.Types.CoverageNormalizedUnits,
    module Network.AWS.CostExplorer.Types.CurrentInstance,
    module Network.AWS.CostExplorer.Types.DateInterval,
    module Network.AWS.CostExplorer.Types.DimensionValues,
    module Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes,
    module Network.AWS.CostExplorer.Types.EC2InstanceDetails,
    module Network.AWS.CostExplorer.Types.EC2ResourceDetails,
    module Network.AWS.CostExplorer.Types.EC2ResourceUtilization,
    module Network.AWS.CostExplorer.Types.EC2Specification,
    module Network.AWS.CostExplorer.Types.ESInstanceDetails,
    module Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails,
    module Network.AWS.CostExplorer.Types.Expression,
    module Network.AWS.CostExplorer.Types.ForecastResult,
    module Network.AWS.CostExplorer.Types.Group,
    module Network.AWS.CostExplorer.Types.GroupDefinition,
    module Network.AWS.CostExplorer.Types.InstanceDetails,
    module Network.AWS.CostExplorer.Types.MetricValue,
    module Network.AWS.CostExplorer.Types.ModifyRecommendationDetail,
    module Network.AWS.CostExplorer.Types.RDSInstanceDetails,
    module Network.AWS.CostExplorer.Types.RedshiftInstanceDetails,
    module Network.AWS.CostExplorer.Types.ReservationAggregates,
    module Network.AWS.CostExplorer.Types.ReservationCoverageGroup,
    module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation,
    module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail,
    module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata,
    module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary,
    module Network.AWS.CostExplorer.Types.ReservationUtilizationGroup,
    module Network.AWS.CostExplorer.Types.ResourceDetails,
    module Network.AWS.CostExplorer.Types.ResourceUtilization,
    module Network.AWS.CostExplorer.Types.ResultByTime,
    module Network.AWS.CostExplorer.Types.RightsizingRecommendation,
    module Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration,
    module Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata,
    module Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary,
    module Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment,
    module Network.AWS.CostExplorer.Types.SavingsPlansCoverage,
    module Network.AWS.CostExplorer.Types.SavingsPlansCoverageData,
    module Network.AWS.CostExplorer.Types.SavingsPlansDetails,
    module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation,
    module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail,
    module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata,
    module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary,
    module Network.AWS.CostExplorer.Types.SavingsPlansSavings,
    module Network.AWS.CostExplorer.Types.SavingsPlansUtilization,
    module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates,
    module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime,
    module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail,
    module Network.AWS.CostExplorer.Types.ServiceSpecification,
    module Network.AWS.CostExplorer.Types.TagValues,
    module Network.AWS.CostExplorer.Types.TargetInstance,
    module Network.AWS.CostExplorer.Types.TerminateRecommendationDetail,
    module Network.AWS.CostExplorer.Types.UtilizationByTime
  ) where

import Network.AWS.CostExplorer.Types.CostCategory
import Network.AWS.CostExplorer.Types.CostCategoryReference
import Network.AWS.CostExplorer.Types.CostCategoryRule
import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.CostExplorer.Types.CoverageByTime
import Network.AWS.CostExplorer.Types.CoverageCost
import Network.AWS.CostExplorer.Types.CoverageHours
import Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import Network.AWS.CostExplorer.Types.EC2Specification
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.Expression
import Network.AWS.CostExplorer.Types.ForecastResult
import Network.AWS.CostExplorer.Types.Group
import Network.AWS.CostExplorer.Types.GroupDefinition
import Network.AWS.CostExplorer.Types.InstanceDetails
import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import Network.AWS.CostExplorer.Types.ReservationAggregates
import Network.AWS.CostExplorer.Types.ReservationCoverageGroup
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendation
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.ReservationUtilizationGroup
import Network.AWS.CostExplorer.Types.ResourceDetails
import Network.AWS.CostExplorer.Types.ResourceUtilization
import Network.AWS.CostExplorer.Types.ResultByTime
import Network.AWS.CostExplorer.Types.RightsizingRecommendation
import Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
import Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
import Network.AWS.CostExplorer.Types.RightsizingRecommendationSummary
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansCoverage
import Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
import Network.AWS.CostExplorer.Types.SavingsPlansDetails
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendation
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationDetail
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
import Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationSummary
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
import Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
import Network.AWS.CostExplorer.Types.ServiceSpecification
import Network.AWS.CostExplorer.Types.TagValues
import Network.AWS.CostExplorer.Types.TargetInstance
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import Network.AWS.CostExplorer.Types.UtilizationByTime
import Network.AWS.Lens
import Network.AWS.Prelude
