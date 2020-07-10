{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostExplorer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CostExplorer where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CostExplorer
import Test.AWS.CostExplorer.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetReservationUtilization $
--             getReservationUtilization
--
--         , requestGetSavingsPlansCoverage $
--             getSavingsPlansCoverage
--
--         , requestGetTags $
--             getTags
--
--         , requestGetRightsizingRecommendation $
--             getRightsizingRecommendation
--
--         , requestGetCostAndUsageWithResources $
--             getCostAndUsageWithResources
--
--         , requestGetUsageForecast $
--             getUsageForecast
--
--         , requestGetReservationCoverage $
--             getReservationCoverage
--
--         , requestGetCostForecast $
--             getCostForecast
--
--         , requestGetDimensionValues $
--             getDimensionValues
--
--         , requestGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendation
--
--         , requestListCostCategoryDefinitions $
--             listCostCategoryDefinitions
--
--         , requestUpdateCostCategoryDefinition $
--             updateCostCategoryDefinition
--
--         , requestDeleteCostCategoryDefinition $
--             deleteCostCategoryDefinition
--
--         , requestCreateCostCategoryDefinition $
--             createCostCategoryDefinition
--
--         , requestGetCostAndUsage $
--             getCostAndUsage
--
--         , requestGetSavingsPlansPurchaseRecommendation $
--             getSavingsPlansPurchaseRecommendation
--
--         , requestGetSavingsPlansUtilization $
--             getSavingsPlansUtilization
--
--         , requestDescribeCostCategoryDefinition $
--             describeCostCategoryDefinition
--
--         , requestGetSavingsPlansUtilizationDetails $
--             getSavingsPlansUtilizationDetails
--
--           ]

--     , testGroup "response"
--         [ responseGetReservationUtilization $
--             getReservationUtilizationResponse
--
--         , responseGetSavingsPlansCoverage $
--             getSavingsPlansCoverageResponse
--
--         , responseGetTags $
--             getTagsResponse
--
--         , responseGetRightsizingRecommendation $
--             getRightsizingRecommendationResponse
--
--         , responseGetCostAndUsageWithResources $
--             getCostAndUsageWithResourcesResponse
--
--         , responseGetUsageForecast $
--             getUsageForecastResponse
--
--         , responseGetReservationCoverage $
--             getReservationCoverageResponse
--
--         , responseGetCostForecast $
--             getCostForecastResponse
--
--         , responseGetDimensionValues $
--             getDimensionValuesResponse
--
--         , responseGetReservationPurchaseRecommendation $
--             getReservationPurchaseRecommendationResponse
--
--         , responseListCostCategoryDefinitions $
--             listCostCategoryDefinitionsResponse
--
--         , responseUpdateCostCategoryDefinition $
--             updateCostCategoryDefinitionResponse
--
--         , responseDeleteCostCategoryDefinition $
--             deleteCostCategoryDefinitionResponse
--
--         , responseCreateCostCategoryDefinition $
--             createCostCategoryDefinitionResponse
--
--         , responseGetCostAndUsage $
--             getCostAndUsageResponse
--
--         , responseGetSavingsPlansPurchaseRecommendation $
--             getSavingsPlansPurchaseRecommendationResponse
--
--         , responseGetSavingsPlansUtilization $
--             getSavingsPlansUtilizationResponse
--
--         , responseDescribeCostCategoryDefinition $
--             describeCostCategoryDefinitionResponse
--
--         , responseGetSavingsPlansUtilizationDetails $
--             getSavingsPlansUtilizationDetailsResponse
--
--           ]
--     ]

-- Requests

requestGetReservationUtilization :: GetReservationUtilization -> TestTree
requestGetReservationUtilization = req
    "GetReservationUtilization"
    "fixture/GetReservationUtilization.yaml"

requestGetSavingsPlansCoverage :: GetSavingsPlansCoverage -> TestTree
requestGetSavingsPlansCoverage = req
    "GetSavingsPlansCoverage"
    "fixture/GetSavingsPlansCoverage.yaml"

requestGetTags :: GetTags -> TestTree
requestGetTags = req
    "GetTags"
    "fixture/GetTags.yaml"

requestGetRightsizingRecommendation :: GetRightsizingRecommendation -> TestTree
requestGetRightsizingRecommendation = req
    "GetRightsizingRecommendation"
    "fixture/GetRightsizingRecommendation.yaml"

requestGetCostAndUsageWithResources :: GetCostAndUsageWithResources -> TestTree
requestGetCostAndUsageWithResources = req
    "GetCostAndUsageWithResources"
    "fixture/GetCostAndUsageWithResources.yaml"

requestGetUsageForecast :: GetUsageForecast -> TestTree
requestGetUsageForecast = req
    "GetUsageForecast"
    "fixture/GetUsageForecast.yaml"

requestGetReservationCoverage :: GetReservationCoverage -> TestTree
requestGetReservationCoverage = req
    "GetReservationCoverage"
    "fixture/GetReservationCoverage.yaml"

requestGetCostForecast :: GetCostForecast -> TestTree
requestGetCostForecast = req
    "GetCostForecast"
    "fixture/GetCostForecast.yaml"

requestGetDimensionValues :: GetDimensionValues -> TestTree
requestGetDimensionValues = req
    "GetDimensionValues"
    "fixture/GetDimensionValues.yaml"

requestGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendation -> TestTree
requestGetReservationPurchaseRecommendation = req
    "GetReservationPurchaseRecommendation"
    "fixture/GetReservationPurchaseRecommendation.yaml"

requestListCostCategoryDefinitions :: ListCostCategoryDefinitions -> TestTree
requestListCostCategoryDefinitions = req
    "ListCostCategoryDefinitions"
    "fixture/ListCostCategoryDefinitions.yaml"

requestUpdateCostCategoryDefinition :: UpdateCostCategoryDefinition -> TestTree
requestUpdateCostCategoryDefinition = req
    "UpdateCostCategoryDefinition"
    "fixture/UpdateCostCategoryDefinition.yaml"

requestDeleteCostCategoryDefinition :: DeleteCostCategoryDefinition -> TestTree
requestDeleteCostCategoryDefinition = req
    "DeleteCostCategoryDefinition"
    "fixture/DeleteCostCategoryDefinition.yaml"

requestCreateCostCategoryDefinition :: CreateCostCategoryDefinition -> TestTree
requestCreateCostCategoryDefinition = req
    "CreateCostCategoryDefinition"
    "fixture/CreateCostCategoryDefinition.yaml"

requestGetCostAndUsage :: GetCostAndUsage -> TestTree
requestGetCostAndUsage = req
    "GetCostAndUsage"
    "fixture/GetCostAndUsage.yaml"

requestGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendation -> TestTree
requestGetSavingsPlansPurchaseRecommendation = req
    "GetSavingsPlansPurchaseRecommendation"
    "fixture/GetSavingsPlansPurchaseRecommendation.yaml"

requestGetSavingsPlansUtilization :: GetSavingsPlansUtilization -> TestTree
requestGetSavingsPlansUtilization = req
    "GetSavingsPlansUtilization"
    "fixture/GetSavingsPlansUtilization.yaml"

requestDescribeCostCategoryDefinition :: DescribeCostCategoryDefinition -> TestTree
requestDescribeCostCategoryDefinition = req
    "DescribeCostCategoryDefinition"
    "fixture/DescribeCostCategoryDefinition.yaml"

requestGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetails -> TestTree
requestGetSavingsPlansUtilizationDetails = req
    "GetSavingsPlansUtilizationDetails"
    "fixture/GetSavingsPlansUtilizationDetails.yaml"

-- Responses

responseGetReservationUtilization :: GetReservationUtilizationResponse -> TestTree
responseGetReservationUtilization = res
    "GetReservationUtilizationResponse"
    "fixture/GetReservationUtilizationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationUtilization)

responseGetSavingsPlansCoverage :: GetSavingsPlansCoverageResponse -> TestTree
responseGetSavingsPlansCoverage = res
    "GetSavingsPlansCoverageResponse"
    "fixture/GetSavingsPlansCoverageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansCoverage)

responseGetTags :: GetTagsResponse -> TestTree
responseGetTags = res
    "GetTagsResponse"
    "fixture/GetTagsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetTags)

responseGetRightsizingRecommendation :: GetRightsizingRecommendationResponse -> TestTree
responseGetRightsizingRecommendation = res
    "GetRightsizingRecommendationResponse"
    "fixture/GetRightsizingRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetRightsizingRecommendation)

responseGetCostAndUsageWithResources :: GetCostAndUsageWithResourcesResponse -> TestTree
responseGetCostAndUsageWithResources = res
    "GetCostAndUsageWithResourcesResponse"
    "fixture/GetCostAndUsageWithResourcesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostAndUsageWithResources)

responseGetUsageForecast :: GetUsageForecastResponse -> TestTree
responseGetUsageForecast = res
    "GetUsageForecastResponse"
    "fixture/GetUsageForecastResponse.proto"
    costExplorer
    (Proxy :: Proxy GetUsageForecast)

responseGetReservationCoverage :: GetReservationCoverageResponse -> TestTree
responseGetReservationCoverage = res
    "GetReservationCoverageResponse"
    "fixture/GetReservationCoverageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationCoverage)

responseGetCostForecast :: GetCostForecastResponse -> TestTree
responseGetCostForecast = res
    "GetCostForecastResponse"
    "fixture/GetCostForecastResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostForecast)

responseGetDimensionValues :: GetDimensionValuesResponse -> TestTree
responseGetDimensionValues = res
    "GetDimensionValuesResponse"
    "fixture/GetDimensionValuesResponse.proto"
    costExplorer
    (Proxy :: Proxy GetDimensionValues)

responseGetReservationPurchaseRecommendation :: GetReservationPurchaseRecommendationResponse -> TestTree
responseGetReservationPurchaseRecommendation = res
    "GetReservationPurchaseRecommendationResponse"
    "fixture/GetReservationPurchaseRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetReservationPurchaseRecommendation)

responseListCostCategoryDefinitions :: ListCostCategoryDefinitionsResponse -> TestTree
responseListCostCategoryDefinitions = res
    "ListCostCategoryDefinitionsResponse"
    "fixture/ListCostCategoryDefinitionsResponse.proto"
    costExplorer
    (Proxy :: Proxy ListCostCategoryDefinitions)

responseUpdateCostCategoryDefinition :: UpdateCostCategoryDefinitionResponse -> TestTree
responseUpdateCostCategoryDefinition = res
    "UpdateCostCategoryDefinitionResponse"
    "fixture/UpdateCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy UpdateCostCategoryDefinition)

responseDeleteCostCategoryDefinition :: DeleteCostCategoryDefinitionResponse -> TestTree
responseDeleteCostCategoryDefinition = res
    "DeleteCostCategoryDefinitionResponse"
    "fixture/DeleteCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy DeleteCostCategoryDefinition)

responseCreateCostCategoryDefinition :: CreateCostCategoryDefinitionResponse -> TestTree
responseCreateCostCategoryDefinition = res
    "CreateCostCategoryDefinitionResponse"
    "fixture/CreateCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy CreateCostCategoryDefinition)

responseGetCostAndUsage :: GetCostAndUsageResponse -> TestTree
responseGetCostAndUsage = res
    "GetCostAndUsageResponse"
    "fixture/GetCostAndUsageResponse.proto"
    costExplorer
    (Proxy :: Proxy GetCostAndUsage)

responseGetSavingsPlansPurchaseRecommendation :: GetSavingsPlansPurchaseRecommendationResponse -> TestTree
responseGetSavingsPlansPurchaseRecommendation = res
    "GetSavingsPlansPurchaseRecommendationResponse"
    "fixture/GetSavingsPlansPurchaseRecommendationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansPurchaseRecommendation)

responseGetSavingsPlansUtilization :: GetSavingsPlansUtilizationResponse -> TestTree
responseGetSavingsPlansUtilization = res
    "GetSavingsPlansUtilizationResponse"
    "fixture/GetSavingsPlansUtilizationResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansUtilization)

responseDescribeCostCategoryDefinition :: DescribeCostCategoryDefinitionResponse -> TestTree
responseDescribeCostCategoryDefinition = res
    "DescribeCostCategoryDefinitionResponse"
    "fixture/DescribeCostCategoryDefinitionResponse.proto"
    costExplorer
    (Proxy :: Proxy DescribeCostCategoryDefinition)

responseGetSavingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> TestTree
responseGetSavingsPlansUtilizationDetails = res
    "GetSavingsPlansUtilizationDetailsResponse"
    "fixture/GetSavingsPlansUtilizationDetailsResponse.proto"
    costExplorer
    (Proxy :: Proxy GetSavingsPlansUtilizationDetails)
