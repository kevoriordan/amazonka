{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticSearch
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticSearch where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElasticSearch
import Test.AWS.ElasticSearch.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateElasticsearchDomain $
--             createElasticsearchDomain
--
--         , requestRemoveTags $
--             removeTags
--
--         , requestGetCompatibleElasticsearchVersions $
--             getCompatibleElasticsearchVersions
--
--         , requestDescribeElasticsearchDomains $
--             describeElasticsearchDomains
--
--         , requestListDomainsForPackage $
--             listDomainsForPackage
--
--         , requestListPackagesForDomain $
--             listPackagesForDomain
--
--         , requestStartElasticsearchServiceSoftwareUpdate $
--             startElasticsearchServiceSoftwareUpdate
--
--         , requestListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypes
--
--         , requestDeleteElasticsearchServiceRole $
--             deleteElasticsearchServiceRole
--
--         , requestDescribeElasticsearchDomain $
--             describeElasticsearchDomain
--
--         , requestListDomainNames $
--             listDomainNames
--
--         , requestAssociatePackage $
--             associatePackage
--
--         , requestDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimits
--
--         , requestGetUpgradeHistory $
--             getUpgradeHistory
--
--         , requestDescribePackages $
--             describePackages
--
--         , requestDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfig
--
--         , requestGetUpgradeStatus $
--             getUpgradeStatus
--
--         , requestDeleteElasticsearchDomain $
--             deleteElasticsearchDomain
--
--         , requestDissociatePackage $
--             dissociatePackage
--
--         , requestPurchaseReservedElasticsearchInstanceOffering $
--             purchaseReservedElasticsearchInstanceOffering
--
--         , requestDescribeReservedElasticsearchInstances $
--             describeReservedElasticsearchInstances
--
--         , requestUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfig
--
--         , requestListElasticsearchVersions $
--             listElasticsearchVersions
--
--         , requestAddTags $
--             addTags
--
--         , requestDescribeReservedElasticsearchInstanceOfferings $
--             describeReservedElasticsearchInstanceOfferings
--
--         , requestUpgradeElasticsearchDomain $
--             upgradeElasticsearchDomain
--
--         , requestListTags $
--             listTags
--
--         , requestDeletePackage $
--             deletePackage
--
--         , requestCancelElasticsearchServiceSoftwareUpdate $
--             cancelElasticsearchServiceSoftwareUpdate
--
--         , requestCreatePackage $
--             createPackage
--
--           ]

--     , testGroup "response"
--         [ responseCreateElasticsearchDomain $
--             createElasticsearchDomainResponse
--
--         , responseRemoveTags $
--             removeTagsResponse
--
--         , responseGetCompatibleElasticsearchVersions $
--             getCompatibleElasticsearchVersionsResponse
--
--         , responseDescribeElasticsearchDomains $
--             describeElasticsearchDomainsResponse
--
--         , responseListDomainsForPackage $
--             listDomainsForPackageResponse
--
--         , responseListPackagesForDomain $
--             listPackagesForDomainResponse
--
--         , responseStartElasticsearchServiceSoftwareUpdate $
--             startElasticsearchServiceSoftwareUpdateResponse
--
--         , responseListElasticsearchInstanceTypes $
--             listElasticsearchInstanceTypesResponse
--
--         , responseDeleteElasticsearchServiceRole $
--             deleteElasticsearchServiceRoleResponse
--
--         , responseDescribeElasticsearchDomain $
--             describeElasticsearchDomainResponse
--
--         , responseListDomainNames $
--             listDomainNamesResponse
--
--         , responseAssociatePackage $
--             associatePackageResponse
--
--         , responseDescribeElasticsearchInstanceTypeLimits $
--             describeElasticsearchInstanceTypeLimitsResponse
--
--         , responseGetUpgradeHistory $
--             getUpgradeHistoryResponse
--
--         , responseDescribePackages $
--             describePackagesResponse
--
--         , responseDescribeElasticsearchDomainConfig $
--             describeElasticsearchDomainConfigResponse
--
--         , responseGetUpgradeStatus $
--             getUpgradeStatusResponse
--
--         , responseDeleteElasticsearchDomain $
--             deleteElasticsearchDomainResponse
--
--         , responseDissociatePackage $
--             dissociatePackageResponse
--
--         , responsePurchaseReservedElasticsearchInstanceOffering $
--             purchaseReservedElasticsearchInstanceOfferingResponse
--
--         , responseDescribeReservedElasticsearchInstances $
--             describeReservedElasticsearchInstancesResponse
--
--         , responseUpdateElasticsearchDomainConfig $
--             updateElasticsearchDomainConfigResponse
--
--         , responseListElasticsearchVersions $
--             listElasticsearchVersionsResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseDescribeReservedElasticsearchInstanceOfferings $
--             describeReservedElasticsearchInstanceOfferingsResponse
--
--         , responseUpgradeElasticsearchDomain $
--             upgradeElasticsearchDomainResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDeletePackage $
--             deletePackageResponse
--
--         , responseCancelElasticsearchServiceSoftwareUpdate $
--             cancelElasticsearchServiceSoftwareUpdateResponse
--
--         , responseCreatePackage $
--             createPackageResponse
--
--           ]
--     ]

-- Requests

requestCreateElasticsearchDomain :: CreateElasticsearchDomain -> TestTree
requestCreateElasticsearchDomain = req
    "CreateElasticsearchDomain"
    "fixture/CreateElasticsearchDomain.yaml"

requestRemoveTags :: RemoveTags -> TestTree
requestRemoveTags = req
    "RemoveTags"
    "fixture/RemoveTags.yaml"

requestGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersions -> TestTree
requestGetCompatibleElasticsearchVersions = req
    "GetCompatibleElasticsearchVersions"
    "fixture/GetCompatibleElasticsearchVersions.yaml"

requestDescribeElasticsearchDomains :: DescribeElasticsearchDomains -> TestTree
requestDescribeElasticsearchDomains = req
    "DescribeElasticsearchDomains"
    "fixture/DescribeElasticsearchDomains.yaml"

requestListDomainsForPackage :: ListDomainsForPackage -> TestTree
requestListDomainsForPackage = req
    "ListDomainsForPackage"
    "fixture/ListDomainsForPackage.yaml"

requestListPackagesForDomain :: ListPackagesForDomain -> TestTree
requestListPackagesForDomain = req
    "ListPackagesForDomain"
    "fixture/ListPackagesForDomain.yaml"

requestStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdate -> TestTree
requestStartElasticsearchServiceSoftwareUpdate = req
    "StartElasticsearchServiceSoftwareUpdate"
    "fixture/StartElasticsearchServiceSoftwareUpdate.yaml"

requestListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypes -> TestTree
requestListElasticsearchInstanceTypes = req
    "ListElasticsearchInstanceTypes"
    "fixture/ListElasticsearchInstanceTypes.yaml"

requestDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRole -> TestTree
requestDeleteElasticsearchServiceRole = req
    "DeleteElasticsearchServiceRole"
    "fixture/DeleteElasticsearchServiceRole.yaml"

requestDescribeElasticsearchDomain :: DescribeElasticsearchDomain -> TestTree
requestDescribeElasticsearchDomain = req
    "DescribeElasticsearchDomain"
    "fixture/DescribeElasticsearchDomain.yaml"

requestListDomainNames :: ListDomainNames -> TestTree
requestListDomainNames = req
    "ListDomainNames"
    "fixture/ListDomainNames.yaml"

requestAssociatePackage :: AssociatePackage -> TestTree
requestAssociatePackage = req
    "AssociatePackage"
    "fixture/AssociatePackage.yaml"

requestDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimits -> TestTree
requestDescribeElasticsearchInstanceTypeLimits = req
    "DescribeElasticsearchInstanceTypeLimits"
    "fixture/DescribeElasticsearchInstanceTypeLimits.yaml"

requestGetUpgradeHistory :: GetUpgradeHistory -> TestTree
requestGetUpgradeHistory = req
    "GetUpgradeHistory"
    "fixture/GetUpgradeHistory.yaml"

requestDescribePackages :: DescribePackages -> TestTree
requestDescribePackages = req
    "DescribePackages"
    "fixture/DescribePackages.yaml"

requestDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfig -> TestTree
requestDescribeElasticsearchDomainConfig = req
    "DescribeElasticsearchDomainConfig"
    "fixture/DescribeElasticsearchDomainConfig.yaml"

requestGetUpgradeStatus :: GetUpgradeStatus -> TestTree
requestGetUpgradeStatus = req
    "GetUpgradeStatus"
    "fixture/GetUpgradeStatus.yaml"

requestDeleteElasticsearchDomain :: DeleteElasticsearchDomain -> TestTree
requestDeleteElasticsearchDomain = req
    "DeleteElasticsearchDomain"
    "fixture/DeleteElasticsearchDomain.yaml"

requestDissociatePackage :: DissociatePackage -> TestTree
requestDissociatePackage = req
    "DissociatePackage"
    "fixture/DissociatePackage.yaml"

requestPurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOffering -> TestTree
requestPurchaseReservedElasticsearchInstanceOffering = req
    "PurchaseReservedElasticsearchInstanceOffering"
    "fixture/PurchaseReservedElasticsearchInstanceOffering.yaml"

requestDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstances -> TestTree
requestDescribeReservedElasticsearchInstances = req
    "DescribeReservedElasticsearchInstances"
    "fixture/DescribeReservedElasticsearchInstances.yaml"

requestUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfig -> TestTree
requestUpdateElasticsearchDomainConfig = req
    "UpdateElasticsearchDomainConfig"
    "fixture/UpdateElasticsearchDomainConfig.yaml"

requestListElasticsearchVersions :: ListElasticsearchVersions -> TestTree
requestListElasticsearchVersions = req
    "ListElasticsearchVersions"
    "fixture/ListElasticsearchVersions.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferings -> TestTree
requestDescribeReservedElasticsearchInstanceOfferings = req
    "DescribeReservedElasticsearchInstanceOfferings"
    "fixture/DescribeReservedElasticsearchInstanceOfferings.yaml"

requestUpgradeElasticsearchDomain :: UpgradeElasticsearchDomain -> TestTree
requestUpgradeElasticsearchDomain = req
    "UpgradeElasticsearchDomain"
    "fixture/UpgradeElasticsearchDomain.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage = req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdate -> TestTree
requestCancelElasticsearchServiceSoftwareUpdate = req
    "CancelElasticsearchServiceSoftwareUpdate"
    "fixture/CancelElasticsearchServiceSoftwareUpdate.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage = req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

-- Responses

responseCreateElasticsearchDomain :: CreateElasticsearchDomainResponse -> TestTree
responseCreateElasticsearchDomain = res
    "CreateElasticsearchDomainResponse"
    "fixture/CreateElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreateElasticsearchDomain)

responseRemoveTags :: RemoveTagsResponse -> TestTree
responseRemoveTags = res
    "RemoveTagsResponse"
    "fixture/RemoveTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy RemoveTags)

responseGetCompatibleElasticsearchVersions :: GetCompatibleElasticsearchVersionsResponse -> TestTree
responseGetCompatibleElasticsearchVersions = res
    "GetCompatibleElasticsearchVersionsResponse"
    "fixture/GetCompatibleElasticsearchVersionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetCompatibleElasticsearchVersions)

responseDescribeElasticsearchDomains :: DescribeElasticsearchDomainsResponse -> TestTree
responseDescribeElasticsearchDomains = res
    "DescribeElasticsearchDomainsResponse"
    "fixture/DescribeElasticsearchDomainsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomains)

responseListDomainsForPackage :: ListDomainsForPackageResponse -> TestTree
responseListDomainsForPackage = res
    "ListDomainsForPackageResponse"
    "fixture/ListDomainsForPackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainsForPackage)

responseListPackagesForDomain :: ListPackagesForDomainResponse -> TestTree
responseListPackagesForDomain = res
    "ListPackagesForDomainResponse"
    "fixture/ListPackagesForDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListPackagesForDomain)

responseStartElasticsearchServiceSoftwareUpdate :: StartElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseStartElasticsearchServiceSoftwareUpdate = res
    "StartElasticsearchServiceSoftwareUpdateResponse"
    "fixture/StartElasticsearchServiceSoftwareUpdateResponse.proto"
    elasticSearch
    (Proxy :: Proxy StartElasticsearchServiceSoftwareUpdate)

responseListElasticsearchInstanceTypes :: ListElasticsearchInstanceTypesResponse -> TestTree
responseListElasticsearchInstanceTypes = res
    "ListElasticsearchInstanceTypesResponse"
    "fixture/ListElasticsearchInstanceTypesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchInstanceTypes)

responseDeleteElasticsearchServiceRole :: DeleteElasticsearchServiceRoleResponse -> TestTree
responseDeleteElasticsearchServiceRole = res
    "DeleteElasticsearchServiceRoleResponse"
    "fixture/DeleteElasticsearchServiceRoleResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchServiceRole)

responseDescribeElasticsearchDomain :: DescribeElasticsearchDomainResponse -> TestTree
responseDescribeElasticsearchDomain = res
    "DescribeElasticsearchDomainResponse"
    "fixture/DescribeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomain)

responseListDomainNames :: ListDomainNamesResponse -> TestTree
responseListDomainNames = res
    "ListDomainNamesResponse"
    "fixture/ListDomainNamesResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListDomainNames)

responseAssociatePackage :: AssociatePackageResponse -> TestTree
responseAssociatePackage = res
    "AssociatePackageResponse"
    "fixture/AssociatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy AssociatePackage)

responseDescribeElasticsearchInstanceTypeLimits :: DescribeElasticsearchInstanceTypeLimitsResponse -> TestTree
responseDescribeElasticsearchInstanceTypeLimits = res
    "DescribeElasticsearchInstanceTypeLimitsResponse"
    "fixture/DescribeElasticsearchInstanceTypeLimitsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchInstanceTypeLimits)

responseGetUpgradeHistory :: GetUpgradeHistoryResponse -> TestTree
responseGetUpgradeHistory = res
    "GetUpgradeHistoryResponse"
    "fixture/GetUpgradeHistoryResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetUpgradeHistory)

responseDescribePackages :: DescribePackagesResponse -> TestTree
responseDescribePackages = res
    "DescribePackagesResponse"
    "fixture/DescribePackagesResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribePackages)

responseDescribeElasticsearchDomainConfig :: DescribeElasticsearchDomainConfigResponse -> TestTree
responseDescribeElasticsearchDomainConfig = res
    "DescribeElasticsearchDomainConfigResponse"
    "fixture/DescribeElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeElasticsearchDomainConfig)

responseGetUpgradeStatus :: GetUpgradeStatusResponse -> TestTree
responseGetUpgradeStatus = res
    "GetUpgradeStatusResponse"
    "fixture/GetUpgradeStatusResponse.proto"
    elasticSearch
    (Proxy :: Proxy GetUpgradeStatus)

responseDeleteElasticsearchDomain :: DeleteElasticsearchDomainResponse -> TestTree
responseDeleteElasticsearchDomain = res
    "DeleteElasticsearchDomainResponse"
    "fixture/DeleteElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeleteElasticsearchDomain)

responseDissociatePackage :: DissociatePackageResponse -> TestTree
responseDissociatePackage = res
    "DissociatePackageResponse"
    "fixture/DissociatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy DissociatePackage)

responsePurchaseReservedElasticsearchInstanceOffering :: PurchaseReservedElasticsearchInstanceOfferingResponse -> TestTree
responsePurchaseReservedElasticsearchInstanceOffering = res
    "PurchaseReservedElasticsearchInstanceOfferingResponse"
    "fixture/PurchaseReservedElasticsearchInstanceOfferingResponse.proto"
    elasticSearch
    (Proxy :: Proxy PurchaseReservedElasticsearchInstanceOffering)

responseDescribeReservedElasticsearchInstances :: DescribeReservedElasticsearchInstancesResponse -> TestTree
responseDescribeReservedElasticsearchInstances = res
    "DescribeReservedElasticsearchInstancesResponse"
    "fixture/DescribeReservedElasticsearchInstancesResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeReservedElasticsearchInstances)

responseUpdateElasticsearchDomainConfig :: UpdateElasticsearchDomainConfigResponse -> TestTree
responseUpdateElasticsearchDomainConfig = res
    "UpdateElasticsearchDomainConfigResponse"
    "fixture/UpdateElasticsearchDomainConfigResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpdateElasticsearchDomainConfig)

responseListElasticsearchVersions :: ListElasticsearchVersionsResponse -> TestTree
responseListElasticsearchVersions = res
    "ListElasticsearchVersionsResponse"
    "fixture/ListElasticsearchVersionsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListElasticsearchVersions)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy AddTags)

responseDescribeReservedElasticsearchInstanceOfferings :: DescribeReservedElasticsearchInstanceOfferingsResponse -> TestTree
responseDescribeReservedElasticsearchInstanceOfferings = res
    "DescribeReservedElasticsearchInstanceOfferingsResponse"
    "fixture/DescribeReservedElasticsearchInstanceOfferingsResponse.proto"
    elasticSearch
    (Proxy :: Proxy DescribeReservedElasticsearchInstanceOfferings)

responseUpgradeElasticsearchDomain :: UpgradeElasticsearchDomainResponse -> TestTree
responseUpgradeElasticsearchDomain = res
    "UpgradeElasticsearchDomainResponse"
    "fixture/UpgradeElasticsearchDomainResponse.proto"
    elasticSearch
    (Proxy :: Proxy UpgradeElasticsearchDomain)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    elasticSearch
    (Proxy :: Proxy ListTags)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage = res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy DeletePackage)

responseCancelElasticsearchServiceSoftwareUpdate :: CancelElasticsearchServiceSoftwareUpdateResponse -> TestTree
responseCancelElasticsearchServiceSoftwareUpdate = res
    "CancelElasticsearchServiceSoftwareUpdateResponse"
    "fixture/CancelElasticsearchServiceSoftwareUpdateResponse.proto"
    elasticSearch
    (Proxy :: Proxy CancelElasticsearchServiceSoftwareUpdate)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage = res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    elasticSearch
    (Proxy :: Proxy CreatePackage)
