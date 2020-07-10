{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.Product (
    module Network.AWS.ElasticSearch.Types.AccessPoliciesStatus,
    module Network.AWS.ElasticSearch.Types.AdditionalLimit,
    module Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus,
    module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions,
    module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput,
    module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus,
    module Network.AWS.ElasticSearch.Types.CognitoOptions,
    module Network.AWS.ElasticSearch.Types.CognitoOptionsStatus,
    module Network.AWS.ElasticSearch.Types.CompatibleVersionsMap,
    module Network.AWS.ElasticSearch.Types.DescribePackagesFilter,
    module Network.AWS.ElasticSearch.Types.DomainEndpointOptions,
    module Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus,
    module Network.AWS.ElasticSearch.Types.DomainInfo,
    module Network.AWS.ElasticSearch.Types.DomainPackageDetails,
    module Network.AWS.ElasticSearch.Types.EBSOptions,
    module Network.AWS.ElasticSearch.Types.EBSOptionsStatus,
    module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig,
    module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus,
    module Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig,
    module Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus,
    module Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus,
    module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions,
    module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus,
    module Network.AWS.ElasticSearch.Types.ErrorDetails,
    module Network.AWS.ElasticSearch.Types.InstanceCountLimits,
    module Network.AWS.ElasticSearch.Types.InstanceLimits,
    module Network.AWS.ElasticSearch.Types.Limits,
    module Network.AWS.ElasticSearch.Types.LogPublishingOption,
    module Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus,
    module Network.AWS.ElasticSearch.Types.MasterUserOptions,
    module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions,
    module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus,
    module Network.AWS.ElasticSearch.Types.OptionStatus,
    module Network.AWS.ElasticSearch.Types.PackageDetails,
    module Network.AWS.ElasticSearch.Types.PackageSource,
    module Network.AWS.ElasticSearch.Types.RecurringCharge,
    module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance,
    module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering,
    module Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions,
    module Network.AWS.ElasticSearch.Types.SnapshotOptions,
    module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus,
    module Network.AWS.ElasticSearch.Types.StorageType,
    module Network.AWS.ElasticSearch.Types.StorageTypeLimit,
    module Network.AWS.ElasticSearch.Types.Tag,
    module Network.AWS.ElasticSearch.Types.UpgradeHistory,
    module Network.AWS.ElasticSearch.Types.UpgradeStepItem,
    module Network.AWS.ElasticSearch.Types.VPCDerivedInfo,
    module Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus,
    module Network.AWS.ElasticSearch.Types.VPCOptions,
    module Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
  ) where

import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainInfo
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.Limits
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.PackageDetails
import Network.AWS.ElasticSearch.Types.PackageSource
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.ElasticSearch.Types.Tag
import Network.AWS.ElasticSearch.Types.UpgradeHistory
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.ElasticSearch.Types.VPCOptions
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import Network.AWS.Lens
import Network.AWS.Prelude
