{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Product (
    module Network.AWS.ServiceCatalog.Types.AccessLevelFilter,
    module Network.AWS.ServiceCatalog.Types.BudgetDetail,
    module Network.AWS.ServiceCatalog.Types.CloudWatchDashboard,
    module Network.AWS.ServiceCatalog.Types.ConstraintDetail,
    module Network.AWS.ServiceCatalog.Types.ConstraintSummary,
    module Network.AWS.ServiceCatalog.Types.ExecutionParameter,
    module Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation,
    module Network.AWS.ServiceCatalog.Types.LaunchPathSummary,
    module Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter,
    module Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters,
    module Network.AWS.ServiceCatalog.Types.OrganizationNode,
    module Network.AWS.ServiceCatalog.Types.ParameterConstraints,
    module Network.AWS.ServiceCatalog.Types.PortfolioDetail,
    module Network.AWS.ServiceCatalog.Types.Principal,
    module Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue,
    module Network.AWS.ServiceCatalog.Types.ProductViewDetail,
    module Network.AWS.ServiceCatalog.Types.ProductViewSummary,
    module Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute,
    module Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail,
    module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails,
    module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary,
    module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView,
    module Network.AWS.ServiceCatalog.Types.ProvisioningParameter,
    module Network.AWS.ServiceCatalog.Types.ProvisioningPreferences,
    module Network.AWS.ServiceCatalog.Types.RecordDetail,
    module Network.AWS.ServiceCatalog.Types.RecordError,
    module Network.AWS.ServiceCatalog.Types.RecordOutput,
    module Network.AWS.ServiceCatalog.Types.RecordTag,
    module Network.AWS.ServiceCatalog.Types.ResourceChange,
    module Network.AWS.ServiceCatalog.Types.ResourceChangeDetail,
    module Network.AWS.ServiceCatalog.Types.ResourceDetail,
    module Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition,
    module Network.AWS.ServiceCatalog.Types.ServiceActionAssociation,
    module Network.AWS.ServiceCatalog.Types.ServiceActionDetail,
    module Network.AWS.ServiceCatalog.Types.ServiceActionSummary,
    module Network.AWS.ServiceCatalog.Types.ShareDetails,
    module Network.AWS.ServiceCatalog.Types.ShareError,
    module Network.AWS.ServiceCatalog.Types.StackInstance,
    module Network.AWS.ServiceCatalog.Types.Tag,
    module Network.AWS.ServiceCatalog.Types.TagOptionDetail,
    module Network.AWS.ServiceCatalog.Types.TagOptionSummary,
    module Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter,
    module Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences,
    module Network.AWS.ServiceCatalog.Types.UsageInstruction
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.AccessLevelFilter
import Network.AWS.ServiceCatalog.Types.BudgetDetail
import Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
import Network.AWS.ServiceCatalog.Types.ConstraintDetail
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.ExecutionParameter
import Network.AWS.ServiceCatalog.Types.FailedServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.LaunchPathSummary
import Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
import Network.AWS.ServiceCatalog.Types.ListTagOptionsFilters
import Network.AWS.ServiceCatalog.Types.OrganizationNode
import Network.AWS.ServiceCatalog.Types.ParameterConstraints
import Network.AWS.ServiceCatalog.Types.PortfolioDetail
import Network.AWS.ServiceCatalog.Types.Principal
import Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
import Network.AWS.ServiceCatalog.Types.ProductViewDetail
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.ProvisionedProductAttribute
import Network.AWS.ServiceCatalog.Types.ProvisionedProductDetail
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanDetails
import Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactView
import Network.AWS.ServiceCatalog.Types.ProvisioningParameter
import Network.AWS.ServiceCatalog.Types.ProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.RecordDetail
import Network.AWS.ServiceCatalog.Types.RecordError
import Network.AWS.ServiceCatalog.Types.RecordOutput
import Network.AWS.ServiceCatalog.Types.RecordTag
import Network.AWS.ServiceCatalog.Types.ResourceChange
import Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
import Network.AWS.ServiceCatalog.Types.ResourceDetail
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
import Network.AWS.ServiceCatalog.Types.ServiceActionAssociation
import Network.AWS.ServiceCatalog.Types.ServiceActionDetail
import Network.AWS.ServiceCatalog.Types.ServiceActionSummary
import Network.AWS.ServiceCatalog.Types.ShareDetails
import Network.AWS.ServiceCatalog.Types.ShareError
import Network.AWS.ServiceCatalog.Types.StackInstance
import Network.AWS.ServiceCatalog.Types.Tag
import Network.AWS.ServiceCatalog.Types.TagOptionDetail
import Network.AWS.ServiceCatalog.Types.TagOptionSummary
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningParameter
import Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
import Network.AWS.ServiceCatalog.Types.UsageInstruction
