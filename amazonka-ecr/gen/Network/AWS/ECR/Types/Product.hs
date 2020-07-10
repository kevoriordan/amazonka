{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Product (
    module Network.AWS.ECR.Types.Attribute,
    module Network.AWS.ECR.Types.AuthorizationData,
    module Network.AWS.ECR.Types.DescribeImagesFilter,
    module Network.AWS.ECR.Types.Image,
    module Network.AWS.ECR.Types.ImageDetail,
    module Network.AWS.ECR.Types.ImageFailure,
    module Network.AWS.ECR.Types.ImageIdentifier,
    module Network.AWS.ECR.Types.ImageScanFinding,
    module Network.AWS.ECR.Types.ImageScanFindings,
    module Network.AWS.ECR.Types.ImageScanFindingsSummary,
    module Network.AWS.ECR.Types.ImageScanStatus,
    module Network.AWS.ECR.Types.ImageScanningConfiguration,
    module Network.AWS.ECR.Types.Layer,
    module Network.AWS.ECR.Types.LayerFailure,
    module Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter,
    module Network.AWS.ECR.Types.LifecyclePolicyPreviewResult,
    module Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary,
    module Network.AWS.ECR.Types.LifecyclePolicyRuleAction,
    module Network.AWS.ECR.Types.ListImagesFilter,
    module Network.AWS.ECR.Types.Repository,
    module Network.AWS.ECR.Types.Tag
  ) where

import Network.AWS.ECR.Types.Attribute
import Network.AWS.ECR.Types.AuthorizationData
import Network.AWS.ECR.Types.DescribeImagesFilter
import Network.AWS.ECR.Types.Image
import Network.AWS.ECR.Types.ImageDetail
import Network.AWS.ECR.Types.ImageFailure
import Network.AWS.ECR.Types.ImageIdentifier
import Network.AWS.ECR.Types.ImageScanFinding
import Network.AWS.ECR.Types.ImageScanFindings
import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.Layer
import Network.AWS.ECR.Types.LayerFailure
import Network.AWS.ECR.Types.LifecyclePolicyPreviewFilter
import Network.AWS.ECR.Types.LifecyclePolicyPreviewResult
import Network.AWS.ECR.Types.LifecyclePolicyPreviewSummary
import Network.AWS.ECR.Types.LifecyclePolicyRuleAction
import Network.AWS.ECR.Types.ListImagesFilter
import Network.AWS.ECR.Types.Repository
import Network.AWS.ECR.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude
