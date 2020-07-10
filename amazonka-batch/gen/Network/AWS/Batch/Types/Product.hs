{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.Product (
    module Network.AWS.Batch.Types.ArrayProperties,
    module Network.AWS.Batch.Types.ArrayPropertiesDetail,
    module Network.AWS.Batch.Types.ArrayPropertiesSummary,
    module Network.AWS.Batch.Types.AttemptContainerDetail,
    module Network.AWS.Batch.Types.AttemptDetail,
    module Network.AWS.Batch.Types.ComputeEnvironmentDetail,
    module Network.AWS.Batch.Types.ComputeEnvironmentOrder,
    module Network.AWS.Batch.Types.ComputeResource,
    module Network.AWS.Batch.Types.ComputeResourceUpdate,
    module Network.AWS.Batch.Types.ContainerDetail,
    module Network.AWS.Batch.Types.ContainerOverrides,
    module Network.AWS.Batch.Types.ContainerProperties,
    module Network.AWS.Batch.Types.ContainerSummary,
    module Network.AWS.Batch.Types.Device,
    module Network.AWS.Batch.Types.Host,
    module Network.AWS.Batch.Types.JobDefinition,
    module Network.AWS.Batch.Types.JobDependency,
    module Network.AWS.Batch.Types.JobDetail,
    module Network.AWS.Batch.Types.JobQueueDetail,
    module Network.AWS.Batch.Types.JobSummary,
    module Network.AWS.Batch.Types.JobTimeout,
    module Network.AWS.Batch.Types.KeyValuePair,
    module Network.AWS.Batch.Types.LaunchTemplateSpecification,
    module Network.AWS.Batch.Types.LinuxParameters,
    module Network.AWS.Batch.Types.MountPoint,
    module Network.AWS.Batch.Types.NetworkInterface,
    module Network.AWS.Batch.Types.NodeDetails,
    module Network.AWS.Batch.Types.NodeOverrides,
    module Network.AWS.Batch.Types.NodeProperties,
    module Network.AWS.Batch.Types.NodePropertiesSummary,
    module Network.AWS.Batch.Types.NodePropertyOverride,
    module Network.AWS.Batch.Types.NodeRangeProperty,
    module Network.AWS.Batch.Types.ResourceRequirement,
    module Network.AWS.Batch.Types.RetryStrategy,
    module Network.AWS.Batch.Types.Ulimit,
    module Network.AWS.Batch.Types.Volume
  ) where

import Network.AWS.Batch.Types.ArrayProperties
import Network.AWS.Batch.Types.ArrayPropertiesDetail
import Network.AWS.Batch.Types.ArrayPropertiesSummary
import Network.AWS.Batch.Types.AttemptContainerDetail
import Network.AWS.Batch.Types.AttemptDetail
import Network.AWS.Batch.Types.ComputeEnvironmentDetail
import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.ComputeResource
import Network.AWS.Batch.Types.ComputeResourceUpdate
import Network.AWS.Batch.Types.ContainerDetail
import Network.AWS.Batch.Types.ContainerOverrides
import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Batch.Types.ContainerSummary
import Network.AWS.Batch.Types.Device
import Network.AWS.Batch.Types.Host
import Network.AWS.Batch.Types.JobDefinition
import Network.AWS.Batch.Types.JobDependency
import Network.AWS.Batch.Types.JobDetail
import Network.AWS.Batch.Types.JobQueueDetail
import Network.AWS.Batch.Types.JobSummary
import Network.AWS.Batch.Types.JobTimeout
import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeOverrides
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.NodePropertiesSummary
import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import Network.AWS.Lens
import Network.AWS.Prelude
