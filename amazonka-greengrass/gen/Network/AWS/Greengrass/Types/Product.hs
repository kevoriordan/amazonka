{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.Product (
    module Network.AWS.Greengrass.Types.BulkDeployment,
    module Network.AWS.Greengrass.Types.BulkDeploymentMetrics,
    module Network.AWS.Greengrass.Types.BulkDeploymentResult,
    module Network.AWS.Greengrass.Types.ConnectivityInfo,
    module Network.AWS.Greengrass.Types.Connector,
    module Network.AWS.Greengrass.Types.ConnectorDefinitionVersion,
    module Network.AWS.Greengrass.Types.Core,
    module Network.AWS.Greengrass.Types.CoreDefinitionVersion,
    module Network.AWS.Greengrass.Types.DefinitionInformation,
    module Network.AWS.Greengrass.Types.Deployment,
    module Network.AWS.Greengrass.Types.Device,
    module Network.AWS.Greengrass.Types.DeviceDefinitionVersion,
    module Network.AWS.Greengrass.Types.ErrorDetail,
    module Network.AWS.Greengrass.Types.Function,
    module Network.AWS.Greengrass.Types.FunctionConfiguration,
    module Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment,
    module Network.AWS.Greengrass.Types.FunctionDefaultConfig,
    module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig,
    module Network.AWS.Greengrass.Types.FunctionDefinitionVersion,
    module Network.AWS.Greengrass.Types.FunctionExecutionConfig,
    module Network.AWS.Greengrass.Types.FunctionRunAsConfig,
    module Network.AWS.Greengrass.Types.GreengrassLogger,
    module Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties,
    module Network.AWS.Greengrass.Types.GroupInformation,
    module Network.AWS.Greengrass.Types.GroupOwnerSetting,
    module Network.AWS.Greengrass.Types.GroupVersion,
    module Network.AWS.Greengrass.Types.LocalDeviceResourceData,
    module Network.AWS.Greengrass.Types.LocalVolumeResourceData,
    module Network.AWS.Greengrass.Types.LoggerDefinitionVersion,
    module Network.AWS.Greengrass.Types.Resource,
    module Network.AWS.Greengrass.Types.ResourceAccessPolicy,
    module Network.AWS.Greengrass.Types.ResourceDataContainer,
    module Network.AWS.Greengrass.Types.ResourceDefinitionVersion,
    module Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting,
    module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData,
    module Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData,
    module Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData,
    module Network.AWS.Greengrass.Types.Subscription,
    module Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion,
    module Network.AWS.Greengrass.Types.VersionInformation
  ) where

import Network.AWS.Greengrass.Types.BulkDeployment
import Network.AWS.Greengrass.Types.BulkDeploymentMetrics
import Network.AWS.Greengrass.Types.BulkDeploymentResult
import Network.AWS.Greengrass.Types.ConnectivityInfo
import Network.AWS.Greengrass.Types.Connector
import Network.AWS.Greengrass.Types.ConnectorDefinitionVersion
import Network.AWS.Greengrass.Types.Core
import Network.AWS.Greengrass.Types.CoreDefinitionVersion
import Network.AWS.Greengrass.Types.DefinitionInformation
import Network.AWS.Greengrass.Types.Deployment
import Network.AWS.Greengrass.Types.Device
import Network.AWS.Greengrass.Types.DeviceDefinitionVersion
import Network.AWS.Greengrass.Types.ErrorDetail
import Network.AWS.Greengrass.Types.Function
import Network.AWS.Greengrass.Types.FunctionConfiguration
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import Network.AWS.Greengrass.Types.FunctionDefaultConfig
import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import Network.AWS.Greengrass.Types.FunctionDefinitionVersion
import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import Network.AWS.Greengrass.Types.GreengrassLogger
import Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
import Network.AWS.Greengrass.Types.GroupInformation
import Network.AWS.Greengrass.Types.GroupOwnerSetting
import Network.AWS.Greengrass.Types.GroupVersion
import Network.AWS.Greengrass.Types.LocalDeviceResourceData
import Network.AWS.Greengrass.Types.LocalVolumeResourceData
import Network.AWS.Greengrass.Types.LoggerDefinitionVersion
import Network.AWS.Greengrass.Types.Resource
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import Network.AWS.Greengrass.Types.ResourceDataContainer
import Network.AWS.Greengrass.Types.ResourceDefinitionVersion
import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
import Network.AWS.Greengrass.Types.SecretsManagerSecretResourceData
import Network.AWS.Greengrass.Types.Subscription
import Network.AWS.Greengrass.Types.SubscriptionDefinitionVersion
import Network.AWS.Greengrass.Types.VersionInformation
import Network.AWS.Lens
import Network.AWS.Prelude
