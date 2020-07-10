{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.Product (
    module Network.AWS.APIGateway.Types.APIKey,
    module Network.AWS.APIGateway.Types.APIStage,
    module Network.AWS.APIGateway.Types.AccessLogSettings,
    module Network.AWS.APIGateway.Types.Account,
    module Network.AWS.APIGateway.Types.Authorizer,
    module Network.AWS.APIGateway.Types.BasePathMapping,
    module Network.AWS.APIGateway.Types.CanarySettings,
    module Network.AWS.APIGateway.Types.ClientCertificate,
    module Network.AWS.APIGateway.Types.Deployment,
    module Network.AWS.APIGateway.Types.DeploymentCanarySettings,
    module Network.AWS.APIGateway.Types.DocumentationPart,
    module Network.AWS.APIGateway.Types.DocumentationPartLocation,
    module Network.AWS.APIGateway.Types.DocumentationVersion,
    module Network.AWS.APIGateway.Types.DomainName,
    module Network.AWS.APIGateway.Types.EndpointConfiguration,
    module Network.AWS.APIGateway.Types.GatewayResponse,
    module Network.AWS.APIGateway.Types.Integration,
    module Network.AWS.APIGateway.Types.IntegrationResponse,
    module Network.AWS.APIGateway.Types.Method,
    module Network.AWS.APIGateway.Types.MethodResponse,
    module Network.AWS.APIGateway.Types.MethodSetting,
    module Network.AWS.APIGateway.Types.MethodSnapshot,
    module Network.AWS.APIGateway.Types.Model,
    module Network.AWS.APIGateway.Types.PatchOperation,
    module Network.AWS.APIGateway.Types.QuotaSettings,
    module Network.AWS.APIGateway.Types.RequestValidator,
    module Network.AWS.APIGateway.Types.Resource,
    module Network.AWS.APIGateway.Types.RestAPI,
    module Network.AWS.APIGateway.Types.SDKConfigurationProperty,
    module Network.AWS.APIGateway.Types.SDKType,
    module Network.AWS.APIGateway.Types.Stage,
    module Network.AWS.APIGateway.Types.StageKey,
    module Network.AWS.APIGateway.Types.ThrottleSettings,
    module Network.AWS.APIGateway.Types.Usage,
    module Network.AWS.APIGateway.Types.UsagePlan,
    module Network.AWS.APIGateway.Types.UsagePlanKey,
    module Network.AWS.APIGateway.Types.VPCLink
  ) where

import Network.AWS.APIGateway.Types.APIKey
import Network.AWS.APIGateway.Types.APIStage
import Network.AWS.APIGateway.Types.AccessLogSettings
import Network.AWS.APIGateway.Types.Account
import Network.AWS.APIGateway.Types.Authorizer
import Network.AWS.APIGateway.Types.BasePathMapping
import Network.AWS.APIGateway.Types.CanarySettings
import Network.AWS.APIGateway.Types.ClientCertificate
import Network.AWS.APIGateway.Types.Deployment
import Network.AWS.APIGateway.Types.DeploymentCanarySettings
import Network.AWS.APIGateway.Types.DocumentationPart
import Network.AWS.APIGateway.Types.DocumentationPartLocation
import Network.AWS.APIGateway.Types.DocumentationVersion
import Network.AWS.APIGateway.Types.DomainName
import Network.AWS.APIGateway.Types.EndpointConfiguration
import Network.AWS.APIGateway.Types.GatewayResponse
import Network.AWS.APIGateway.Types.Integration
import Network.AWS.APIGateway.Types.IntegrationResponse
import Network.AWS.APIGateway.Types.Method
import Network.AWS.APIGateway.Types.MethodResponse
import Network.AWS.APIGateway.Types.MethodSetting
import Network.AWS.APIGateway.Types.MethodSnapshot
import Network.AWS.APIGateway.Types.Model
import Network.AWS.APIGateway.Types.PatchOperation
import Network.AWS.APIGateway.Types.QuotaSettings
import Network.AWS.APIGateway.Types.RequestValidator
import Network.AWS.APIGateway.Types.Resource
import Network.AWS.APIGateway.Types.RestAPI
import Network.AWS.APIGateway.Types.SDKConfigurationProperty
import Network.AWS.APIGateway.Types.SDKType
import Network.AWS.APIGateway.Types.Stage
import Network.AWS.APIGateway.Types.StageKey
import Network.AWS.APIGateway.Types.ThrottleSettings
import Network.AWS.APIGateway.Types.Usage
import Network.AWS.APIGateway.Types.UsagePlan
import Network.AWS.APIGateway.Types.UsagePlanKey
import Network.AWS.APIGateway.Types.VPCLink
import Network.AWS.Lens
import Network.AWS.Prelude
