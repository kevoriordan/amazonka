{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Product (
    module Network.AWS.ElasticBeanstalk.Types.ApplicationDescription,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage,
    module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig,
    module Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup,
    module Network.AWS.ElasticBeanstalk.Types.BuildConfiguration,
    module Network.AWS.ElasticBeanstalk.Types.Builder,
    module Network.AWS.ElasticBeanstalk.Types.CPUUtilization,
    module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription,
    module Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting,
    module Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription,
    module Network.AWS.ElasticBeanstalk.Types.CustomAMI,
    module Network.AWS.ElasticBeanstalk.Types.Deployment,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentLink,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription,
    module Network.AWS.ElasticBeanstalk.Types.EnvironmentTier,
    module Network.AWS.ElasticBeanstalk.Types.EventDescription,
    module Network.AWS.ElasticBeanstalk.Types.Instance,
    module Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary,
    module Network.AWS.ElasticBeanstalk.Types.Latency,
    module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration,
    module Network.AWS.ElasticBeanstalk.Types.LaunchTemplate,
    module Network.AWS.ElasticBeanstalk.Types.Listener,
    module Network.AWS.ElasticBeanstalk.Types.LoadBalancer,
    module Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription,
    module Network.AWS.ElasticBeanstalk.Types.ManagedAction,
    module Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem,
    module Network.AWS.ElasticBeanstalk.Types.MaxAgeRule,
    module Network.AWS.ElasticBeanstalk.Types.MaxCountRule,
    module Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex,
    module Network.AWS.ElasticBeanstalk.Types.OptionSpecification,
    module Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary,
    module Network.AWS.ElasticBeanstalk.Types.PlatformDescription,
    module Network.AWS.ElasticBeanstalk.Types.PlatformFilter,
    module Network.AWS.ElasticBeanstalk.Types.PlatformFramework,
    module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage,
    module Network.AWS.ElasticBeanstalk.Types.PlatformSummary,
    module Network.AWS.ElasticBeanstalk.Types.Queue,
    module Network.AWS.ElasticBeanstalk.Types.ResourceQuota,
    module Network.AWS.ElasticBeanstalk.Types.ResourceQuotas,
    module Network.AWS.ElasticBeanstalk.Types.S3Location,
    module Network.AWS.ElasticBeanstalk.Types.SearchFilter,
    module Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth,
    module Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription,
    module Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation,
    module Network.AWS.ElasticBeanstalk.Types.SourceConfiguration,
    module Network.AWS.ElasticBeanstalk.Types.StatusCodes,
    module Network.AWS.ElasticBeanstalk.Types.SystemStatus,
    module Network.AWS.ElasticBeanstalk.Types.Tag,
    module Network.AWS.ElasticBeanstalk.Types.Trigger,
    module Network.AWS.ElasticBeanstalk.Types.ValidationMessage
  ) where

import Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationMetrics
import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescriptionMessage
import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionLifecycleConfig
import Network.AWS.ElasticBeanstalk.Types.AutoScalingGroup
import Network.AWS.ElasticBeanstalk.Types.BuildConfiguration
import Network.AWS.ElasticBeanstalk.Types.Builder
import Network.AWS.ElasticBeanstalk.Types.CPUUtilization
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionDescription
import Network.AWS.ElasticBeanstalk.Types.ConfigurationOptionSetting
import Network.AWS.ElasticBeanstalk.Types.ConfigurationSettingsDescription
import Network.AWS.ElasticBeanstalk.Types.CustomAMI
import Network.AWS.ElasticBeanstalk.Types.Deployment
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentDescriptionsMessage
import Network.AWS.ElasticBeanstalk.Types.EnvironmentInfoDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentLink
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourceDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentResourcesDescription
import Network.AWS.ElasticBeanstalk.Types.EnvironmentTier
import Network.AWS.ElasticBeanstalk.Types.EventDescription
import Network.AWS.ElasticBeanstalk.Types.Instance
import Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
import Network.AWS.ElasticBeanstalk.Types.Latency
import Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
import Network.AWS.ElasticBeanstalk.Types.LaunchTemplate
import Network.AWS.ElasticBeanstalk.Types.Listener
import Network.AWS.ElasticBeanstalk.Types.LoadBalancer
import Network.AWS.ElasticBeanstalk.Types.LoadBalancerDescription
import Network.AWS.ElasticBeanstalk.Types.ManagedAction
import Network.AWS.ElasticBeanstalk.Types.ManagedActionHistoryItem
import Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
import Network.AWS.ElasticBeanstalk.Types.MaxCountRule
import Network.AWS.ElasticBeanstalk.Types.OptionRestrictionRegex
import Network.AWS.ElasticBeanstalk.Types.OptionSpecification
import Network.AWS.ElasticBeanstalk.Types.PlatformBranchSummary
import Network.AWS.ElasticBeanstalk.Types.PlatformDescription
import Network.AWS.ElasticBeanstalk.Types.PlatformFilter
import Network.AWS.ElasticBeanstalk.Types.PlatformFramework
import Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
import Network.AWS.ElasticBeanstalk.Types.PlatformSummary
import Network.AWS.ElasticBeanstalk.Types.Queue
import Network.AWS.ElasticBeanstalk.Types.ResourceQuota
import Network.AWS.ElasticBeanstalk.Types.ResourceQuotas
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SearchFilter
import Network.AWS.ElasticBeanstalk.Types.SingleInstanceHealth
import Network.AWS.ElasticBeanstalk.Types.SolutionStackDescription
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import Network.AWS.ElasticBeanstalk.Types.SourceConfiguration
import Network.AWS.ElasticBeanstalk.Types.StatusCodes
import Network.AWS.ElasticBeanstalk.Types.SystemStatus
import Network.AWS.ElasticBeanstalk.Types.Tag
import Network.AWS.ElasticBeanstalk.Types.Trigger
import Network.AWS.ElasticBeanstalk.Types.ValidationMessage
import Network.AWS.Lens
import Network.AWS.Prelude
