{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Product (
    module Network.AWS.Route53.Types.AccountLimit,
    module Network.AWS.Route53.Types.AlarmIdentifier,
    module Network.AWS.Route53.Types.AliasTarget,
    module Network.AWS.Route53.Types.Change,
    module Network.AWS.Route53.Types.ChangeBatch,
    module Network.AWS.Route53.Types.ChangeInfo,
    module Network.AWS.Route53.Types.CloudWatchAlarmConfiguration,
    module Network.AWS.Route53.Types.DelegationSet,
    module Network.AWS.Route53.Types.Dimension,
    module Network.AWS.Route53.Types.GeoLocation,
    module Network.AWS.Route53.Types.GeoLocationDetails,
    module Network.AWS.Route53.Types.HealthCheck,
    module Network.AWS.Route53.Types.HealthCheckConfig,
    module Network.AWS.Route53.Types.HealthCheckObservation,
    module Network.AWS.Route53.Types.HostedZone,
    module Network.AWS.Route53.Types.HostedZoneConfig,
    module Network.AWS.Route53.Types.HostedZoneLimit,
    module Network.AWS.Route53.Types.LinkedService,
    module Network.AWS.Route53.Types.QueryLoggingConfig,
    module Network.AWS.Route53.Types.ResourceRecord,
    module Network.AWS.Route53.Types.ResourceRecordSet,
    module Network.AWS.Route53.Types.ResourceTagSet,
    module Network.AWS.Route53.Types.ReusableDelegationSetLimit,
    module Network.AWS.Route53.Types.StatusReport,
    module Network.AWS.Route53.Types.Tag,
    module Network.AWS.Route53.Types.TrafficPolicy,
    module Network.AWS.Route53.Types.TrafficPolicyInstance,
    module Network.AWS.Route53.Types.TrafficPolicySummary,
    module Network.AWS.Route53.Types.VPC
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AccountLimit
import Network.AWS.Route53.Types.AlarmIdentifier
import Network.AWS.Route53.Types.AliasTarget
import Network.AWS.Route53.Types.Change
import Network.AWS.Route53.Types.ChangeBatch
import Network.AWS.Route53.Types.ChangeInfo
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.DelegationSet
import Network.AWS.Route53.Types.Dimension
import Network.AWS.Route53.Types.GeoLocation
import Network.AWS.Route53.Types.GeoLocationDetails
import Network.AWS.Route53.Types.HealthCheck
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.HealthCheckObservation
import Network.AWS.Route53.Types.HostedZone
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.HostedZoneLimit
import Network.AWS.Route53.Types.LinkedService
import Network.AWS.Route53.Types.QueryLoggingConfig
import Network.AWS.Route53.Types.ResourceRecord
import Network.AWS.Route53.Types.ResourceRecordSet
import Network.AWS.Route53.Types.ResourceTagSet
import Network.AWS.Route53.Types.ReusableDelegationSetLimit
import Network.AWS.Route53.Types.StatusReport
import Network.AWS.Route53.Types.Tag
import Network.AWS.Route53.Types.TrafficPolicy
import Network.AWS.Route53.Types.TrafficPolicyInstance
import Network.AWS.Route53.Types.TrafficPolicySummary
import Network.AWS.Route53.Types.VPC
