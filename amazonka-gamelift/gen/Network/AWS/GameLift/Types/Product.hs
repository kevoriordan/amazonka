{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.Product (
    module Network.AWS.GameLift.Types.AWSCredentials,
    module Network.AWS.GameLift.Types.Alias,
    module Network.AWS.GameLift.Types.AttributeValue,
    module Network.AWS.GameLift.Types.Build,
    module Network.AWS.GameLift.Types.CertificateConfiguration,
    module Network.AWS.GameLift.Types.DesiredPlayerSession,
    module Network.AWS.GameLift.Types.EC2InstanceCounts,
    module Network.AWS.GameLift.Types.EC2InstanceLimit,
    module Network.AWS.GameLift.Types.Event,
    module Network.AWS.GameLift.Types.FleetAttributes,
    module Network.AWS.GameLift.Types.FleetCapacity,
    module Network.AWS.GameLift.Types.FleetUtilization,
    module Network.AWS.GameLift.Types.GameProperty,
    module Network.AWS.GameLift.Types.GameServer,
    module Network.AWS.GameLift.Types.GameServerGroup,
    module Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy,
    module Network.AWS.GameLift.Types.GameSession,
    module Network.AWS.GameLift.Types.GameSessionConnectionInfo,
    module Network.AWS.GameLift.Types.GameSessionDetail,
    module Network.AWS.GameLift.Types.GameSessionPlacement,
    module Network.AWS.GameLift.Types.GameSessionQueue,
    module Network.AWS.GameLift.Types.GameSessionQueueDestination,
    module Network.AWS.GameLift.Types.IPPermission,
    module Network.AWS.GameLift.Types.Instance,
    module Network.AWS.GameLift.Types.InstanceAccess,
    module Network.AWS.GameLift.Types.InstanceCredentials,
    module Network.AWS.GameLift.Types.InstanceDefinition,
    module Network.AWS.GameLift.Types.LaunchTemplateSpecification,
    module Network.AWS.GameLift.Types.MatchedPlayerSession,
    module Network.AWS.GameLift.Types.MatchmakingConfiguration,
    module Network.AWS.GameLift.Types.MatchmakingRuleSet,
    module Network.AWS.GameLift.Types.MatchmakingTicket,
    module Network.AWS.GameLift.Types.PlacedPlayerSession,
    module Network.AWS.GameLift.Types.Player,
    module Network.AWS.GameLift.Types.PlayerLatency,
    module Network.AWS.GameLift.Types.PlayerLatencyPolicy,
    module Network.AWS.GameLift.Types.PlayerSession,
    module Network.AWS.GameLift.Types.ResourceCreationLimitPolicy,
    module Network.AWS.GameLift.Types.RoutingStrategy,
    module Network.AWS.GameLift.Types.RuntimeConfiguration,
    module Network.AWS.GameLift.Types.S3Location,
    module Network.AWS.GameLift.Types.ScalingPolicy,
    module Network.AWS.GameLift.Types.Script,
    module Network.AWS.GameLift.Types.ServerProcess,
    module Network.AWS.GameLift.Types.Tag,
    module Network.AWS.GameLift.Types.TargetConfiguration,
    module Network.AWS.GameLift.Types.TargetTrackingConfiguration,
    module Network.AWS.GameLift.Types.VPCPeeringAuthorization,
    module Network.AWS.GameLift.Types.VPCPeeringConnection,
    module Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
  ) where

import Network.AWS.GameLift.Types.AWSCredentials
import Network.AWS.GameLift.Types.Alias
import Network.AWS.GameLift.Types.AttributeValue
import Network.AWS.GameLift.Types.Build
import Network.AWS.GameLift.Types.CertificateConfiguration
import Network.AWS.GameLift.Types.DesiredPlayerSession
import Network.AWS.GameLift.Types.EC2InstanceCounts
import Network.AWS.GameLift.Types.EC2InstanceLimit
import Network.AWS.GameLift.Types.Event
import Network.AWS.GameLift.Types.FleetAttributes
import Network.AWS.GameLift.Types.FleetCapacity
import Network.AWS.GameLift.Types.FleetUtilization
import Network.AWS.GameLift.Types.GameProperty
import Network.AWS.GameLift.Types.GameServer
import Network.AWS.GameLift.Types.GameServerGroup
import Network.AWS.GameLift.Types.GameServerGroupAutoScalingPolicy
import Network.AWS.GameLift.Types.GameSession
import Network.AWS.GameLift.Types.GameSessionConnectionInfo
import Network.AWS.GameLift.Types.GameSessionDetail
import Network.AWS.GameLift.Types.GameSessionPlacement
import Network.AWS.GameLift.Types.GameSessionQueue
import Network.AWS.GameLift.Types.GameSessionQueueDestination
import Network.AWS.GameLift.Types.IPPermission
import Network.AWS.GameLift.Types.Instance
import Network.AWS.GameLift.Types.InstanceAccess
import Network.AWS.GameLift.Types.InstanceCredentials
import Network.AWS.GameLift.Types.InstanceDefinition
import Network.AWS.GameLift.Types.LaunchTemplateSpecification
import Network.AWS.GameLift.Types.MatchedPlayerSession
import Network.AWS.GameLift.Types.MatchmakingConfiguration
import Network.AWS.GameLift.Types.MatchmakingRuleSet
import Network.AWS.GameLift.Types.MatchmakingTicket
import Network.AWS.GameLift.Types.PlacedPlayerSession
import Network.AWS.GameLift.Types.Player
import Network.AWS.GameLift.Types.PlayerLatency
import Network.AWS.GameLift.Types.PlayerLatencyPolicy
import Network.AWS.GameLift.Types.PlayerSession
import Network.AWS.GameLift.Types.ResourceCreationLimitPolicy
import Network.AWS.GameLift.Types.RoutingStrategy
import Network.AWS.GameLift.Types.RuntimeConfiguration
import Network.AWS.GameLift.Types.S3Location
import Network.AWS.GameLift.Types.ScalingPolicy
import Network.AWS.GameLift.Types.Script
import Network.AWS.GameLift.Types.ServerProcess
import Network.AWS.GameLift.Types.Tag
import Network.AWS.GameLift.Types.TargetConfiguration
import Network.AWS.GameLift.Types.TargetTrackingConfiguration
import Network.AWS.GameLift.Types.VPCPeeringAuthorization
import Network.AWS.GameLift.Types.VPCPeeringConnection
import Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
import Network.AWS.Lens
import Network.AWS.Prelude
