{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Product (
    module Network.AWS.Lightsail.Types.AddOn,
    module Network.AWS.Lightsail.Types.AddOnRequest,
    module Network.AWS.Lightsail.Types.Alarm,
    module Network.AWS.Lightsail.Types.AttachedDisk,
    module Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest,
    module Network.AWS.Lightsail.Types.AutoSnapshotDetails,
    module Network.AWS.Lightsail.Types.AvailabilityZone,
    module Network.AWS.Lightsail.Types.Blueprint,
    module Network.AWS.Lightsail.Types.Bundle,
    module Network.AWS.Lightsail.Types.CloudFormationStackRecord,
    module Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo,
    module Network.AWS.Lightsail.Types.ContactMethod,
    module Network.AWS.Lightsail.Types.DestinationInfo,
    module Network.AWS.Lightsail.Types.Disk,
    module Network.AWS.Lightsail.Types.DiskInfo,
    module Network.AWS.Lightsail.Types.DiskMap,
    module Network.AWS.Lightsail.Types.DiskSnapshot,
    module Network.AWS.Lightsail.Types.DiskSnapshotInfo,
    module Network.AWS.Lightsail.Types.Domain,
    module Network.AWS.Lightsail.Types.DomainEntry,
    module Network.AWS.Lightsail.Types.ExportSnapshotRecord,
    module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo,
    module Network.AWS.Lightsail.Types.HostKeyAttributes,
    module Network.AWS.Lightsail.Types.Instance,
    module Network.AWS.Lightsail.Types.InstanceAccessDetails,
    module Network.AWS.Lightsail.Types.InstanceEntry,
    module Network.AWS.Lightsail.Types.InstanceHardware,
    module Network.AWS.Lightsail.Types.InstanceHealthSummary,
    module Network.AWS.Lightsail.Types.InstanceNetworking,
    module Network.AWS.Lightsail.Types.InstancePortInfo,
    module Network.AWS.Lightsail.Types.InstancePortState,
    module Network.AWS.Lightsail.Types.InstanceSnapshot,
    module Network.AWS.Lightsail.Types.InstanceSnapshotInfo,
    module Network.AWS.Lightsail.Types.InstanceState,
    module Network.AWS.Lightsail.Types.KeyPair,
    module Network.AWS.Lightsail.Types.LoadBalancer,
    module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate,
    module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption,
    module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord,
    module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary,
    module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary,
    module Network.AWS.Lightsail.Types.LogEvent,
    module Network.AWS.Lightsail.Types.MetricDatapoint,
    module Network.AWS.Lightsail.Types.MonitoredResourceInfo,
    module Network.AWS.Lightsail.Types.MonthlyTransfer,
    module Network.AWS.Lightsail.Types.Operation,
    module Network.AWS.Lightsail.Types.PasswordData,
    module Network.AWS.Lightsail.Types.PendingMaintenanceAction,
    module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues,
    module Network.AWS.Lightsail.Types.PortInfo,
    module Network.AWS.Lightsail.Types.RegionInfo,
    module Network.AWS.Lightsail.Types.RelationalDatabase,
    module Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint,
    module Network.AWS.Lightsail.Types.RelationalDatabaseBundle,
    module Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint,
    module Network.AWS.Lightsail.Types.RelationalDatabaseEvent,
    module Network.AWS.Lightsail.Types.RelationalDatabaseHardware,
    module Network.AWS.Lightsail.Types.RelationalDatabaseParameter,
    module Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot,
    module Network.AWS.Lightsail.Types.ResourceLocation,
    module Network.AWS.Lightsail.Types.StaticIP,
    module Network.AWS.Lightsail.Types.Tag
  ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.AddOnRequest
import Network.AWS.Lightsail.Types.Alarm
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.Blueprint
import Network.AWS.Lightsail.Types.Bundle
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.ContactMethod
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.DiskInfo
import Network.AWS.Lightsail.Types.DiskMap
import Network.AWS.Lightsail.Types.DiskSnapshot
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.Domain
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.Instance
import Network.AWS.Lightsail.Types.InstanceAccessDetails
import Network.AWS.Lightsail.Types.InstanceEntry
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.InstancePortState
import Network.AWS.Lightsail.Types.InstanceSnapshot
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.KeyPair
import Network.AWS.Lightsail.Types.LoadBalancer
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
import Network.AWS.Lightsail.Types.LogEvent
import Network.AWS.Lightsail.Types.MetricDatapoint
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Lightsail.Types.Operation
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.PortInfo
import Network.AWS.Lightsail.Types.RegionInfo
import Network.AWS.Lightsail.Types.RelationalDatabase
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.StaticIP
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude
