{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Product (
    module Network.AWS.DMS.Types.AccountQuota,
    module Network.AWS.DMS.Types.AvailabilityZone,
    module Network.AWS.DMS.Types.Certificate,
    module Network.AWS.DMS.Types.Connection,
    module Network.AWS.DMS.Types.DmsTransferSettings,
    module Network.AWS.DMS.Types.DynamoDBSettings,
    module Network.AWS.DMS.Types.ElasticsearchSettings,
    module Network.AWS.DMS.Types.Endpoint,
    module Network.AWS.DMS.Types.Event,
    module Network.AWS.DMS.Types.EventCategoryGroup,
    module Network.AWS.DMS.Types.EventSubscription,
    module Network.AWS.DMS.Types.Filter,
    module Network.AWS.DMS.Types.KafkaSettings,
    module Network.AWS.DMS.Types.KinesisSettings,
    module Network.AWS.DMS.Types.MongoDBSettings,
    module Network.AWS.DMS.Types.NeptuneSettings,
    module Network.AWS.DMS.Types.OrderableReplicationInstance,
    module Network.AWS.DMS.Types.PendingMaintenanceAction,
    module Network.AWS.DMS.Types.RedshiftSettings,
    module Network.AWS.DMS.Types.RefreshSchemasStatus,
    module Network.AWS.DMS.Types.ReplicationInstance,
    module Network.AWS.DMS.Types.ReplicationInstanceTaskLog,
    module Network.AWS.DMS.Types.ReplicationPendingModifiedValues,
    module Network.AWS.DMS.Types.ReplicationSubnetGroup,
    module Network.AWS.DMS.Types.ReplicationTask,
    module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult,
    module Network.AWS.DMS.Types.ReplicationTaskStats,
    module Network.AWS.DMS.Types.ResourcePendingMaintenanceActions,
    module Network.AWS.DMS.Types.S3Settings,
    module Network.AWS.DMS.Types.Subnet,
    module Network.AWS.DMS.Types.SupportedEndpointType,
    module Network.AWS.DMS.Types.TableStatistics,
    module Network.AWS.DMS.Types.TableToReload,
    module Network.AWS.DMS.Types.Tag,
    module Network.AWS.DMS.Types.VPCSecurityGroupMembership
  ) where

import Network.AWS.DMS.Types.AccountQuota
import Network.AWS.DMS.Types.AvailabilityZone
import Network.AWS.DMS.Types.Certificate
import Network.AWS.DMS.Types.Connection
import Network.AWS.DMS.Types.DmsTransferSettings
import Network.AWS.DMS.Types.DynamoDBSettings
import Network.AWS.DMS.Types.ElasticsearchSettings
import Network.AWS.DMS.Types.Endpoint
import Network.AWS.DMS.Types.Event
import Network.AWS.DMS.Types.EventCategoryGroup
import Network.AWS.DMS.Types.EventSubscription
import Network.AWS.DMS.Types.Filter
import Network.AWS.DMS.Types.KafkaSettings
import Network.AWS.DMS.Types.KinesisSettings
import Network.AWS.DMS.Types.MongoDBSettings
import Network.AWS.DMS.Types.NeptuneSettings
import Network.AWS.DMS.Types.OrderableReplicationInstance
import Network.AWS.DMS.Types.PendingMaintenanceAction
import Network.AWS.DMS.Types.RedshiftSettings
import Network.AWS.DMS.Types.RefreshSchemasStatus
import Network.AWS.DMS.Types.ReplicationInstance
import Network.AWS.DMS.Types.ReplicationInstanceTaskLog
import Network.AWS.DMS.Types.ReplicationPendingModifiedValues
import Network.AWS.DMS.Types.ReplicationSubnetGroup
import Network.AWS.DMS.Types.ReplicationTask
import Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
import Network.AWS.DMS.Types.S3Settings
import Network.AWS.DMS.Types.Subnet
import Network.AWS.DMS.Types.SupportedEndpointType
import Network.AWS.DMS.Types.TableStatistics
import Network.AWS.DMS.Types.TableToReload
import Network.AWS.DMS.Types.Tag
import Network.AWS.DMS.Types.VPCSecurityGroupMembership
import Network.AWS.Lens
import Network.AWS.Prelude
