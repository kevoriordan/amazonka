{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Product (
    module Network.AWS.AppStream.Types.AccessEndpoint,
    module Network.AWS.AppStream.Types.Application,
    module Network.AWS.AppStream.Types.ApplicationSettings,
    module Network.AWS.AppStream.Types.ApplicationSettingsResponse,
    module Network.AWS.AppStream.Types.ComputeCapacity,
    module Network.AWS.AppStream.Types.ComputeCapacityStatus,
    module Network.AWS.AppStream.Types.DirectoryConfig,
    module Network.AWS.AppStream.Types.DomainJoinInfo,
    module Network.AWS.AppStream.Types.Fleet,
    module Network.AWS.AppStream.Types.FleetError,
    module Network.AWS.AppStream.Types.Image,
    module Network.AWS.AppStream.Types.ImageBuilder,
    module Network.AWS.AppStream.Types.ImageBuilderStateChangeReason,
    module Network.AWS.AppStream.Types.ImagePermissions,
    module Network.AWS.AppStream.Types.ImageStateChangeReason,
    module Network.AWS.AppStream.Types.LastReportGenerationExecutionError,
    module Network.AWS.AppStream.Types.NetworkAccessConfiguration,
    module Network.AWS.AppStream.Types.ResourceError,
    module Network.AWS.AppStream.Types.ServiceAccountCredentials,
    module Network.AWS.AppStream.Types.Session,
    module Network.AWS.AppStream.Types.SharedImagePermissions,
    module Network.AWS.AppStream.Types.Stack,
    module Network.AWS.AppStream.Types.StackError,
    module Network.AWS.AppStream.Types.StorageConnector,
    module Network.AWS.AppStream.Types.UsageReportSubscription,
    module Network.AWS.AppStream.Types.User,
    module Network.AWS.AppStream.Types.UserSetting,
    module Network.AWS.AppStream.Types.UserStackAssociation,
    module Network.AWS.AppStream.Types.UserStackAssociationError,
    module Network.AWS.AppStream.Types.VPCConfig
  ) where

import Network.AWS.AppStream.Types.AccessEndpoint
import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ApplicationSettings
import Network.AWS.AppStream.Types.ApplicationSettingsResponse
import Network.AWS.AppStream.Types.ComputeCapacity
import Network.AWS.AppStream.Types.ComputeCapacityStatus
import Network.AWS.AppStream.Types.DirectoryConfig
import Network.AWS.AppStream.Types.DomainJoinInfo
import Network.AWS.AppStream.Types.Fleet
import Network.AWS.AppStream.Types.FleetError
import Network.AWS.AppStream.Types.Image
import Network.AWS.AppStream.Types.ImageBuilder
import Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.LastReportGenerationExecutionError
import Network.AWS.AppStream.Types.NetworkAccessConfiguration
import Network.AWS.AppStream.Types.ResourceError
import Network.AWS.AppStream.Types.ServiceAccountCredentials
import Network.AWS.AppStream.Types.Session
import Network.AWS.AppStream.Types.SharedImagePermissions
import Network.AWS.AppStream.Types.Stack
import Network.AWS.AppStream.Types.StackError
import Network.AWS.AppStream.Types.StorageConnector
import Network.AWS.AppStream.Types.UsageReportSubscription
import Network.AWS.AppStream.Types.User
import Network.AWS.AppStream.Types.UserSetting
import Network.AWS.AppStream.Types.UserStackAssociation
import Network.AWS.AppStream.Types.UserStackAssociationError
import Network.AWS.AppStream.Types.VPCConfig
import Network.AWS.Lens
import Network.AWS.Prelude
