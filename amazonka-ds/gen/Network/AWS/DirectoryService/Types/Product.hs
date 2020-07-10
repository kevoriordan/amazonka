{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Product (
    module Network.AWS.DirectoryService.Types.Attribute,
    module Network.AWS.DirectoryService.Types.Certificate,
    module Network.AWS.DirectoryService.Types.CertificateInfo,
    module Network.AWS.DirectoryService.Types.Computer,
    module Network.AWS.DirectoryService.Types.ConditionalForwarder,
    module Network.AWS.DirectoryService.Types.DirectoryConnectSettings,
    module Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription,
    module Network.AWS.DirectoryService.Types.DirectoryDescription,
    module Network.AWS.DirectoryService.Types.DirectoryLimits,
    module Network.AWS.DirectoryService.Types.DirectoryVPCSettings,
    module Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription,
    module Network.AWS.DirectoryService.Types.DomainController,
    module Network.AWS.DirectoryService.Types.EventTopic,
    module Network.AWS.DirectoryService.Types.IPRoute,
    module Network.AWS.DirectoryService.Types.IPRouteInfo,
    module Network.AWS.DirectoryService.Types.LDAPSSettingInfo,
    module Network.AWS.DirectoryService.Types.LogSubscription,
    module Network.AWS.DirectoryService.Types.OwnerDirectoryDescription,
    module Network.AWS.DirectoryService.Types.RadiusSettings,
    module Network.AWS.DirectoryService.Types.SchemaExtensionInfo,
    module Network.AWS.DirectoryService.Types.ShareTarget,
    module Network.AWS.DirectoryService.Types.SharedDirectory,
    module Network.AWS.DirectoryService.Types.Snapshot,
    module Network.AWS.DirectoryService.Types.SnapshotLimits,
    module Network.AWS.DirectoryService.Types.Tag,
    module Network.AWS.DirectoryService.Types.Trust,
    module Network.AWS.DirectoryService.Types.UnshareTarget
  ) where

import Network.AWS.DirectoryService.Types.Attribute
import Network.AWS.DirectoryService.Types.Certificate
import Network.AWS.DirectoryService.Types.CertificateInfo
import Network.AWS.DirectoryService.Types.Computer
import Network.AWS.DirectoryService.Types.ConditionalForwarder
import Network.AWS.DirectoryService.Types.DirectoryConnectSettings
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryDescription
import Network.AWS.DirectoryService.Types.DirectoryLimits
import Network.AWS.DirectoryService.Types.DirectoryVPCSettings
import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.DomainController
import Network.AWS.DirectoryService.Types.EventTopic
import Network.AWS.DirectoryService.Types.IPRoute
import Network.AWS.DirectoryService.Types.IPRouteInfo
import Network.AWS.DirectoryService.Types.LDAPSSettingInfo
import Network.AWS.DirectoryService.Types.LogSubscription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.SchemaExtensionInfo
import Network.AWS.DirectoryService.Types.ShareTarget
import Network.AWS.DirectoryService.Types.SharedDirectory
import Network.AWS.DirectoryService.Types.Snapshot
import Network.AWS.DirectoryService.Types.SnapshotLimits
import Network.AWS.DirectoryService.Types.Tag
import Network.AWS.DirectoryService.Types.Trust
import Network.AWS.DirectoryService.Types.UnshareTarget
import Network.AWS.Lens
import Network.AWS.Prelude
