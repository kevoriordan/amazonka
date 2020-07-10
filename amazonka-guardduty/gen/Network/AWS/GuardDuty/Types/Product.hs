{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Product (
    module Network.AWS.GuardDuty.Types.AWSAPICallAction,
    module Network.AWS.GuardDuty.Types.AccessKeyDetails,
    module Network.AWS.GuardDuty.Types.AccountDetail,
    module Network.AWS.GuardDuty.Types.Action,
    module Network.AWS.GuardDuty.Types.AdminAccount,
    module Network.AWS.GuardDuty.Types.City,
    module Network.AWS.GuardDuty.Types.Condition,
    module Network.AWS.GuardDuty.Types.Country,
    module Network.AWS.GuardDuty.Types.DNSRequestAction,
    module Network.AWS.GuardDuty.Types.Destination,
    module Network.AWS.GuardDuty.Types.DestinationProperties,
    module Network.AWS.GuardDuty.Types.DomainDetails,
    module Network.AWS.GuardDuty.Types.Evidence,
    module Network.AWS.GuardDuty.Types.Finding,
    module Network.AWS.GuardDuty.Types.FindingCriteria,
    module Network.AWS.GuardDuty.Types.FindingStatistics,
    module Network.AWS.GuardDuty.Types.GeoLocation,
    module Network.AWS.GuardDuty.Types.IAMInstanceProfile,
    module Network.AWS.GuardDuty.Types.InstanceDetails,
    module Network.AWS.GuardDuty.Types.Invitation,
    module Network.AWS.GuardDuty.Types.LocalIPDetails,
    module Network.AWS.GuardDuty.Types.LocalPortDetails,
    module Network.AWS.GuardDuty.Types.Master,
    module Network.AWS.GuardDuty.Types.Member,
    module Network.AWS.GuardDuty.Types.NetworkConnectionAction,
    module Network.AWS.GuardDuty.Types.NetworkInterface,
    module Network.AWS.GuardDuty.Types.Organization,
    module Network.AWS.GuardDuty.Types.PortProbeAction,
    module Network.AWS.GuardDuty.Types.PortProbeDetail,
    module Network.AWS.GuardDuty.Types.PrivateIPAddressDetails,
    module Network.AWS.GuardDuty.Types.ProductCode,
    module Network.AWS.GuardDuty.Types.RemoteIPDetails,
    module Network.AWS.GuardDuty.Types.RemotePortDetails,
    module Network.AWS.GuardDuty.Types.Resource,
    module Network.AWS.GuardDuty.Types.SecurityGroup,
    module Network.AWS.GuardDuty.Types.ServiceInfo,
    module Network.AWS.GuardDuty.Types.SortCriteria,
    module Network.AWS.GuardDuty.Types.Tag,
    module Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail,
    module Network.AWS.GuardDuty.Types.UnprocessedAccount
  ) where

import Network.AWS.GuardDuty.Types.AWSAPICallAction
import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.AccountDetail
import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.AdminAccount
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.DNSRequestAction
import Network.AWS.GuardDuty.Types.Destination
import Network.AWS.GuardDuty.Types.DestinationProperties
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.GuardDuty.Types.Finding
import Network.AWS.GuardDuty.Types.FindingCriteria
import Network.AWS.GuardDuty.Types.FindingStatistics
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.IAMInstanceProfile
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.Invitation
import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.Master
import Network.AWS.GuardDuty.Types.Member
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.Organization
import Network.AWS.GuardDuty.Types.PortProbeAction
import Network.AWS.GuardDuty.Types.PortProbeDetail
import Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.GuardDuty.Types.SortCriteria
import Network.AWS.GuardDuty.Types.Tag
import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.GuardDuty.Types.UnprocessedAccount
import Network.AWS.Lens
import Network.AWS.Prelude
