{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Product (
    module Network.AWS.Connect.Types.ChatMessage,
    module Network.AWS.Connect.Types.ContactFlowSummary,
    module Network.AWS.Connect.Types.Credentials,
    module Network.AWS.Connect.Types.CurrentMetric,
    module Network.AWS.Connect.Types.CurrentMetricData,
    module Network.AWS.Connect.Types.CurrentMetricResult,
    module Network.AWS.Connect.Types.Dimensions,
    module Network.AWS.Connect.Types.Filters,
    module Network.AWS.Connect.Types.HierarchyGroup,
    module Network.AWS.Connect.Types.HierarchyGroupSummary,
    module Network.AWS.Connect.Types.HierarchyLevel,
    module Network.AWS.Connect.Types.HierarchyPath,
    module Network.AWS.Connect.Types.HierarchyStructure,
    module Network.AWS.Connect.Types.HistoricalMetric,
    module Network.AWS.Connect.Types.HistoricalMetricData,
    module Network.AWS.Connect.Types.HistoricalMetricResult,
    module Network.AWS.Connect.Types.HoursOfOperationSummary,
    module Network.AWS.Connect.Types.ParticipantDetails,
    module Network.AWS.Connect.Types.PhoneNumberSummary,
    module Network.AWS.Connect.Types.QueueReference,
    module Network.AWS.Connect.Types.QueueSummary,
    module Network.AWS.Connect.Types.RoutingProfileSummary,
    module Network.AWS.Connect.Types.SecurityProfileSummary,
    module Network.AWS.Connect.Types.Threshold,
    module Network.AWS.Connect.Types.User,
    module Network.AWS.Connect.Types.UserIdentityInfo,
    module Network.AWS.Connect.Types.UserPhoneConfig,
    module Network.AWS.Connect.Types.UserSummary
  ) where

import Network.AWS.Connect.Types.ChatMessage
import Network.AWS.Connect.Types.ContactFlowSummary
import Network.AWS.Connect.Types.Credentials
import Network.AWS.Connect.Types.CurrentMetric
import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.CurrentMetricResult
import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Connect.Types.Filters
import Network.AWS.Connect.Types.HierarchyGroup
import Network.AWS.Connect.Types.HierarchyGroupSummary
import Network.AWS.Connect.Types.HierarchyLevel
import Network.AWS.Connect.Types.HierarchyPath
import Network.AWS.Connect.Types.HierarchyStructure
import Network.AWS.Connect.Types.HistoricalMetric
import Network.AWS.Connect.Types.HistoricalMetricData
import Network.AWS.Connect.Types.HistoricalMetricResult
import Network.AWS.Connect.Types.HoursOfOperationSummary
import Network.AWS.Connect.Types.ParticipantDetails
import Network.AWS.Connect.Types.PhoneNumberSummary
import Network.AWS.Connect.Types.QueueReference
import Network.AWS.Connect.Types.QueueSummary
import Network.AWS.Connect.Types.RoutingProfileSummary
import Network.AWS.Connect.Types.SecurityProfileSummary
import Network.AWS.Connect.Types.Threshold
import Network.AWS.Connect.Types.User
import Network.AWS.Connect.Types.UserIdentityInfo
import Network.AWS.Connect.Types.UserPhoneConfig
import Network.AWS.Connect.Types.UserSummary
import Network.AWS.Lens
import Network.AWS.Prelude
