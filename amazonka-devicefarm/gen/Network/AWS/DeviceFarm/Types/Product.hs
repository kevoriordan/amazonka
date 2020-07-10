{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Product (
    module Network.AWS.DeviceFarm.Types.AccountSettings,
    module Network.AWS.DeviceFarm.Types.Artifact,
    module Network.AWS.DeviceFarm.Types.CPU,
    module Network.AWS.DeviceFarm.Types.Counters,
    module Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration,
    module Network.AWS.DeviceFarm.Types.CustomerArtifactPaths,
    module Network.AWS.DeviceFarm.Types.Device,
    module Network.AWS.DeviceFarm.Types.DeviceFilter,
    module Network.AWS.DeviceFarm.Types.DeviceInstance,
    module Network.AWS.DeviceFarm.Types.DeviceMinutes,
    module Network.AWS.DeviceFarm.Types.DevicePool,
    module Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult,
    module Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration,
    module Network.AWS.DeviceFarm.Types.DeviceSelectionResult,
    module Network.AWS.DeviceFarm.Types.ExecutionConfiguration,
    module Network.AWS.DeviceFarm.Types.IncompatibilityMessage,
    module Network.AWS.DeviceFarm.Types.InstanceProfile,
    module Network.AWS.DeviceFarm.Types.Job,
    module Network.AWS.DeviceFarm.Types.Location,
    module Network.AWS.DeviceFarm.Types.MonetaryAmount,
    module Network.AWS.DeviceFarm.Types.NetworkProfile,
    module Network.AWS.DeviceFarm.Types.Offering,
    module Network.AWS.DeviceFarm.Types.OfferingPromotion,
    module Network.AWS.DeviceFarm.Types.OfferingStatus,
    module Network.AWS.DeviceFarm.Types.OfferingTransaction,
    module Network.AWS.DeviceFarm.Types.Problem,
    module Network.AWS.DeviceFarm.Types.ProblemDetail,
    module Network.AWS.DeviceFarm.Types.Project,
    module Network.AWS.DeviceFarm.Types.Radios,
    module Network.AWS.DeviceFarm.Types.RecurringCharge,
    module Network.AWS.DeviceFarm.Types.RemoteAccessSession,
    module Network.AWS.DeviceFarm.Types.Resolution,
    module Network.AWS.DeviceFarm.Types.Rule,
    module Network.AWS.DeviceFarm.Types.Run,
    module Network.AWS.DeviceFarm.Types.Sample,
    module Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration,
    module Network.AWS.DeviceFarm.Types.ScheduleRunTest,
    module Network.AWS.DeviceFarm.Types.Suite,
    module Network.AWS.DeviceFarm.Types.Tag,
    module Network.AWS.DeviceFarm.Types.Test,
    module Network.AWS.DeviceFarm.Types.TestGridProject,
    module Network.AWS.DeviceFarm.Types.TestGridSession,
    module Network.AWS.DeviceFarm.Types.TestGridSessionAction,
    module Network.AWS.DeviceFarm.Types.TestGridSessionArtifact,
    module Network.AWS.DeviceFarm.Types.TrialMinutes,
    module Network.AWS.DeviceFarm.Types.UniqueProblem,
    module Network.AWS.DeviceFarm.Types.Upload,
    module Network.AWS.DeviceFarm.Types.VPCEConfiguration
  ) where

import Network.AWS.DeviceFarm.Types.AccountSettings
import Network.AWS.DeviceFarm.Types.Artifact
import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceFilter
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.DevicePool
import Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
import Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
import Network.AWS.DeviceFarm.Types.ExecutionConfiguration
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.Job
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.NetworkProfile
import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingPromotion
import Network.AWS.DeviceFarm.Types.OfferingStatus
import Network.AWS.DeviceFarm.Types.OfferingTransaction
import Network.AWS.DeviceFarm.Types.Problem
import Network.AWS.DeviceFarm.Types.ProblemDetail
import Network.AWS.DeviceFarm.Types.Project
import Network.AWS.DeviceFarm.Types.Radios
import Network.AWS.DeviceFarm.Types.RecurringCharge
import Network.AWS.DeviceFarm.Types.RemoteAccessSession
import Network.AWS.DeviceFarm.Types.Resolution
import Network.AWS.DeviceFarm.Types.Rule
import Network.AWS.DeviceFarm.Types.Run
import Network.AWS.DeviceFarm.Types.Sample
import Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
import Network.AWS.DeviceFarm.Types.ScheduleRunTest
import Network.AWS.DeviceFarm.Types.Suite
import Network.AWS.DeviceFarm.Types.Tag
import Network.AWS.DeviceFarm.Types.Test
import Network.AWS.DeviceFarm.Types.TestGridProject
import Network.AWS.DeviceFarm.Types.TestGridSession
import Network.AWS.DeviceFarm.Types.TestGridSessionAction
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
import Network.AWS.DeviceFarm.Types.TrialMinutes
import Network.AWS.DeviceFarm.Types.UniqueProblem
import Network.AWS.DeviceFarm.Types.Upload
import Network.AWS.DeviceFarm.Types.VPCEConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude
