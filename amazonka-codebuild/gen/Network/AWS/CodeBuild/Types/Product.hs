{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.Product (
    module Network.AWS.CodeBuild.Types.Build,
    module Network.AWS.CodeBuild.Types.BuildArtifacts,
    module Network.AWS.CodeBuild.Types.BuildNotDeleted,
    module Network.AWS.CodeBuild.Types.BuildPhase,
    module Network.AWS.CodeBuild.Types.CloudWatchLogsConfig,
    module Network.AWS.CodeBuild.Types.EnvironmentImage,
    module Network.AWS.CodeBuild.Types.EnvironmentLanguage,
    module Network.AWS.CodeBuild.Types.EnvironmentPlatform,
    module Network.AWS.CodeBuild.Types.EnvironmentVariable,
    module Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable,
    module Network.AWS.CodeBuild.Types.GitSubmodulesConfig,
    module Network.AWS.CodeBuild.Types.LogsConfig,
    module Network.AWS.CodeBuild.Types.LogsLocation,
    module Network.AWS.CodeBuild.Types.NetworkInterface,
    module Network.AWS.CodeBuild.Types.PhaseContext,
    module Network.AWS.CodeBuild.Types.Project,
    module Network.AWS.CodeBuild.Types.ProjectArtifacts,
    module Network.AWS.CodeBuild.Types.ProjectBadge,
    module Network.AWS.CodeBuild.Types.ProjectCache,
    module Network.AWS.CodeBuild.Types.ProjectEnvironment,
    module Network.AWS.CodeBuild.Types.ProjectFileSystemLocation,
    module Network.AWS.CodeBuild.Types.ProjectSource,
    module Network.AWS.CodeBuild.Types.ProjectSourceVersion,
    module Network.AWS.CodeBuild.Types.RegistryCredential,
    module Network.AWS.CodeBuild.Types.Report,
    module Network.AWS.CodeBuild.Types.ReportExportConfig,
    module Network.AWS.CodeBuild.Types.ReportFilter,
    module Network.AWS.CodeBuild.Types.ReportGroup,
    module Network.AWS.CodeBuild.Types.S3LogsConfig,
    module Network.AWS.CodeBuild.Types.S3ReportExportConfig,
    module Network.AWS.CodeBuild.Types.SourceAuth,
    module Network.AWS.CodeBuild.Types.SourceCredentialsInfo,
    module Network.AWS.CodeBuild.Types.Tag,
    module Network.AWS.CodeBuild.Types.TestCase,
    module Network.AWS.CodeBuild.Types.TestCaseFilter,
    module Network.AWS.CodeBuild.Types.TestReportSummary,
    module Network.AWS.CodeBuild.Types.VPCConfig,
    module Network.AWS.CodeBuild.Types.Webhook,
    module Network.AWS.CodeBuild.Types.WebhookFilter
  ) where

import Network.AWS.CodeBuild.Types.Build
import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildNotDeleted
import Network.AWS.CodeBuild.Types.BuildPhase
import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.EnvironmentPlatform
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.LogsLocation
import Network.AWS.CodeBuild.Types.NetworkInterface
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.Project
import Network.AWS.CodeBuild.Types.ProjectArtifacts
import Network.AWS.CodeBuild.Types.ProjectBadge
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.RegistryCredential
import Network.AWS.CodeBuild.Types.Report
import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportFilter
import Network.AWS.CodeBuild.Types.ReportGroup
import Network.AWS.CodeBuild.Types.S3LogsConfig
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceCredentialsInfo
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.CodeBuild.Types.TestCase
import Network.AWS.CodeBuild.Types.TestCaseFilter
import Network.AWS.CodeBuild.Types.TestReportSummary
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.CodeBuild.Types.WebhookFilter
import Network.AWS.Lens
import Network.AWS.Prelude
