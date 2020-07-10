{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types
    (
    -- * Service Configuration
      codeBuild

    -- * Errors
    , _OAuthProviderException
    , _ResourceNotFoundException
    , _ResourceAlreadyExistsException
    , _AccountLimitExceededException
    , _InvalidInputException

    -- * ArtifactNamespace
    , ArtifactNamespace (..)

    -- * ArtifactPackaging
    , ArtifactPackaging (..)

    -- * ArtifactsType
    , ArtifactsType (..)

    -- * AuthType
    , AuthType (..)

    -- * BuildPhaseType
    , BuildPhaseType (..)

    -- * CacheMode
    , CacheMode (..)

    -- * CacheType
    , CacheType (..)

    -- * ComputeType
    , ComputeType (..)

    -- * CredentialProviderType
    , CredentialProviderType (..)

    -- * EnvironmentType
    , EnvironmentType (..)

    -- * EnvironmentVariableType
    , EnvironmentVariableType (..)

    -- * FileSystemType
    , FileSystemType (..)

    -- * ImagePullCredentialsType
    , ImagePullCredentialsType (..)

    -- * LanguageType
    , LanguageType (..)

    -- * LogsConfigStatusType
    , LogsConfigStatusType (..)

    -- * PlatformType
    , PlatformType (..)

    -- * ProjectSortByType
    , ProjectSortByType (..)

    -- * ReportExportConfigType
    , ReportExportConfigType (..)

    -- * ReportGroupSortByType
    , ReportGroupSortByType (..)

    -- * ReportPackagingType
    , ReportPackagingType (..)

    -- * ReportStatusType
    , ReportStatusType (..)

    -- * ReportType
    , ReportType (..)

    -- * ServerType
    , ServerType (..)

    -- * SharedResourceSortByType
    , SharedResourceSortByType (..)

    -- * SortOrderType
    , SortOrderType (..)

    -- * SourceAuthType
    , SourceAuthType (..)

    -- * SourceType
    , SourceType (..)

    -- * StatusType
    , StatusType (..)

    -- * WebhookFilterType
    , WebhookFilterType (..)

    -- * Build
    , Build
    , build
    , bPhases
    , bBuildComplete
    , bSecondaryArtifacts
    , bArn
    , bExportedEnvironmentVariables
    , bBuildNumber
    , bStartTime
    , bArtifacts
    , bEnvironment
    , bInitiator
    , bNetworkInterface
    , bSecondarySourceVersions
    , bCurrentPhase
    , bQueuedTimeoutInMinutes
    , bCache
    , bSecondarySources
    , bSourceVersion
    , bLogs
    , bResolvedSourceVersion
    , bVpcConfig
    , bEndTime
    , bProjectName
    , bBuildStatus
    , bSource
    , bId
    , bFileSystemLocations
    , bReportARNs
    , bEncryptionKey
    , bServiceRole
    , bTimeoutInMinutes

    -- * BuildArtifacts
    , BuildArtifacts
    , buildArtifacts
    , baLocation
    , baMd5sum
    , baEncryptionDisabled
    , baOverrideArtifactName
    , baArtifactIdentifier
    , baSha256sum

    -- * BuildNotDeleted
    , BuildNotDeleted
    , buildNotDeleted
    , bndId
    , bndStatusCode

    -- * BuildPhase
    , BuildPhase
    , buildPhase
    , bpContexts
    , bpStartTime
    , bpPhaseStatus
    , bpPhaseType
    , bpEndTime
    , bpDurationInSeconds

    -- * CloudWatchLogsConfig
    , CloudWatchLogsConfig
    , cloudWatchLogsConfig
    , cwlcGroupName
    , cwlcStreamName
    , cwlcStatus

    -- * EnvironmentImage
    , EnvironmentImage
    , environmentImage
    , eiVersions
    , eiName
    , eiDescription

    -- * EnvironmentLanguage
    , EnvironmentLanguage
    , environmentLanguage
    , elImages
    , elLanguage

    -- * EnvironmentPlatform
    , EnvironmentPlatform
    , environmentPlatform
    , epPlatform
    , epLanguages

    -- * EnvironmentVariable
    , EnvironmentVariable
    , environmentVariable
    , evType
    , evName
    , evValue

    -- * ExportedEnvironmentVariable
    , ExportedEnvironmentVariable
    , exportedEnvironmentVariable
    , eevValue
    , eevName

    -- * GitSubmodulesConfig
    , GitSubmodulesConfig
    , gitSubmodulesConfig
    , gscFetchSubmodules

    -- * LogsConfig
    , LogsConfig
    , logsConfig
    , lcS3Logs
    , lcCloudWatchLogs

    -- * LogsLocation
    , LogsLocation
    , logsLocation
    , llDeepLink
    , llS3Logs
    , llCloudWatchLogs
    , llS3DeepLink
    , llS3LogsARN
    , llCloudWatchLogsARN
    , llGroupName
    , llStreamName

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niSubnetId
    , niNetworkInterfaceId

    -- * PhaseContext
    , PhaseContext
    , phaseContext
    , pcMessage
    , pcStatusCode

    -- * Project
    , Project
    , project
    , pSecondaryArtifacts
    , pArn
    , pArtifacts
    , pEnvironment
    , pCreated
    , pSecondarySourceVersions
    , pQueuedTimeoutInMinutes
    , pCache
    , pSecondarySources
    , pSourceVersion
    , pName
    , pVpcConfig
    , pSource
    , pBadge
    , pLogsConfig
    , pFileSystemLocations
    , pEncryptionKey
    , pLastModified
    , pWebhook
    , pDescription
    , pServiceRole
    , pTags
    , pTimeoutInMinutes

    -- * ProjectArtifacts
    , ProjectArtifacts
    , projectArtifacts
    , paPackaging
    , paPath
    , paLocation
    , paName
    , paEncryptionDisabled
    , paOverrideArtifactName
    , paArtifactIdentifier
    , paNamespaceType
    , paType

    -- * ProjectBadge
    , ProjectBadge
    , projectBadge
    , pbBadgeEnabled
    , pbBadgeRequestURL

    -- * ProjectCache
    , ProjectCache
    , projectCache
    , pcLocation
    , pcModes
    , pcType

    -- * ProjectEnvironment
    , ProjectEnvironment
    , projectEnvironment
    , peImagePullCredentialsType
    , pePrivilegedMode
    , peRegistryCredential
    , peCertificate
    , peEnvironmentVariables
    , peType
    , peImage
    , peComputeType

    -- * ProjectFileSystemLocation
    , ProjectFileSystemLocation
    , projectFileSystemLocation
    , pfslLocation
    , pfslIdentifier
    , pfslMountOptions
    , pfslType
    , pfslMountPoint

    -- * ProjectSource
    , ProjectSource
    , projectSource
    , psReportBuildStatus
    , psInsecureSSL
    , psLocation
    , psAuth
    , psBuildspec
    , psSourceIdentifier
    , psGitCloneDepth
    , psGitSubmodulesConfig
    , psType

    -- * ProjectSourceVersion
    , ProjectSourceVersion
    , projectSourceVersion
    , psvSourceIdentifier
    , psvSourceVersion

    -- * RegistryCredential
    , RegistryCredential
    , registryCredential
    , rcCredential
    , rcCredentialProvider

    -- * Report
    , Report
    , report
    , rReportGroupARN
    , rStatus
    , rExpired
    , rExecutionId
    , rTruncated
    , rArn
    , rCreated
    , rName
    , rTestSummary
    , rType
    , rExportConfig

    -- * ReportExportConfig
    , ReportExportConfig
    , reportExportConfig
    , recExportConfigType
    , recS3Destination

    -- * ReportFilter
    , ReportFilter
    , reportFilter
    , rfStatus

    -- * ReportGroup
    , ReportGroup
    , reportGroup
    , rgArn
    , rgCreated
    , rgName
    , rgType
    , rgLastModified
    , rgExportConfig

    -- * S3LogsConfig
    , S3LogsConfig
    , s3LogsConfig
    , slcLocation
    , slcEncryptionDisabled
    , slcStatus

    -- * S3ReportExportConfig
    , S3ReportExportConfig
    , s3ReportExportConfig
    , srecPackaging
    , srecPath
    , srecBucket
    , srecEncryptionDisabled
    , srecEncryptionKey

    -- * SourceAuth
    , SourceAuth
    , sourceAuth
    , saResource
    , saType

    -- * SourceCredentialsInfo
    , SourceCredentialsInfo
    , sourceCredentialsInfo
    , sciArn
    , sciServerType
    , sciAuthType

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TestCase
    , TestCase
    , testCase
    , tcDurationInNanoSeconds
    , tcStatus
    , tcExpired
    , tcPrefix
    , tcName
    , tcTestRawDataPath
    , tcMessage
    , tcReportARN

    -- * TestCaseFilter
    , TestCaseFilter
    , testCaseFilter
    , tcfStatus

    -- * TestReportSummary
    , TestReportSummary
    , testReportSummary
    , trsTotal
    , trsStatusCounts
    , trsDurationInNanoSeconds

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcVpcId
    , vcSubnets

    -- * Webhook
    , Webhook
    , webhook
    , wBranchFilter
    , wLastModifiedSecret
    , wUrl
    , wSecret
    , wFilterGroups
    , wPayloadURL

    -- * WebhookFilter
    , WebhookFilter
    , webhookFilter
    , wfExcludeMatchedPattern
    , wfType
    , wfPattern
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.BuildPhaseType
import Network.AWS.CodeBuild.Types.CacheMode
import Network.AWS.CodeBuild.Types.CacheType
import Network.AWS.CodeBuild.Types.ComputeType
import Network.AWS.CodeBuild.Types.CredentialProviderType
import Network.AWS.CodeBuild.Types.EnvironmentType
import Network.AWS.CodeBuild.Types.EnvironmentVariableType
import Network.AWS.CodeBuild.Types.FileSystemType
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
import Network.AWS.CodeBuild.Types.LanguageType
import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import Network.AWS.CodeBuild.Types.PlatformType
import Network.AWS.CodeBuild.Types.ProjectSortByType
import Network.AWS.CodeBuild.Types.ReportExportConfigType
import Network.AWS.CodeBuild.Types.ReportGroupSortByType
import Network.AWS.CodeBuild.Types.ReportPackagingType
import Network.AWS.CodeBuild.Types.ReportStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.ServerType
import Network.AWS.CodeBuild.Types.SharedResourceSortByType
import Network.AWS.CodeBuild.Types.SortOrderType
import Network.AWS.CodeBuild.Types.SourceAuthType
import Network.AWS.CodeBuild.Types.SourceType
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.WebhookFilterType
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

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
codeBuild :: Service
codeBuild
  = Service{_svcAbbrev = "CodeBuild", _svcSigner = v4,
            _svcPrefix = "codebuild", _svcVersion = "2016-10-06",
            _svcEndpoint = defaultEndpoint codeBuild,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "CodeBuild",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | There was a problem with the underlying OAuth provider.
--
--
_OAuthProviderException :: AsError a => Getting (First ServiceError) a ServiceError
_OAuthProviderException
  = _MatchServiceError codeBuild
      "OAuthProviderException"

-- | The specified AWS resource cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError codeBuild
      "ResourceNotFoundException"

-- | The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException
  = _MatchServiceError codeBuild
      "ResourceAlreadyExistsException"

-- | An AWS service limit was exceeded for the calling AWS account.
--
--
_AccountLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AccountLimitExceededException
  = _MatchServiceError codeBuild
      "AccountLimitExceededException"

-- | The input value that was provided is not valid.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError codeBuild
      "InvalidInputException"
