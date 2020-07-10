{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types
    (
    -- * Service Configuration
      ecs

    -- * Errors
    , _ServiceNotActiveException
    , _ClusterContainsTasksException
    , _ServerException
    , _InvalidParameterException
    , _BlockedException
    , _AccessDeniedException
    , _PlatformTaskDefinitionIncompatibilityException
    , _ServiceNotFoundException
    , _ClusterNotFoundException
    , _NoUpdateAvailableException
    , _ClientException
    , _ResourceNotFoundException
    , _ClusterContainsContainerInstancesException
    , _TaskSetNotFoundException
    , _ClusterContainsServicesException
    , _PlatformUnknownException
    , _AttributeLimitExceededException
    , _TargetNotFoundException
    , _UpdateInProgressException
    , _LimitExceededException
    , _MissingVersionException
    , _ResourceInUseException
    , _UnsupportedFeatureException

    -- * AgentUpdateStatus
    , AgentUpdateStatus (..)

    -- * AssignPublicIP
    , AssignPublicIP (..)

    -- * CapacityProviderField
    , CapacityProviderField (..)

    -- * CapacityProviderStatus
    , CapacityProviderStatus (..)

    -- * ClusterField
    , ClusterField (..)

    -- * ClusterSettingName
    , ClusterSettingName (..)

    -- * Compatibility
    , Compatibility (..)

    -- * Connectivity
    , Connectivity (..)

    -- * ContainerCondition
    , ContainerCondition (..)

    -- * ContainerInstanceField
    , ContainerInstanceField (..)

    -- * ContainerInstanceStatus
    , ContainerInstanceStatus (..)

    -- * DeploymentControllerType
    , DeploymentControllerType (..)

    -- * DesiredStatus
    , DesiredStatus (..)

    -- * DeviceCgroupPermission
    , DeviceCgroupPermission (..)

    -- * EFSAuthorizationConfigIAM
    , EFSAuthorizationConfigIAM (..)

    -- * EFSTransitEncryption
    , EFSTransitEncryption (..)

    -- * FirelensConfigurationType
    , FirelensConfigurationType (..)

    -- * HealthStatus
    , HealthStatus (..)

    -- * IPcMode
    , IPcMode (..)

    -- * LaunchType
    , LaunchType (..)

    -- * LogDriver
    , LogDriver (..)

    -- * ManagedScalingStatus
    , ManagedScalingStatus (..)

    -- * ManagedTerminationProtection
    , ManagedTerminationProtection (..)

    -- * NetworkMode
    , NetworkMode (..)

    -- * PidMode
    , PidMode (..)

    -- * PlacementConstraintType
    , PlacementConstraintType (..)

    -- * PlacementStrategyType
    , PlacementStrategyType (..)

    -- * PlatformDeviceType
    , PlatformDeviceType (..)

    -- * PropagateTags
    , PropagateTags (..)

    -- * ProxyConfigurationType
    , ProxyConfigurationType (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ScaleUnit
    , ScaleUnit (..)

    -- * SchedulingStrategy
    , SchedulingStrategy (..)

    -- * Scope
    , Scope (..)

    -- * ServiceField
    , ServiceField (..)

    -- * SettingName
    , SettingName (..)

    -- * SortOrder
    , SortOrder (..)

    -- * StabilityStatus
    , StabilityStatus (..)

    -- * TargetType
    , TargetType (..)

    -- * TaskDefinitionFamilyStatus
    , TaskDefinitionFamilyStatus (..)

    -- * TaskDefinitionField
    , TaskDefinitionField (..)

    -- * TaskDefinitionPlacementConstraintType
    , TaskDefinitionPlacementConstraintType (..)

    -- * TaskDefinitionStatus
    , TaskDefinitionStatus (..)

    -- * TaskField
    , TaskField (..)

    -- * TaskSetField
    , TaskSetField (..)

    -- * TaskStopCode
    , TaskStopCode (..)

    -- * TransportProtocol
    , TransportProtocol (..)

    -- * UlimitName
    , UlimitName (..)

    -- * AWSVPCConfiguration
    , AWSVPCConfiguration
    , awsVPCConfiguration
    , avcSecurityGroups
    , avcAssignPublicIP
    , avcSubnets

    -- * Attachment
    , Attachment
    , attachment
    , aStatus
    , aDetails
    , aId
    , aType

    -- * AttachmentStateChange
    , AttachmentStateChange
    , attachmentStateChange
    , ascAttachmentARN
    , ascStatus

    -- * Attribute
    , Attribute
    , attribute
    , aTargetId
    , aValue
    , aTargetType
    , aName

    -- * AutoScalingGroupProvider
    , AutoScalingGroupProvider
    , autoScalingGroupProvider
    , asgpManagedScaling
    , asgpManagedTerminationProtection
    , asgpAutoScalingGroupARN

    -- * CapacityProvider
    , CapacityProvider
    , capacityProvider
    , cpStatus
    , cpAutoScalingGroupProvider
    , cpName
    , cpCapacityProviderARN
    , cpTags

    -- * CapacityProviderStrategyItem
    , CapacityProviderStrategyItem
    , capacityProviderStrategyItem
    , cpsiBase
    , cpsiWeight
    , cpsiCapacityProvider

    -- * Cluster
    , Cluster
    , cluster
    , cStatus
    , cClusterARN
    , cAttachments
    , cRunningTasksCount
    , cDefaultCapacityProviderStrategy
    , cSettings
    , cRegisteredContainerInstancesCount
    , cPendingTasksCount
    , cClusterName
    , cStatistics
    , cAttachmentsStatus
    , cCapacityProviders
    , cActiveServicesCount
    , cTags

    -- * ClusterSetting
    , ClusterSetting
    , clusterSetting
    , csValue
    , csName

    -- * Container
    , Container
    , container
    , cGpuIds
    , cNetworkBindings
    , cImage
    , cContainerARN
    , cNetworkInterfaces
    , cTaskARN
    , cLastStatus
    , cMemory
    , cReason
    , cName
    , cImageDigest
    , cExitCode
    , cHealthStatus
    , cCpu
    , cRuntimeId
    , cMemoryReservation

    -- * ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdImage
    , cdCommand
    , cdHostname
    , cdRepositoryCredentials
    , cdDockerSecurityOptions
    , cdHealthCheck
    , cdDisableNetworking
    , cdSecrets
    , cdVolumesFrom
    , cdEnvironment
    , cdEntryPoint
    , cdWorkingDirectory
    , cdUlimits
    , cdStopTimeout
    , cdPrivileged
    , cdPortMappings
    , cdResourceRequirements
    , cdDockerLabels
    , cdExtraHosts
    , cdMemory
    , cdSystemControls
    , cdUser
    , cdFirelensConfiguration
    , cdDnsSearchDomains
    , cdLogConfiguration
    , cdLinuxParameters
    , cdPseudoTerminal
    , cdDependsOn
    , cdName
    , cdDnsServers
    , cdMountPoints
    , cdInteractive
    , cdStartTimeout
    , cdLinks
    , cdReadonlyRootFilesystem
    , cdEssential
    , cdCpu
    , cdMemoryReservation

    -- * ContainerDependency
    , ContainerDependency
    , containerDependency
    , cdContainerName
    , cdCondition

    -- * ContainerInstance
    , ContainerInstance
    , containerInstance
    , ciStatus
    , ciAttachments
    , ciRunningTasksCount
    , ciRemainingResources
    , ciEc2InstanceId
    , ciContainerInstanceARN
    , ciAgentConnected
    , ciVersionInfo
    , ciAgentUpdateStatus
    , ciAttributes
    , ciVersion
    , ciPendingTasksCount
    , ciCapacityProviderName
    , ciRegisteredAt
    , ciStatusReason
    , ciTags
    , ciRegisteredResources

    -- * ContainerOverride
    , ContainerOverride
    , containerOverride
    , coCommand
    , coEnvironment
    , coResourceRequirements
    , coMemory
    , coName
    , coCpu
    , coMemoryReservation

    -- * ContainerService
    , ContainerService
    , containerService
    , csTaskSets
    , csRunningCount
    , csStatus
    , csClusterARN
    , csPropagateTags
    , csCreatedAt
    , csPlatformVersion
    , csEnableECSManagedTags
    , csCreatedBy
    , csDesiredCount
    , csLoadBalancers
    , csPendingCount
    , csPlacementConstraints
    , csEvents
    , csPlacementStrategy
    , csDeployments
    , csServiceName
    , csDeploymentController
    , csLaunchType
    , csServiceARN
    , csTaskDefinition
    , csSchedulingStrategy
    , csHealthCheckGracePeriodSeconds
    , csNetworkConfiguration
    , csServiceRegistries
    , csCapacityProviderStrategy
    , csTags
    , csRoleARN
    , csDeploymentConfiguration

    -- * ContainerStateChange
    , ContainerStateChange
    , containerStateChange
    , cscNetworkBindings
    , cscStatus
    , cscContainerName
    , cscReason
    , cscImageDigest
    , cscExitCode
    , cscRuntimeId

    -- * Deployment
    , Deployment
    , deployment
    , dRunningCount
    , dStatus
    , dCreatedAt
    , dPlatformVersion
    , dDesiredCount
    , dPendingCount
    , dId
    , dLaunchType
    , dUpdatedAt
    , dTaskDefinition
    , dNetworkConfiguration
    , dCapacityProviderStrategy

    -- * DeploymentConfiguration
    , DeploymentConfiguration
    , deploymentConfiguration
    , dcMinimumHealthyPercent
    , dcMaximumPercent

    -- * DeploymentController
    , DeploymentController
    , deploymentController
    , dcType

    -- * Device
    , Device
    , device
    , dContainerPath
    , dPermissions
    , dHostPath

    -- * DockerVolumeConfiguration
    , DockerVolumeConfiguration
    , dockerVolumeConfiguration
    , dvcDriverOpts
    , dvcDriver
    , dvcScope
    , dvcLabels
    , dvcAutoprovision

    -- * EFSAuthorizationConfig
    , EFSAuthorizationConfig
    , eFSAuthorizationConfig
    , efsacAccessPointId
    , efsacIam

    -- * EFSVolumeConfiguration
    , EFSVolumeConfiguration
    , eFSVolumeConfiguration
    , efsvcRootDirectory
    , efsvcTransitEncryption
    , efsvcAuthorizationConfig
    , efsvcTransitEncryptionPort
    , efsvcFileSystemId

    -- * Failure
    , Failure
    , failure
    , fArn
    , fReason
    , fDetail

    -- * FirelensConfiguration
    , FirelensConfiguration
    , firelensConfiguration
    , fcOptions
    , fcType

    -- * HealthCheck
    , HealthCheck
    , healthCheck
    , hcStartPeriod
    , hcRetries
    , hcInterval
    , hcTimeout
    , hcCommand

    -- * HostEntry
    , HostEntry
    , hostEntry
    , heHostname
    , heIpAddress

    -- * HostVolumeProperties
    , HostVolumeProperties
    , hostVolumeProperties
    , hvpSourcePath

    -- * InferenceAccelerator
    , InferenceAccelerator
    , inferenceAccelerator
    , iaDeviceName
    , iaDeviceType

    -- * InferenceAcceleratorOverride
    , InferenceAcceleratorOverride
    , inferenceAcceleratorOverride
    , iaoDeviceName
    , iaoDeviceType

    -- * KernelCapabilities
    , KernelCapabilities
    , kernelCapabilities
    , kcDrop
    , kcAdd

    -- * KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpValue
    , kvpName

    -- * LinuxParameters
    , LinuxParameters
    , linuxParameters
    , lpSharedMemorySize
    , lpInitProcessEnabled
    , lpTmpfs
    , lpSwappiness
    , lpDevices
    , lpCapabilities
    , lpMaxSwap

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbLoadBalancerName
    , lbContainerName
    , lbTargetGroupARN
    , lbContainerPort

    -- * LogConfiguration
    , LogConfiguration
    , logConfiguration
    , lcOptions
    , lcSecretOptions
    , lcLogDriver

    -- * ManagedScaling
    , ManagedScaling
    , managedScaling
    , msStatus
    , msMaximumScalingStepSize
    , msTargetCapacity
    , msMinimumScalingStepSize

    -- * MountPoint
    , MountPoint
    , mountPoint
    , mpContainerPath
    , mpSourceVolume
    , mpReadOnly

    -- * NetworkBinding
    , NetworkBinding
    , networkBinding
    , nbBindIP
    , nbProtocol
    , nbHostPort
    , nbContainerPort

    -- * NetworkConfiguration
    , NetworkConfiguration
    , networkConfiguration
    , ncAwsvpcConfiguration

    -- * NetworkInterface
    , NetworkInterface
    , networkInterface
    , niIpv6Address
    , niPrivateIPv4Address
    , niAttachmentId

    -- * PlacementConstraint
    , PlacementConstraint
    , placementConstraint
    , pcExpression
    , pcType

    -- * PlacementStrategy
    , PlacementStrategy
    , placementStrategy
    , psField
    , psType

    -- * PlatformDevice
    , PlatformDevice
    , platformDevice
    , pdId
    , pdType

    -- * PortMapping
    , PortMapping
    , portMapping
    , pmProtocol
    , pmHostPort
    , pmContainerPort

    -- * ProxyConfiguration
    , ProxyConfiguration
    , proxyConfiguration
    , pType
    , pProperties
    , pContainerName

    -- * RepositoryCredentials
    , RepositoryCredentials
    , repositoryCredentials
    , rcCredentialsParameter

    -- * Resource
    , Resource
    , resource
    , rStringSetValue
    , rIntegerValue
    , rDoubleValue
    , rLongValue
    , rName
    , rType

    -- * ResourceRequirement
    , ResourceRequirement
    , resourceRequirement
    , rrValue
    , rrType

    -- * Scale
    , Scale
    , scale
    , sValue
    , sUnit

    -- * Secret
    , Secret
    , secret
    , sName
    , sValueFrom

    -- * ServiceEvent
    , ServiceEvent
    , serviceEvent
    , seCreatedAt
    , seId
    , seMessage

    -- * ServiceRegistry
    , ServiceRegistry
    , serviceRegistry
    , srRegistryARN
    , srContainerName
    , srContainerPort
    , srPort

    -- * Setting
    , Setting
    , setting
    , setValue
    , setName
    , setPrincipalARN

    -- * SystemControl
    , SystemControl
    , systemControl
    , scValue
    , scNamespace

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Task
    , Task
    , task
    , tStoppedAt
    , tDesiredStatus
    , tOverrides
    , tInferenceAccelerators
    , tClusterARN
    , tGroup
    , tAttachments
    , tCreatedAt
    , tStopCode
    , tPlatformVersion
    , tTaskARN
    , tContainerInstanceARN
    , tExecutionStoppedAt
    , tLastStatus
    , tMemory
    , tPullStoppedAt
    , tContainers
    , tStartedAt
    , tAvailabilityZone
    , tAttributes
    , tVersion
    , tCapacityProviderName
    , tStartedBy
    , tStoppedReason
    , tConnectivity
    , tStoppingAt
    , tLaunchType
    , tTaskDefinitionARN
    , tHealthStatus
    , tConnectivityAt
    , tCpu
    , tPullStartedAt
    , tTags

    -- * TaskDefinition
    , TaskDefinition
    , taskDefinition
    , tdStatus
    , tdInferenceAccelerators
    , tdExecutionRoleARN
    , tdRequiresCompatibilities
    , tdPidMode
    , tdFamily
    , tdIpcMode
    , tdContainerDefinitions
    , tdMemory
    , tdProxyConfiguration
    , tdTaskRoleARN
    , tdPlacementConstraints
    , tdNetworkMode
    , tdTaskDefinitionARN
    , tdCompatibilities
    , tdRevision
    , tdVolumes
    , tdCpu
    , tdRequiresAttributes

    -- * TaskDefinitionPlacementConstraint
    , TaskDefinitionPlacementConstraint
    , taskDefinitionPlacementConstraint
    , tdpcExpression
    , tdpcType

    -- * TaskOverride
    , TaskOverride
    , taskOverride
    , toContainerOverrides
    , toExecutionRoleARN
    , toMemory
    , toTaskRoleARN
    , toInferenceAcceleratorOverrides
    , toCpu

    -- * TaskSet
    , TaskSet
    , taskSet
    , tsRunningCount
    , tsStatus
    , tsClusterARN
    , tsComputedDesiredCount
    , tsCreatedAt
    , tsPlatformVersion
    , tsScale
    , tsLoadBalancers
    , tsStabilityStatusAt
    , tsPendingCount
    , tsTaskSetARN
    , tsStartedBy
    , tsId
    , tsLaunchType
    , tsUpdatedAt
    , tsServiceARN
    , tsTaskDefinition
    , tsExternalId
    , tsNetworkConfiguration
    , tsServiceRegistries
    , tsCapacityProviderStrategy
    , tsStabilityStatus
    , tsTags

    -- * Tmpfs
    , Tmpfs
    , tmpfs
    , tMountOptions
    , tContainerPath
    , tSize

    -- * Ulimit
    , Ulimit
    , ulimit
    , uName
    , uSoftLimit
    , uHardLimit

    -- * VersionInfo
    , VersionInfo
    , versionInfo
    , viAgentHash
    , viAgentVersion
    , viDockerVersion

    -- * Volume
    , Volume
    , volume
    , vDockerVolumeConfiguration
    , vName
    , vEfsVolumeConfiguration
    , vHost

    -- * VolumeFrom
    , VolumeFrom
    , volumeFrom
    , vfSourceContainer
    , vfReadOnly
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.ECS.Types.AgentUpdateStatus
import Network.AWS.ECS.Types.AssignPublicIP
import Network.AWS.ECS.Types.CapacityProviderField
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.ClusterField
import Network.AWS.ECS.Types.ClusterSettingName
import Network.AWS.ECS.Types.Compatibility
import Network.AWS.ECS.Types.Connectivity
import Network.AWS.ECS.Types.ContainerCondition
import Network.AWS.ECS.Types.ContainerInstanceField
import Network.AWS.ECS.Types.ContainerInstanceStatus
import Network.AWS.ECS.Types.DeploymentControllerType
import Network.AWS.ECS.Types.DesiredStatus
import Network.AWS.ECS.Types.DeviceCgroupPermission
import Network.AWS.ECS.Types.EFSAuthorizationConfigIAM
import Network.AWS.ECS.Types.EFSTransitEncryption
import Network.AWS.ECS.Types.FirelensConfigurationType
import Network.AWS.ECS.Types.HealthStatus
import Network.AWS.ECS.Types.IPcMode
import Network.AWS.ECS.Types.LaunchType
import Network.AWS.ECS.Types.LogDriver
import Network.AWS.ECS.Types.ManagedScalingStatus
import Network.AWS.ECS.Types.ManagedTerminationProtection
import Network.AWS.ECS.Types.NetworkMode
import Network.AWS.ECS.Types.PidMode
import Network.AWS.ECS.Types.PlacementConstraintType
import Network.AWS.ECS.Types.PlacementStrategyType
import Network.AWS.ECS.Types.PlatformDeviceType
import Network.AWS.ECS.Types.PropagateTags
import Network.AWS.ECS.Types.ProxyConfigurationType
import Network.AWS.ECS.Types.ResourceType
import Network.AWS.ECS.Types.ScaleUnit
import Network.AWS.ECS.Types.SchedulingStrategy
import Network.AWS.ECS.Types.Scope
import Network.AWS.ECS.Types.ServiceField
import Network.AWS.ECS.Types.SettingName
import Network.AWS.ECS.Types.SortOrder
import Network.AWS.ECS.Types.StabilityStatus
import Network.AWS.ECS.Types.TargetType
import Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
import Network.AWS.ECS.Types.TaskDefinitionField
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import Network.AWS.ECS.Types.TaskDefinitionStatus
import Network.AWS.ECS.Types.TaskField
import Network.AWS.ECS.Types.TaskSetField
import Network.AWS.ECS.Types.TaskStopCode
import Network.AWS.ECS.Types.TransportProtocol
import Network.AWS.ECS.Types.UlimitName
import Network.AWS.ECS.Types.AWSVPCConfiguration
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.AttachmentStateChange
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.CapacityProvider
import Network.AWS.ECS.Types.CapacityProviderStrategyItem
import Network.AWS.ECS.Types.Cluster
import Network.AWS.ECS.Types.ClusterSetting
import Network.AWS.ECS.Types.Container
import Network.AWS.ECS.Types.ContainerDefinition
import Network.AWS.ECS.Types.ContainerDependency
import Network.AWS.ECS.Types.ContainerInstance
import Network.AWS.ECS.Types.ContainerOverride
import Network.AWS.ECS.Types.ContainerService
import Network.AWS.ECS.Types.ContainerStateChange
import Network.AWS.ECS.Types.Deployment
import Network.AWS.ECS.Types.DeploymentConfiguration
import Network.AWS.ECS.Types.DeploymentController
import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.DockerVolumeConfiguration
import Network.AWS.ECS.Types.EFSAuthorizationConfig
import Network.AWS.ECS.Types.EFSVolumeConfiguration
import Network.AWS.ECS.Types.Failure
import Network.AWS.ECS.Types.FirelensConfiguration
import Network.AWS.ECS.Types.HealthCheck
import Network.AWS.ECS.Types.HostEntry
import Network.AWS.ECS.Types.HostVolumeProperties
import Network.AWS.ECS.Types.InferenceAccelerator
import Network.AWS.ECS.Types.InferenceAcceleratorOverride
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.LinuxParameters
import Network.AWS.ECS.Types.LoadBalancer
import Network.AWS.ECS.Types.LogConfiguration
import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.MountPoint
import Network.AWS.ECS.Types.NetworkBinding
import Network.AWS.ECS.Types.NetworkConfiguration
import Network.AWS.ECS.Types.NetworkInterface
import Network.AWS.ECS.Types.PlacementConstraint
import Network.AWS.ECS.Types.PlacementStrategy
import Network.AWS.ECS.Types.PlatformDevice
import Network.AWS.ECS.Types.PortMapping
import Network.AWS.ECS.Types.ProxyConfiguration
import Network.AWS.ECS.Types.RepositoryCredentials
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.ResourceRequirement
import Network.AWS.ECS.Types.Scale
import Network.AWS.ECS.Types.Secret
import Network.AWS.ECS.Types.ServiceEvent
import Network.AWS.ECS.Types.ServiceRegistry
import Network.AWS.ECS.Types.Setting
import Network.AWS.ECS.Types.SystemControl
import Network.AWS.ECS.Types.Tag
import Network.AWS.ECS.Types.Task
import Network.AWS.ECS.Types.TaskDefinition
import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
import Network.AWS.ECS.Types.TaskOverride
import Network.AWS.ECS.Types.TaskSet
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.ECS.Types.Ulimit
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.ECS.Types.Volume
import Network.AWS.ECS.Types.VolumeFrom

-- | API version @2014-11-13@ of the Amazon EC2 Container Service SDK configuration.
ecs :: Service
ecs
  = Service{_svcAbbrev = "ECS", _svcSigner = v4,
            _svcPrefix = "ecs", _svcVersion = "2014-11-13",
            _svcEndpoint = defaultEndpoint ecs,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "ECS", _svcRetry = retry}
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

-- | The specified service is not active. You can't update a service that is inactive. If you have previously deleted a service, you can re-create it with 'CreateService' .
--
--
_ServiceNotActiveException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceNotActiveException
  = _MatchServiceError ecs "ServiceNotActiveException"

-- | You cannot delete a cluster that has active tasks.
--
--
_ClusterContainsTasksException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterContainsTasksException
  = _MatchServiceError ecs
      "ClusterContainsTasksException"

-- | These errors are usually caused by a server issue.
--
--
_ServerException :: AsError a => Getting (First ServiceError) a ServiceError
_ServerException
  = _MatchServiceError ecs "ServerException"

-- | The specified parameter is invalid. Review the available parameters for the API request.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError ecs "InvalidParameterException"

-- | Your AWS account has been blocked. For more information, contact <http://aws.amazon.com/contact-us/ AWS Support> .
--
--
_BlockedException :: AsError a => Getting (First ServiceError) a ServiceError
_BlockedException
  = _MatchServiceError ecs "BlockedException"

-- | You do not have authorization to perform the requested action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException
  = _MatchServiceError ecs "AccessDeniedException"

-- | The specified platform version does not satisfy the task definition's required capabilities.
--
--
_PlatformTaskDefinitionIncompatibilityException :: AsError a => Getting (First ServiceError) a ServiceError
_PlatformTaskDefinitionIncompatibilityException
  = _MatchServiceError ecs
      "PlatformTaskDefinitionIncompatibilityException"

-- | The specified service could not be found. You can view your available services with 'ListServices' . Amazon ECS services are cluster-specific and Region-specific.
--
--
_ServiceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceNotFoundException
  = _MatchServiceError ecs "ServiceNotFoundException"

-- | The specified cluster could not be found. You can view your available clusters with 'ListClusters' . Amazon ECS clusters are Region-specific.
--
--
_ClusterNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundException
  = _MatchServiceError ecs "ClusterNotFoundException"

-- | There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.
--
--
_NoUpdateAvailableException :: AsError a => Getting (First ServiceError) a ServiceError
_NoUpdateAvailableException
  = _MatchServiceError ecs "NoUpdateAvailableException"

-- | These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.
--
--
_ClientException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientException
  = _MatchServiceError ecs "ClientException"

-- | The specified resource could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError ecs "ResourceNotFoundException"

-- | You cannot delete a cluster that has registered container instances. First, deregister the container instances before you can delete the cluster. For more information, see 'DeregisterContainerInstance' .
--
--
_ClusterContainsContainerInstancesException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterContainsContainerInstancesException
  = _MatchServiceError ecs
      "ClusterContainsContainerInstancesException"

-- | The specified task set could not be found. You can view your available task sets with 'DescribeTaskSets' . Task sets are specific to each cluster, service and Region.
--
--
_TaskSetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TaskSetNotFoundException
  = _MatchServiceError ecs "TaskSetNotFoundException"

-- | You cannot delete a cluster that contains services. First, update the service to reduce its desired task count to 0 and then delete the service. For more information, see 'UpdateService' and 'DeleteService' .
--
--
_ClusterContainsServicesException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterContainsServicesException
  = _MatchServiceError ecs
      "ClusterContainsServicesException"

-- | The specified platform version does not exist.
--
--
_PlatformUnknownException :: AsError a => Getting (First ServiceError) a ServiceError
_PlatformUnknownException
  = _MatchServiceError ecs "PlatformUnknownException"

-- | You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with 'ListAttributes' . You can remove existing attributes on a resource with 'DeleteAttributes' .
--
--
_AttributeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AttributeLimitExceededException
  = _MatchServiceError ecs
      "AttributeLimitExceededException"

-- | The specified target could not be found. You can view your available container instances with 'ListContainerInstances' . Amazon ECS container instances are cluster-specific and Region-specific.
--
--
_TargetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetNotFoundException
  = _MatchServiceError ecs "TargetNotFoundException"

-- | There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as @PENDING@ or @STAGING@ , the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.
--
--
_UpdateInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_UpdateInProgressException
  = _MatchServiceError ecs "UpdateInProgressException"

-- | The limit for the resource has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError ecs "LimitExceededException"

-- | Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.
--
--
_MissingVersionException :: AsError a => Getting (First ServiceError) a ServiceError
_MissingVersionException
  = _MatchServiceError ecs "MissingVersionException"

-- | The specified resource is in-use and cannot be removed.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError ecs "ResourceInUseException"

-- | The specified task is not supported in this Region.
--
--
_UnsupportedFeatureException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedFeatureException
  = _MatchServiceError ecs
      "UnsupportedFeatureException"
