{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types
    (
    -- * Service Configuration
      lambda

    -- * Errors
    , _PolicyLengthExceededException
    , _CodeStorageExceededException
    , _PreconditionFailedException
    , _KMSDisabledException
    , _SubnetIPAddressLimitReachedException
    , _TooManyRequestsException
    , _ServiceException
    , _InvalidRuntimeException
    , _EC2ThrottledException
    , _RequestTooLargeException
    , _ENILimitReachedException
    , _InvalidParameterValueException
    , _KMSNotFoundException
    , _InvalidRequestContentException
    , _ResourceNotFoundException
    , _InvalidZipFileException
    , _ResourceNotReadyException
    , _ProvisionedConcurrencyConfigNotFoundException
    , _ResourceConflictException
    , _EC2AccessDeniedException
    , _InvalidSubnetIdException
    , _InvalidSecurityGroupIdException
    , _KMSInvalidStateException
    , _KMSAccessDeniedException
    , _ResourceInUseException
    , _EC2UnexpectedException
    , _UnsupportedMediaTypeException

    -- * EventSourcePosition
    , EventSourcePosition (..)

    -- * FunctionVersion
    , FunctionVersion (..)

    -- * InvocationType
    , InvocationType (..)

    -- * LastUpdateStatus
    , LastUpdateStatus (..)

    -- * LastUpdateStatusReasonCode
    , LastUpdateStatusReasonCode (..)

    -- * LogType
    , LogType (..)

    -- * ProvisionedConcurrencyStatusEnum
    , ProvisionedConcurrencyStatusEnum (..)

    -- * Runtime
    , Runtime (..)

    -- * State
    , State (..)

    -- * StateReasonCode
    , StateReasonCode (..)

    -- * TracingMode
    , TracingMode (..)

    -- * AccountLimit
    , AccountLimit
    , accountLimit
    , alConcurrentExecutions
    , alTotalCodeSize
    , alUnreservedConcurrentExecutions
    , alCodeSizeUnzipped
    , alCodeSizeZipped

    -- * AccountUsage
    , AccountUsage
    , accountUsage
    , auTotalCodeSize
    , auFunctionCount

    -- * AliasConfiguration
    , AliasConfiguration
    , aliasConfiguration
    , acRoutingConfig
    , acName
    , acFunctionVersion
    , acAliasARN
    , acDescription
    , acRevisionId

    -- * AliasRoutingConfiguration
    , AliasRoutingConfiguration
    , aliasRoutingConfiguration
    , arcAdditionalVersionWeights

    -- * Concurrency
    , Concurrency
    , concurrency
    , cReservedConcurrentExecutions

    -- * DeadLetterConfig
    , DeadLetterConfig
    , deadLetterConfig
    , dlcTargetARN

    -- * DestinationConfig
    , DestinationConfig
    , destinationConfig
    , dcOnSuccess
    , dcOnFailure

    -- * Environment
    , Environment
    , environment
    , eVariables

    -- * EnvironmentError
    , EnvironmentError
    , environmentError
    , eeErrorCode
    , eeMessage

    -- * EnvironmentResponse
    , EnvironmentResponse
    , environmentResponse
    , envVariables
    , envError

    -- * EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
    , esmcBisectBatchOnFunctionError
    , esmcUUId
    , esmcParallelizationFactor
    , esmcLastProcessingResult
    , esmcMaximumRetryAttempts
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcMaximumBatchingWindowInSeconds
    , esmcMaximumRecordAgeInSeconds
    , esmcLastModified
    , esmcDestinationConfig

    -- * FunctionCode
    , FunctionCode
    , functionCode
    , fcS3ObjectVersion
    , fcS3Key
    , fcZipFile
    , fcS3Bucket

    -- * FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- * FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcMemorySize
    , fcRuntime
    , fcState
    , fcLastUpdateStatus
    , fcFunctionARN
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcLayers
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastUpdateStatusReason
    , fcStateReason
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcStateReasonCode
    , fcDescription
    , fcLastUpdateStatusReasonCode
    , fcRevisionId
    , fcMasterARN

    -- * FunctionEventInvokeConfig
    , FunctionEventInvokeConfig
    , functionEventInvokeConfig
    , feicFunctionARN
    , feicMaximumEventAgeInSeconds
    , feicMaximumRetryAttempts
    , feicLastModified
    , feicDestinationConfig

    -- * GetLayerVersionResponse
    , GetLayerVersionResponse
    , getLayerVersionResponse
    , glvLayerVersionARN
    , glvContent
    , glvCreatedDate
    , glvVersion
    , glvLicenseInfo
    , glvLayerARN
    , glvDescription
    , glvCompatibleRuntimes

    -- * Layer
    , Layer
    , layer
    , lARN
    , lCodeSize

    -- * LayerVersionContentInput
    , LayerVersionContentInput
    , layerVersionContentInput
    , lvciS3ObjectVersion
    , lvciS3Key
    , lvciZipFile
    , lvciS3Bucket

    -- * LayerVersionContentOutput
    , LayerVersionContentOutput
    , layerVersionContentOutput
    , lvcoLocation
    , lvcoCodeSize
    , lvcoCodeSha256

    -- * LayerVersionsListItem
    , LayerVersionsListItem
    , layerVersionsListItem
    , lvliLayerVersionARN
    , lvliCreatedDate
    , lvliVersion
    , lvliLicenseInfo
    , lvliDescription
    , lvliCompatibleRuntimes

    -- * LayersListItem
    , LayersListItem
    , layersListItem
    , lliLayerName
    , lliLatestMatchingVersion
    , lliLayerARN

    -- * OnFailure
    , OnFailure
    , onFailure
    , ofDestination

    -- * OnSuccess
    , OnSuccess
    , onSuccess
    , osDestination

    -- * ProvisionedConcurrencyConfigListItem
    , ProvisionedConcurrencyConfigListItem
    , provisionedConcurrencyConfigListItem
    , pccliStatus
    , pccliFunctionARN
    , pccliRequestedProvisionedConcurrentExecutions
    , pccliAvailableProvisionedConcurrentExecutions
    , pccliStatusReason
    , pccliAllocatedProvisionedConcurrentExecutions
    , pccliLastModified

    -- * TracingConfig
    , TracingConfig
    , tracingConfig
    , tMode

    -- * TracingConfigResponse
    , TracingConfigResponse
    , tracingConfigResponse
    , tcMode

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vpccSecurityGroupIds
    , vpccSubnetIds

    -- * VPCConfigResponse
    , VPCConfigResponse
    , vpcConfigResponse
    , vcSecurityGroupIds
    , vcSubnetIds
    , vcVPCId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Lambda.Types.EventSourcePosition
import Network.AWS.Lambda.Types.FunctionVersion
import Network.AWS.Lambda.Types.InvocationType
import Network.AWS.Lambda.Types.LastUpdateStatus
import Network.AWS.Lambda.Types.LastUpdateStatusReasonCode
import Network.AWS.Lambda.Types.LogType
import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lambda.Types.State
import Network.AWS.Lambda.Types.StateReasonCode
import Network.AWS.Lambda.Types.TracingMode
import Network.AWS.Lambda.Types.AccountLimit
import Network.AWS.Lambda.Types.AccountUsage
import Network.AWS.Lambda.Types.AliasConfiguration
import Network.AWS.Lambda.Types.AliasRoutingConfiguration
import Network.AWS.Lambda.Types.Concurrency
import Network.AWS.Lambda.Types.DeadLetterConfig
import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.Environment
import Network.AWS.Lambda.Types.EnvironmentError
import Network.AWS.Lambda.Types.EnvironmentResponse
import Network.AWS.Lambda.Types.EventSourceMappingConfiguration
import Network.AWS.Lambda.Types.FunctionCode
import Network.AWS.Lambda.Types.FunctionCodeLocation
import Network.AWS.Lambda.Types.FunctionConfiguration
import Network.AWS.Lambda.Types.FunctionEventInvokeConfig
import Network.AWS.Lambda.Types.GetLayerVersionResponse
import Network.AWS.Lambda.Types.Layer
import Network.AWS.Lambda.Types.LayerVersionContentInput
import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.LayerVersionsListItem
import Network.AWS.Lambda.Types.LayersListItem
import Network.AWS.Lambda.Types.OnFailure
import Network.AWS.Lambda.Types.OnSuccess
import Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
import Network.AWS.Lambda.Types.TracingConfig
import Network.AWS.Lambda.Types.TracingConfigResponse
import Network.AWS.Lambda.Types.VPCConfig
import Network.AWS.Lambda.Types.VPCConfigResponse

-- | API version @2015-03-31@ of the Amazon Lambda SDK configuration.
lambda :: Service
lambda
  = Service{_svcAbbrev = "Lambda", _svcSigner = v4,
            _svcPrefix = "lambda", _svcVersion = "2015-03-31",
            _svcEndpoint = defaultEndpoint lambda,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Lambda",
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

-- | The permissions policy for the resource is too large. <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more> 
--
--
_PolicyLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyLengthExceededException
  = _MatchServiceError lambda
      "PolicyLengthExceededException"
      . hasStatus 400

-- | You have exceeded your maximum total code size per account. <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Learn more> 
--
--
_CodeStorageExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeStorageExceededException
  = _MatchServiceError lambda
      "CodeStorageExceededException"
      . hasStatus 400

-- | The RevisionId provided does not match the latest RevisionId for the Lambda function or alias. Call the @GetFunction@ or the @GetAlias@ API to retrieve the latest RevisionId for your resource.
--
--
_PreconditionFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_PreconditionFailedException
  = _MatchServiceError lambda
      "PreconditionFailedException"
      . hasStatus 412

-- | Lambda was unable to decrypt the environment variables because the KMS key used is disabled. Check the Lambda function's KMS key settings.
--
--
_KMSDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSDisabledException
  = _MatchServiceError lambda "KMSDisabledException" .
      hasStatus 502

-- | AWS Lambda was not able to set up VPC access for the Lambda function because one or more configured subnets has no available IP addresses.
--
--
_SubnetIPAddressLimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetIPAddressLimitReachedException
  = _MatchServiceError lambda
      "SubnetIPAddressLimitReachedException"
      . hasStatus 502

-- | The request throughput limit was exceeded.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException
  = _MatchServiceError lambda
      "TooManyRequestsException"
      . hasStatus 429

-- | The AWS Lambda service encountered an internal error.
--
--
_ServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceException
  = _MatchServiceError lambda "ServiceException" .
      hasStatus 500

-- | The runtime or runtime version specified is not supported.
--
--
_InvalidRuntimeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRuntimeException
  = _MatchServiceError lambda "InvalidRuntimeException"
      . hasStatus 502

-- | AWS Lambda was throttled by Amazon EC2 during Lambda function initialization using the execution role provided for the Lambda function.
--
--
_EC2ThrottledException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2ThrottledException
  = _MatchServiceError lambda "EC2ThrottledException" .
      hasStatus 502

-- | The request payload exceeded the @Invoke@ request body JSON input limit. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/limits.html Limits> . 
--
--
_RequestTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTooLargeException
  = _MatchServiceError lambda
      "RequestTooLargeException"
      . hasStatus 413

-- | AWS Lambda was not able to create an elastic network interface in the VPC, specified as part of Lambda function configuration, because the limit for network interfaces has been reached.
--
--
_ENILimitReachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ENILimitReachedException
  = _MatchServiceError lambda
      "ENILimitReachedException"
      . hasStatus 502

-- | One of the parameters in the request is invalid.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException
  = _MatchServiceError lambda
      "InvalidParameterValueException"
      . hasStatus 400

-- | Lambda was unable to decrypt the environment variables because the KMS key was not found. Check the function's KMS key settings. 
--
--
_KMSNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSNotFoundException
  = _MatchServiceError lambda "KMSNotFoundException" .
      hasStatus 502

-- | The request body could not be parsed as JSON.
--
--
_InvalidRequestContentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestContentException
  = _MatchServiceError lambda
      "InvalidRequestContentException"
      . hasStatus 400

-- | The resource specified in the request does not exist.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError lambda
      "ResourceNotFoundException"
      . hasStatus 404

-- | AWS Lambda could not unzip the deployment package.
--
--
_InvalidZipFileException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidZipFileException
  = _MatchServiceError lambda "InvalidZipFileException"
      . hasStatus 502

-- | The function is inactive and its VPC connection is no longer available. Wait for the VPC connection to reestablish and try again.
--
--
_ResourceNotReadyException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotReadyException
  = _MatchServiceError lambda
      "ResourceNotReadyException"
      . hasStatus 502

-- | The specified configuration does not exist.
--
--
_ProvisionedConcurrencyConfigNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ProvisionedConcurrencyConfigNotFoundException
  = _MatchServiceError lambda
      "ProvisionedConcurrencyConfigNotFoundException"
      . hasStatus 404

-- | The resource already exists, or another operation is in progress.
--
--
_ResourceConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceConflictException
  = _MatchServiceError lambda
      "ResourceConflictException"
      . hasStatus 409

-- | Need additional permissions to configure VPC settings.
--
--
_EC2AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2AccessDeniedException
  = _MatchServiceError lambda
      "EC2AccessDeniedException"
      . hasStatus 502

-- | The Subnet ID provided in the Lambda function VPC configuration is invalid.
--
--
_InvalidSubnetIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnetIdException
  = _MatchServiceError lambda
      "InvalidSubnetIDException"
      . hasStatus 502

-- | The Security Group ID provided in the Lambda function VPC configuration is invalid.
--
--
_InvalidSecurityGroupIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSecurityGroupIdException
  = _MatchServiceError lambda
      "InvalidSecurityGroupIDException"
      . hasStatus 502

-- | Lambda was unable to decrypt the environment variables because the KMS key used is in an invalid state for Decrypt. Check the function's KMS key settings.
--
--
_KMSInvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSInvalidStateException
  = _MatchServiceError lambda
      "KMSInvalidStateException"
      . hasStatus 502

-- | Lambda was unable to decrypt the environment variables because KMS access was denied. Check the Lambda function's KMS permissions.
--
--
_KMSAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSAccessDeniedException
  = _MatchServiceError lambda
      "KMSAccessDeniedException"
      . hasStatus 502

-- | The operation conflicts with the resource's availability. For example, you attempted to update an EventSource Mapping in CREATING, or tried to delete a EventSource mapping currently in the UPDATING state. 
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError lambda "ResourceInUseException"
      . hasStatus 400

-- | AWS Lambda received an unexpected EC2 client exception while setting up for the Lambda function.
--
--
_EC2UnexpectedException :: AsError a => Getting (First ServiceError) a ServiceError
_EC2UnexpectedException
  = _MatchServiceError lambda "EC2UnexpectedException"
      . hasStatus 502

-- | The content type of the @Invoke@ request body is not JSON.
--
--
_UnsupportedMediaTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedMediaTypeException
  = _MatchServiceError lambda
      "UnsupportedMediaTypeException"
      . hasStatus 415
