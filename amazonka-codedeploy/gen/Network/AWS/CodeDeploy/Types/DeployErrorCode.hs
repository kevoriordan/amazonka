{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeployErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.DeployErrorCode (
  DeployErrorCode (
    ..
    , AgentIssue
    , AlarmActive
    , ApplicationMissing
    , AutoScalingConfiguration
    , AutoScalingIAMRolePermissions
    , AutoscalingValidationError
    , CodedeployResourceCannotBeFound
    , CustomerApplicationUnhealthy
    , DeploymentGroupMissing
    , EcsUpdateError
    , ElasticLoadBalancingInvalid
    , ElbInvalidInstance
    , HealthConstraints
    , HealthConstraintsInvalid
    , HookExecutionFailure
    , IAMRoleMissing
    , IAMRolePermissions
    , InternalError
    , InvalidEcsService
    , InvalidLambdaConfiguration
    , InvalidLambdaFunction
    , InvalidRevision
    , ManualStop
    , MissingBlueGreenDeploymentConfiguration
    , MissingElbInformation
    , MissingGithubToken
    , NoEC2Subscription
    , NoInstances
    , OverMaxInstances
    , ResourceLimitExceeded
    , RevisionMissing
    , Throttled
    , Timeout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeployErrorCode = DeployErrorCode' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AgentIssue :: DeployErrorCode
pattern AgentIssue = DeployErrorCode' "AGENT_ISSUE"

pattern AlarmActive :: DeployErrorCode
pattern AlarmActive = DeployErrorCode' "ALARM_ACTIVE"

pattern ApplicationMissing :: DeployErrorCode
pattern ApplicationMissing = DeployErrorCode' "APPLICATION_MISSING"

pattern AutoScalingConfiguration :: DeployErrorCode
pattern AutoScalingConfiguration = DeployErrorCode' "AUTO_SCALING_CONFIGURATION"

pattern AutoScalingIAMRolePermissions :: DeployErrorCode
pattern AutoScalingIAMRolePermissions = DeployErrorCode' "AUTO_SCALING_IAM_ROLE_PERMISSIONS"

pattern AutoscalingValidationError :: DeployErrorCode
pattern AutoscalingValidationError = DeployErrorCode' "AUTOSCALING_VALIDATION_ERROR"

pattern CodedeployResourceCannotBeFound :: DeployErrorCode
pattern CodedeployResourceCannotBeFound = DeployErrorCode' "CODEDEPLOY_RESOURCE_CANNOT_BE_FOUND"

pattern CustomerApplicationUnhealthy :: DeployErrorCode
pattern CustomerApplicationUnhealthy = DeployErrorCode' "CUSTOMER_APPLICATION_UNHEALTHY"

pattern DeploymentGroupMissing :: DeployErrorCode
pattern DeploymentGroupMissing = DeployErrorCode' "DEPLOYMENT_GROUP_MISSING"

pattern EcsUpdateError :: DeployErrorCode
pattern EcsUpdateError = DeployErrorCode' "ECS_UPDATE_ERROR"

pattern ElasticLoadBalancingInvalid :: DeployErrorCode
pattern ElasticLoadBalancingInvalid = DeployErrorCode' "ELASTIC_LOAD_BALANCING_INVALID"

pattern ElbInvalidInstance :: DeployErrorCode
pattern ElbInvalidInstance = DeployErrorCode' "ELB_INVALID_INSTANCE"

pattern HealthConstraints :: DeployErrorCode
pattern HealthConstraints = DeployErrorCode' "HEALTH_CONSTRAINTS"

pattern HealthConstraintsInvalid :: DeployErrorCode
pattern HealthConstraintsInvalid = DeployErrorCode' "HEALTH_CONSTRAINTS_INVALID"

pattern HookExecutionFailure :: DeployErrorCode
pattern HookExecutionFailure = DeployErrorCode' "HOOK_EXECUTION_FAILURE"

pattern IAMRoleMissing :: DeployErrorCode
pattern IAMRoleMissing = DeployErrorCode' "IAM_ROLE_MISSING"

pattern IAMRolePermissions :: DeployErrorCode
pattern IAMRolePermissions = DeployErrorCode' "IAM_ROLE_PERMISSIONS"

pattern InternalError :: DeployErrorCode
pattern InternalError = DeployErrorCode' "INTERNAL_ERROR"

pattern InvalidEcsService :: DeployErrorCode
pattern InvalidEcsService = DeployErrorCode' "INVALID_ECS_SERVICE"

pattern InvalidLambdaConfiguration :: DeployErrorCode
pattern InvalidLambdaConfiguration = DeployErrorCode' "INVALID_LAMBDA_CONFIGURATION"

pattern InvalidLambdaFunction :: DeployErrorCode
pattern InvalidLambdaFunction = DeployErrorCode' "INVALID_LAMBDA_FUNCTION"

pattern InvalidRevision :: DeployErrorCode
pattern InvalidRevision = DeployErrorCode' "INVALID_REVISION"

pattern ManualStop :: DeployErrorCode
pattern ManualStop = DeployErrorCode' "MANUAL_STOP"

pattern MissingBlueGreenDeploymentConfiguration :: DeployErrorCode
pattern MissingBlueGreenDeploymentConfiguration = DeployErrorCode' "MISSING_BLUE_GREEN_DEPLOYMENT_CONFIGURATION"

pattern MissingElbInformation :: DeployErrorCode
pattern MissingElbInformation = DeployErrorCode' "MISSING_ELB_INFORMATION"

pattern MissingGithubToken :: DeployErrorCode
pattern MissingGithubToken = DeployErrorCode' "MISSING_GITHUB_TOKEN"

pattern NoEC2Subscription :: DeployErrorCode
pattern NoEC2Subscription = DeployErrorCode' "NO_EC2_SUBSCRIPTION"

pattern NoInstances :: DeployErrorCode
pattern NoInstances = DeployErrorCode' "NO_INSTANCES"

pattern OverMaxInstances :: DeployErrorCode
pattern OverMaxInstances = DeployErrorCode' "OVER_MAX_INSTANCES"

pattern ResourceLimitExceeded :: DeployErrorCode
pattern ResourceLimitExceeded = DeployErrorCode' "RESOURCE_LIMIT_EXCEEDED"

pattern RevisionMissing :: DeployErrorCode
pattern RevisionMissing = DeployErrorCode' "REVISION_MISSING"

pattern Throttled :: DeployErrorCode
pattern Throttled = DeployErrorCode' "THROTTLED"

pattern Timeout :: DeployErrorCode
pattern Timeout = DeployErrorCode' "TIMEOUT"

{-# COMPLETE
  AgentIssue,
  AlarmActive,
  ApplicationMissing,
  AutoScalingConfiguration,
  AutoScalingIAMRolePermissions,
  AutoscalingValidationError,
  CodedeployResourceCannotBeFound,
  CustomerApplicationUnhealthy,
  DeploymentGroupMissing,
  EcsUpdateError,
  ElasticLoadBalancingInvalid,
  ElbInvalidInstance,
  HealthConstraints,
  HealthConstraintsInvalid,
  HookExecutionFailure,
  IAMRoleMissing,
  IAMRolePermissions,
  InternalError,
  InvalidEcsService,
  InvalidLambdaConfiguration,
  InvalidLambdaFunction,
  InvalidRevision,
  ManualStop,
  MissingBlueGreenDeploymentConfiguration,
  MissingElbInformation,
  MissingGithubToken,
  NoEC2Subscription,
  NoInstances,
  OverMaxInstances,
  ResourceLimitExceeded,
  RevisionMissing,
  Throttled,
  Timeout,
  DeployErrorCode' #-}

instance FromText DeployErrorCode where
    parser = (DeployErrorCode' . mk) <$> takeText

instance ToText DeployErrorCode where
    toText (DeployErrorCode' ci) = original ci

-- | Represents an enum of /known/ $DeployErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeployErrorCode where
    toEnum i = case i of
        0 -> AgentIssue
        1 -> AlarmActive
        2 -> ApplicationMissing
        3 -> AutoScalingConfiguration
        4 -> AutoScalingIAMRolePermissions
        5 -> AutoscalingValidationError
        6 -> CodedeployResourceCannotBeFound
        7 -> CustomerApplicationUnhealthy
        8 -> DeploymentGroupMissing
        9 -> EcsUpdateError
        10 -> ElasticLoadBalancingInvalid
        11 -> ElbInvalidInstance
        12 -> HealthConstraints
        13 -> HealthConstraintsInvalid
        14 -> HookExecutionFailure
        15 -> IAMRoleMissing
        16 -> IAMRolePermissions
        17 -> InternalError
        18 -> InvalidEcsService
        19 -> InvalidLambdaConfiguration
        20 -> InvalidLambdaFunction
        21 -> InvalidRevision
        22 -> ManualStop
        23 -> MissingBlueGreenDeploymentConfiguration
        24 -> MissingElbInformation
        25 -> MissingGithubToken
        26 -> NoEC2Subscription
        27 -> NoInstances
        28 -> OverMaxInstances
        29 -> ResourceLimitExceeded
        30 -> RevisionMissing
        31 -> Throttled
        32 -> Timeout
        _ -> (error . showText) $ "Unknown index for DeployErrorCode: " <> toText i
    fromEnum x = case x of
        AgentIssue -> 0
        AlarmActive -> 1
        ApplicationMissing -> 2
        AutoScalingConfiguration -> 3
        AutoScalingIAMRolePermissions -> 4
        AutoscalingValidationError -> 5
        CodedeployResourceCannotBeFound -> 6
        CustomerApplicationUnhealthy -> 7
        DeploymentGroupMissing -> 8
        EcsUpdateError -> 9
        ElasticLoadBalancingInvalid -> 10
        ElbInvalidInstance -> 11
        HealthConstraints -> 12
        HealthConstraintsInvalid -> 13
        HookExecutionFailure -> 14
        IAMRoleMissing -> 15
        IAMRolePermissions -> 16
        InternalError -> 17
        InvalidEcsService -> 18
        InvalidLambdaConfiguration -> 19
        InvalidLambdaFunction -> 20
        InvalidRevision -> 21
        ManualStop -> 22
        MissingBlueGreenDeploymentConfiguration -> 23
        MissingElbInformation -> 24
        MissingGithubToken -> 25
        NoEC2Subscription -> 26
        NoInstances -> 27
        OverMaxInstances -> 28
        ResourceLimitExceeded -> 29
        RevisionMissing -> 30
        Throttled -> 31
        Timeout -> 32
        DeployErrorCode' name -> (error . showText) $ "Unknown DeployErrorCode: " <> original name

-- | Represents the bounds of /known/ $DeployErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeployErrorCode where
    minBound = AgentIssue
    maxBound = Timeout

instance Hashable     DeployErrorCode
instance NFData       DeployErrorCode
instance ToByteString DeployErrorCode
instance ToQuery      DeployErrorCode
instance ToHeader     DeployErrorCode

instance FromJSON DeployErrorCode where
    parseJSON = parseJSONText "DeployErrorCode"
