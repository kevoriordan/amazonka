{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types
    (
    -- * Service Configuration
      dax

    -- * Errors
    , _SubnetGroupNotFoundFault
    , _SubnetGroupInUseFault
    , _ClusterQuotaForCustomerExceededFault
    , _InvalidSubnet
    , _TagQuotaPerResourceExceeded
    , _InvalidParameterGroupStateFault
    , _ParameterGroupNotFoundFault
    , _InvalidParameterValueException
    , _ServiceLinkedRoleNotFoundFault
    , _InsufficientClusterCapacityFault
    , _ParameterGroupQuotaExceededFault
    , _InvalidClusterStateFault
    , _NodeQuotaForClusterExceededFault
    , _InvalidARNFault
    , _ClusterAlreadyExistsFault
    , _SubnetGroupQuotaExceededFault
    , _ClusterNotFoundFault
    , _TagNotFoundFault
    , _NodeQuotaForCustomerExceededFault
    , _SubnetGroupAlreadyExistsFault
    , _ParameterGroupAlreadyExistsFault
    , _InvalidVPCNetworkStateFault
    , _SubnetInUse
    , _SubnetQuotaExceededFault
    , _NodeNotFoundFault
    , _InvalidParameterCombinationException

    -- * ChangeType
    , ChangeType (..)

    -- * IsModifiable
    , IsModifiable (..)

    -- * ParameterType
    , ParameterType (..)

    -- * SSEStatus
    , SSEStatus (..)

    -- * SourceType
    , SourceType (..)

    -- * Cluster
    , Cluster
    , cluster
    , cStatus
    , cIAMRoleARN
    , cClusterARN
    , cActiveNodes
    , cSecurityGroups
    , cNotificationConfiguration
    , cNodeIdsToRemove
    , cTotalNodes
    , cPreferredMaintenanceWindow
    , cSubnetGroup
    , cClusterName
    , cNodeType
    , cNodes
    , cClusterDiscoveryEndpoint
    , cSSEDescription
    , cDescription
    , cParameterGroup

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * Event
    , Event
    , event
    , eSourceName
    , eSourceType
    , eDate
    , eMessage

    -- * Node
    , Node
    , node
    , nNodeStatus
    , nParameterGroupStatus
    , nAvailabilityZone
    , nNodeId
    , nEndpoint
    , nNodeCreateTime

    -- * NodeTypeSpecificValue
    , NodeTypeSpecificValue
    , nodeTypeSpecificValue
    , ntsvValue
    , ntsvNodeType

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pParameterType
    , pSource
    , pIsModifiable
    , pDataType
    , pNodeTypeSpecificValues
    , pAllowedValues
    , pParameterName
    , pDescription
    , pChangeType

    -- * ParameterGroup
    , ParameterGroup
    , parameterGroup
    , pgDescription
    , pgParameterGroupName

    -- * ParameterGroupStatus
    , ParameterGroupStatus
    , parameterGroupStatus
    , pgsNodeIdsToReboot
    , pgsParameterApplyStatus
    , pgsParameterGroupName

    -- * ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- * SSEDescription
    , SSEDescription
    , sSEDescription
    , ssedStatus

    -- * SSESpecification
    , SSESpecification
    , sSESpecification
    , ssesEnabled

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupIdentifier

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SubnetGroup
    , SubnetGroup
    , subnetGroup
    , sgVPCId
    , sgSubnets
    , sgSubnetGroupName
    , sgDescription

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.ParameterType
import Network.AWS.DAX.Types.SSEStatus
import Network.AWS.DAX.Types.SourceType
import Network.AWS.DAX.Types.Cluster
import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Event
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.Parameter
import Network.AWS.DAX.Types.ParameterGroup
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.ParameterNameValue
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SSESpecification
import Network.AWS.DAX.Types.SecurityGroupMembership
import Network.AWS.DAX.Types.Subnet
import Network.AWS.DAX.Types.SubnetGroup
import Network.AWS.DAX.Types.Tag

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
dax :: Service
dax
  = Service{_svcAbbrev = "DAX", _svcSigner = v4,
            _svcPrefix = "dax", _svcVersion = "2017-04-19",
            _svcEndpoint = defaultEndpoint dax,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "DAX", _svcRetry = retry}
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

-- | The requested subnet group name does not refer to an existing subnet group.
--
--
_SubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupNotFoundFault
  = _MatchServiceError dax "SubnetGroupNotFoundFault"

-- | The specified subnet group is currently in use.
--
--
_SubnetGroupInUseFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupInUseFault
  = _MatchServiceError dax "SubnetGroupInUseFault"

-- | You have attempted to exceed the maximum number of DAX clusters for your AWS account.
--
--
_ClusterQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaForCustomerExceededFault
  = _MatchServiceError dax
      "ClusterQuotaForCustomerExceededFault"

-- | An invalid subnet identifier was specified.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet
  = _MatchServiceError dax "InvalidSubnet"

-- | You have exceeded the maximum number of tags for this DAX cluster.
--
--
_TagQuotaPerResourceExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_TagQuotaPerResourceExceeded
  = _MatchServiceError dax
      "TagQuotaPerResourceExceeded"

-- | One or more parameters in a parameter group are in an invalid state.
--
--
_InvalidParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterGroupStateFault
  = _MatchServiceError dax
      "InvalidParameterGroupStateFault"

-- | The specified parameter group does not exist.
--
--
_ParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupNotFoundFault
  = _MatchServiceError dax
      "ParameterGroupNotFoundFault"

-- | The value for a parameter is invalid.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException
  = _MatchServiceError dax
      "InvalidParameterValueException"

-- | The specified service linked role (SLR) was not found.
--
--
_ServiceLinkedRoleNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceLinkedRoleNotFoundFault
  = _MatchServiceError dax
      "ServiceLinkedRoleNotFoundFault"

-- | There are not enough system resources to create the cluster you requested (or to resize an already-existing cluster). 
--
--
_InsufficientClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientClusterCapacityFault
  = _MatchServiceError dax
      "InsufficientClusterCapacityFault"

-- | You have attempted to exceed the maximum number of parameter groups.
--
--
_ParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupQuotaExceededFault
  = _MatchServiceError dax
      "ParameterGroupQuotaExceededFault"

-- | The requested DAX cluster is not in the /available/ state.
--
--
_InvalidClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterStateFault
  = _MatchServiceError dax "InvalidClusterStateFault"

-- | You have attempted to exceed the maximum number of nodes for a DAX cluster.
--
--
_NodeQuotaForClusterExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForClusterExceededFault
  = _MatchServiceError dax
      "NodeQuotaForClusterExceededFault"

-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
--
--
_InvalidARNFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNFault
  = _MatchServiceError dax "InvalidARNFault"

-- | You already have a DAX cluster with the given identifier.
--
--
_ClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterAlreadyExistsFault
  = _MatchServiceError dax "ClusterAlreadyExistsFault"

-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
--
--
_SubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupQuotaExceededFault
  = _MatchServiceError dax
      "SubnetGroupQuotaExceededFault"

-- | The requested cluster ID does not refer to an existing DAX cluster.
--
--
_ClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundFault
  = _MatchServiceError dax "ClusterNotFoundFault"

-- | The tag does not exist.
--
--
_TagNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagNotFoundFault
  = _MatchServiceError dax "TagNotFoundFault"

-- | You have attempted to exceed the maximum number of nodes for your AWS account.
--
--
_NodeQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForCustomerExceededFault
  = _MatchServiceError dax
      "NodeQuotaForCustomerExceededFault"

-- | The specified subnet group already exists.
--
--
_SubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupAlreadyExistsFault
  = _MatchServiceError dax
      "SubnetGroupAlreadyExistsFault"

-- | The specified parameter group already exists.
--
--
_ParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupAlreadyExistsFault
  = _MatchServiceError dax
      "ParameterGroupAlreadyExistsFault"

-- | The VPC network is in an invalid state.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault
  = _MatchServiceError dax
      "InvalidVPCNetworkStateFault"

-- | The requested subnet is being used by another subnet group.
--
--
_SubnetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetInUse = _MatchServiceError dax "SubnetInUse"

-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
--
--
_SubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetQuotaExceededFault
  = _MatchServiceError dax "SubnetQuotaExceededFault"

-- | None of the nodes in the cluster have the given node ID.
--
--
_NodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeNotFoundFault
  = _MatchServiceError dax "NodeNotFoundFault"

-- | Two or more incompatible parameters were specified.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException
  = _MatchServiceError dax
      "InvalidParameterCombinationException"
