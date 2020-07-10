{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateTargetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target group.
--
--
-- To register targets with the target group, use 'RegisterTargets' . To update the health check settings for the target group, use 'ModifyTargetGroup' . To monitor the health of targets in the target group, use 'DescribeTargetHealth' .
--
-- To route traffic to the targets in a target group, specify the target group in an action using 'CreateListener' or 'CreateRule' .
--
-- To delete a target group, use 'DeleteTargetGroup' .
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.
--
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html Target Groups for Your Application Load Balancers> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html Target Groups for Your Network Load Balancers> in the /Network Load Balancers Guide/ .
--
module Network.AWS.ELBv2.CreateTargetGroup
    (
    -- * Creating a Request
      createTargetGroup
    , CreateTargetGroup
    -- * Request Lenses
    , ctgMatcher
    , ctgHealthCheckPath
    , ctgHealthCheckEnabled
    , ctgUnhealthyThresholdCount
    , ctgVPCId
    , ctgProtocol
    , ctgHealthCheckIntervalSeconds
    , ctgTargetType
    , ctgHealthyThresholdCount
    , ctgHealthCheckProtocol
    , ctgHealthCheckTimeoutSeconds
    , ctgHealthCheckPort
    , ctgPort
    , ctgName

    -- * Destructuring the Response
    , createTargetGroupResponse
    , CreateTargetGroupResponse
    -- * Response Lenses
    , ctgrsTargetGroups
    , ctgrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTargetGroup' smart constructor.
data CreateTargetGroup = CreateTargetGroup'{_ctgMatcher
                                            :: !(Maybe Matcher),
                                            _ctgHealthCheckPath ::
                                            !(Maybe Text),
                                            _ctgHealthCheckEnabled ::
                                            !(Maybe Bool),
                                            _ctgUnhealthyThresholdCount ::
                                            !(Maybe Nat),
                                            _ctgVPCId :: !(Maybe Text),
                                            _ctgProtocol ::
                                            !(Maybe ProtocolEnum),
                                            _ctgHealthCheckIntervalSeconds ::
                                            !(Maybe Nat),
                                            _ctgTargetType ::
                                            !(Maybe TargetTypeEnum),
                                            _ctgHealthyThresholdCount ::
                                            !(Maybe Nat),
                                            _ctgHealthCheckProtocol ::
                                            !(Maybe ProtocolEnum),
                                            _ctgHealthCheckTimeoutSeconds ::
                                            !(Maybe Nat),
                                            _ctgHealthCheckPort ::
                                            !(Maybe Text),
                                            _ctgPort :: !(Maybe Nat),
                                            _ctgName :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgMatcher' - [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
--
-- * 'ctgHealthCheckPath' - [HTTP/HTTPS health checks] The ping path that is the destination on the targets for health checks. The default is /.
--
-- * 'ctgHealthCheckEnabled' - Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
--
-- * 'ctgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering a target unhealthy. For target groups with a protocol of HTTP or HTTPS, the default is 2. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count. If the target type is @lambda@ , the default is 2.
--
-- * 'ctgVPCId' - The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
--
-- * 'ctgProtocol' - The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
--
-- * 'ctgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target. For HTTP and HTTPS health checks, the range is 5–300 seconds. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target type is @lambda@ , the default is 35 seconds.
--
-- * 'ctgTargetType' - The type of target that you must specify when registering targets with this target group. You can't specify targets for a target group using more than one target type.     * @instance@ - Targets are specified by instance ID. This is the default value. If the target group protocol is UDP or TCP_UDP, the target type must be @instance@ .     * @ip@ - Targets are specified by IP address. You can specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.     * @lambda@ - The target groups contains a single Lambda function.
--
-- * 'ctgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP or TLS, the default is 3. If the target type is @lambda@ , the default is 5.
--
-- * 'ctgHealthCheckProtocol' - The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers, the default is TCP. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The TLS, UDP, and TCP_UDP protocols are not supported for health checks.
--
-- * 'ctgHealthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP or HTTPS, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
--
-- * 'ctgHealthCheckPort' - The port the load balancer uses when performing health checks on targets. The default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer.
--
-- * 'ctgPort' - The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply.
--
-- * 'ctgName' - The name of the target group. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
createTargetGroup
    :: Text -- ^ 'ctgName'
    -> CreateTargetGroup
createTargetGroup pName_
  = CreateTargetGroup'{_ctgMatcher = Nothing,
                       _ctgHealthCheckPath = Nothing,
                       _ctgHealthCheckEnabled = Nothing,
                       _ctgUnhealthyThresholdCount = Nothing,
                       _ctgVPCId = Nothing, _ctgProtocol = Nothing,
                       _ctgHealthCheckIntervalSeconds = Nothing,
                       _ctgTargetType = Nothing,
                       _ctgHealthyThresholdCount = Nothing,
                       _ctgHealthCheckProtocol = Nothing,
                       _ctgHealthCheckTimeoutSeconds = Nothing,
                       _ctgHealthCheckPort = Nothing, _ctgPort = Nothing,
                       _ctgName = pName_}

-- | [HTTP/HTTPS health checks] The HTTP codes to use when checking for a successful response from a target.
ctgMatcher :: Lens' CreateTargetGroup (Maybe Matcher)
ctgMatcher = lens _ctgMatcher (\ s a -> s{_ctgMatcher = a})

-- | [HTTP/HTTPS health checks] The ping path that is the destination on the targets for health checks. The default is /.
ctgHealthCheckPath :: Lens' CreateTargetGroup (Maybe Text)
ctgHealthCheckPath = lens _ctgHealthCheckPath (\ s a -> s{_ctgHealthCheckPath = a})

-- | Indicates whether health checks are enabled. If the target type is @lambda@ , health checks are disabled by default but can be enabled. If the target type is @instance@ or @ip@ , health checks are always enabled and cannot be disabled.
ctgHealthCheckEnabled :: Lens' CreateTargetGroup (Maybe Bool)
ctgHealthCheckEnabled = lens _ctgHealthCheckEnabled (\ s a -> s{_ctgHealthCheckEnabled = a})

-- | The number of consecutive health check failures required before considering a target unhealthy. For target groups with a protocol of HTTP or HTTPS, the default is 2. For target groups with a protocol of TCP or TLS, this value must be the same as the healthy threshold count. If the target type is @lambda@ , the default is 2.
ctgUnhealthyThresholdCount :: Lens' CreateTargetGroup (Maybe Natural)
ctgUnhealthyThresholdCount = lens _ctgUnhealthyThresholdCount (\ s a -> s{_ctgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The identifier of the virtual private cloud (VPC). If the target is a Lambda function, this parameter does not apply. Otherwise, this parameter is required.
ctgVPCId :: Lens' CreateTargetGroup (Maybe Text)
ctgVPCId = lens _ctgVPCId (\ s a -> s{_ctgVPCId = a})

-- | The protocol to use for routing traffic to the targets. For Application Load Balancers, the supported protocols are HTTP and HTTPS. For Network Load Balancers, the supported protocols are TCP, TLS, UDP, or TCP_UDP. A TCP_UDP listener must be associated with a TCP_UDP target group. If the target is a Lambda function, this parameter does not apply.
ctgProtocol :: Lens' CreateTargetGroup (Maybe ProtocolEnum)
ctgProtocol = lens _ctgProtocol (\ s a -> s{_ctgProtocol = a})

-- | The approximate amount of time, in seconds, between health checks of an individual target. For HTTP and HTTPS health checks, the range is 5–300 seconds. For TCP health checks, the supported values are 10 and 30 seconds. If the target type is @instance@ or @ip@ , the default is 30 seconds. If the target type is @lambda@ , the default is 35 seconds.
ctgHealthCheckIntervalSeconds :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthCheckIntervalSeconds = lens _ctgHealthCheckIntervalSeconds (\ s a -> s{_ctgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The type of target that you must specify when registering targets with this target group. You can't specify targets for a target group using more than one target type.     * @instance@ - Targets are specified by instance ID. This is the default value. If the target group protocol is UDP or TCP_UDP, the target type must be @instance@ .     * @ip@ - Targets are specified by IP address. You can specify IP addresses from the subnets of the virtual private cloud (VPC) for the target group, the RFC 1918 range (10.0.0.0/8, 172.16.0.0/12, and 192.168.0.0/16), and the RFC 6598 range (100.64.0.0/10). You can't specify publicly routable IP addresses.     * @lambda@ - The target groups contains a single Lambda function.
ctgTargetType :: Lens' CreateTargetGroup (Maybe TargetTypeEnum)
ctgTargetType = lens _ctgTargetType (\ s a -> s{_ctgTargetType = a})

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy. For target groups with a protocol of HTTP or HTTPS, the default is 5. For target groups with a protocol of TCP or TLS, the default is 3. If the target type is @lambda@ , the default is 5.
ctgHealthyThresholdCount :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthyThresholdCount = lens _ctgHealthyThresholdCount (\ s a -> s{_ctgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol the load balancer uses when performing health checks on targets. For Application Load Balancers, the default is HTTP. For Network Load Balancers, the default is TCP. The TCP protocol is supported for health checks only if the protocol of the target group is TCP, TLS, UDP, or TCP_UDP. The TLS, UDP, and TCP_UDP protocols are not supported for health checks.
ctgHealthCheckProtocol :: Lens' CreateTargetGroup (Maybe ProtocolEnum)
ctgHealthCheckProtocol = lens _ctgHealthCheckProtocol (\ s a -> s{_ctgHealthCheckProtocol = a})

-- | The amount of time, in seconds, during which no response from a target means a failed health check. For target groups with a protocol of HTTP or HTTPS, the default is 5 seconds. For target groups with a protocol of TCP or TLS, this value must be 6 seconds for HTTP health checks and 10 seconds for TCP and HTTPS health checks. If the target type is @lambda@ , the default is 30 seconds.
ctgHealthCheckTimeoutSeconds :: Lens' CreateTargetGroup (Maybe Natural)
ctgHealthCheckTimeoutSeconds = lens _ctgHealthCheckTimeoutSeconds (\ s a -> s{_ctgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port the load balancer uses when performing health checks on targets. The default is @traffic-port@ , which is the port on which each target receives traffic from the load balancer.
ctgHealthCheckPort :: Lens' CreateTargetGroup (Maybe Text)
ctgHealthCheckPort = lens _ctgHealthCheckPort (\ s a -> s{_ctgHealthCheckPort = a})

-- | The port on which the targets receive traffic. This port is used unless you specify a port override when registering the target. If the target is a Lambda function, this parameter does not apply.
ctgPort :: Lens' CreateTargetGroup (Maybe Natural)
ctgPort = lens _ctgPort (\ s a -> s{_ctgPort = a}) . mapping _Nat

-- | The name of the target group. This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, and must not begin or end with a hyphen.
ctgName :: Lens' CreateTargetGroup Text
ctgName = lens _ctgName (\ s a -> s{_ctgName = a})

instance AWSRequest CreateTargetGroup where
        type Rs CreateTargetGroup = CreateTargetGroupResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "CreateTargetGroupResult"
              (\ s h x ->
                 CreateTargetGroupResponse' <$>
                   (x .@? "TargetGroups" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateTargetGroup where

instance NFData CreateTargetGroup where

instance ToHeaders CreateTargetGroup where
        toHeaders = const mempty

instance ToPath CreateTargetGroup where
        toPath = const "/"

instance ToQuery CreateTargetGroup where
        toQuery CreateTargetGroup'{..}
          = mconcat
              ["Action" =: ("CreateTargetGroup" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Matcher" =: _ctgMatcher,
               "HealthCheckPath" =: _ctgHealthCheckPath,
               "HealthCheckEnabled" =: _ctgHealthCheckEnabled,
               "UnhealthyThresholdCount" =:
                 _ctgUnhealthyThresholdCount,
               "VpcId" =: _ctgVPCId, "Protocol" =: _ctgProtocol,
               "HealthCheckIntervalSeconds" =:
                 _ctgHealthCheckIntervalSeconds,
               "TargetType" =: _ctgTargetType,
               "HealthyThresholdCount" =: _ctgHealthyThresholdCount,
               "HealthCheckProtocol" =: _ctgHealthCheckProtocol,
               "HealthCheckTimeoutSeconds" =:
                 _ctgHealthCheckTimeoutSeconds,
               "HealthCheckPort" =: _ctgHealthCheckPort,
               "Port" =: _ctgPort, "Name" =: _ctgName]

-- | /See:/ 'createTargetGroupResponse' smart constructor.
data CreateTargetGroupResponse = CreateTargetGroupResponse'{_ctgrsTargetGroups
                                                            ::
                                                            !(Maybe
                                                                [TargetGroup]),
                                                            _ctgrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'CreateTargetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgrsTargetGroups' - Information about the target group.
--
-- * 'ctgrsResponseStatus' - -- | The response status code.
createTargetGroupResponse
    :: Int -- ^ 'ctgrsResponseStatus'
    -> CreateTargetGroupResponse
createTargetGroupResponse pResponseStatus_
  = CreateTargetGroupResponse'{_ctgrsTargetGroups =
                                 Nothing,
                               _ctgrsResponseStatus = pResponseStatus_}

-- | Information about the target group.
ctgrsTargetGroups :: Lens' CreateTargetGroupResponse [TargetGroup]
ctgrsTargetGroups = lens _ctgrsTargetGroups (\ s a -> s{_ctgrsTargetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ctgrsResponseStatus :: Lens' CreateTargetGroupResponse Int
ctgrsResponseStatus = lens _ctgrsResponseStatus (\ s a -> s{_ctgrsResponseStatus = a})

instance NFData CreateTargetGroupResponse where
