{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetGroup where

import Network.AWS.ELBv2.Types.Matcher
import Network.AWS.ELBv2.Types.ProtocolEnum
import Network.AWS.ELBv2.Types.TargetTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a target group.
--
--
--
-- /See:/ 'targetGroup' smart constructor.
data TargetGroup = TargetGroup'{_tgMatcher ::
                                !(Maybe Matcher),
                                _tgHealthCheckPath :: !(Maybe Text),
                                _tgHealthCheckEnabled :: !(Maybe Bool),
                                _tgUnhealthyThresholdCount :: !(Maybe Nat),
                                _tgVPCId :: !(Maybe Text),
                                _tgTargetGroupARN :: !(Maybe Text),
                                _tgProtocol :: !(Maybe ProtocolEnum),
                                _tgHealthCheckIntervalSeconds :: !(Maybe Nat),
                                _tgTargetType :: !(Maybe TargetTypeEnum),
                                _tgHealthyThresholdCount :: !(Maybe Nat),
                                _tgHealthCheckProtocol :: !(Maybe ProtocolEnum),
                                _tgLoadBalancerARNs :: !(Maybe [Text]),
                                _tgHealthCheckTimeoutSeconds :: !(Maybe Nat),
                                _tgHealthCheckPort :: !(Maybe Text),
                                _tgTargetGroupName :: !(Maybe Text),
                                _tgPort :: !(Maybe Nat)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgMatcher' - The HTTP codes to use when checking for a successful response from a target.
--
-- * 'tgHealthCheckPath' - The destination for the health check request.
--
-- * 'tgHealthCheckEnabled' - Indicates whether health checks are enabled.
--
-- * 'tgUnhealthyThresholdCount' - The number of consecutive health check failures required before considering the target unhealthy.
--
-- * 'tgVPCId' - The ID of the VPC for the targets.
--
-- * 'tgTargetGroupARN' - The Amazon Resource Name (ARN) of the target group.
--
-- * 'tgProtocol' - The protocol to use for routing traffic to the targets.
--
-- * 'tgHealthCheckIntervalSeconds' - The approximate amount of time, in seconds, between health checks of an individual target.
--
-- * 'tgTargetType' - The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
--
-- * 'tgHealthyThresholdCount' - The number of consecutive health checks successes required before considering an unhealthy target healthy.
--
-- * 'tgHealthCheckProtocol' - The protocol to use to connect with the target.
--
-- * 'tgLoadBalancerARNs' - The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
--
-- * 'tgHealthCheckTimeoutSeconds' - The amount of time, in seconds, during which no response means a failed health check.
--
-- * 'tgHealthCheckPort' - The port to use to connect with the target.
--
-- * 'tgTargetGroupName' - The name of the target group.
--
-- * 'tgPort' - The port on which the targets are listening. Not used if the target is a Lambda function.
targetGroup
    :: TargetGroup
targetGroup
  = TargetGroup'{_tgMatcher = Nothing,
                 _tgHealthCheckPath = Nothing,
                 _tgHealthCheckEnabled = Nothing,
                 _tgUnhealthyThresholdCount = Nothing,
                 _tgVPCId = Nothing, _tgTargetGroupARN = Nothing,
                 _tgProtocol = Nothing,
                 _tgHealthCheckIntervalSeconds = Nothing,
                 _tgTargetType = Nothing,
                 _tgHealthyThresholdCount = Nothing,
                 _tgHealthCheckProtocol = Nothing,
                 _tgLoadBalancerARNs = Nothing,
                 _tgHealthCheckTimeoutSeconds = Nothing,
                 _tgHealthCheckPort = Nothing,
                 _tgTargetGroupName = Nothing, _tgPort = Nothing}

-- | The HTTP codes to use when checking for a successful response from a target.
tgMatcher :: Lens' TargetGroup (Maybe Matcher)
tgMatcher = lens _tgMatcher (\ s a -> s{_tgMatcher = a})

-- | The destination for the health check request.
tgHealthCheckPath :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPath = lens _tgHealthCheckPath (\ s a -> s{_tgHealthCheckPath = a})

-- | Indicates whether health checks are enabled.
tgHealthCheckEnabled :: Lens' TargetGroup (Maybe Bool)
tgHealthCheckEnabled = lens _tgHealthCheckEnabled (\ s a -> s{_tgHealthCheckEnabled = a})

-- | The number of consecutive health check failures required before considering the target unhealthy.
tgUnhealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgUnhealthyThresholdCount = lens _tgUnhealthyThresholdCount (\ s a -> s{_tgUnhealthyThresholdCount = a}) . mapping _Nat

-- | The ID of the VPC for the targets.
tgVPCId :: Lens' TargetGroup (Maybe Text)
tgVPCId = lens _tgVPCId (\ s a -> s{_tgVPCId = a})

-- | The Amazon Resource Name (ARN) of the target group.
tgTargetGroupARN :: Lens' TargetGroup (Maybe Text)
tgTargetGroupARN = lens _tgTargetGroupARN (\ s a -> s{_tgTargetGroupARN = a})

-- | The protocol to use for routing traffic to the targets.
tgProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgProtocol = lens _tgProtocol (\ s a -> s{_tgProtocol = a})

-- | The approximate amount of time, in seconds, between health checks of an individual target.
tgHealthCheckIntervalSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckIntervalSeconds = lens _tgHealthCheckIntervalSeconds (\ s a -> s{_tgHealthCheckIntervalSeconds = a}) . mapping _Nat

-- | The type of target that you must specify when registering targets with this target group. The possible values are @instance@ (targets are specified by instance ID) or @ip@ (targets are specified by IP address).
tgTargetType :: Lens' TargetGroup (Maybe TargetTypeEnum)
tgTargetType = lens _tgTargetType (\ s a -> s{_tgTargetType = a})

-- | The number of consecutive health checks successes required before considering an unhealthy target healthy.
tgHealthyThresholdCount :: Lens' TargetGroup (Maybe Natural)
tgHealthyThresholdCount = lens _tgHealthyThresholdCount (\ s a -> s{_tgHealthyThresholdCount = a}) . mapping _Nat

-- | The protocol to use to connect with the target.
tgHealthCheckProtocol :: Lens' TargetGroup (Maybe ProtocolEnum)
tgHealthCheckProtocol = lens _tgHealthCheckProtocol (\ s a -> s{_tgHealthCheckProtocol = a})

-- | The Amazon Resource Names (ARN) of the load balancers that route traffic to this target group.
tgLoadBalancerARNs :: Lens' TargetGroup [Text]
tgLoadBalancerARNs = lens _tgLoadBalancerARNs (\ s a -> s{_tgLoadBalancerARNs = a}) . _Default . _Coerce

-- | The amount of time, in seconds, during which no response means a failed health check.
tgHealthCheckTimeoutSeconds :: Lens' TargetGroup (Maybe Natural)
tgHealthCheckTimeoutSeconds = lens _tgHealthCheckTimeoutSeconds (\ s a -> s{_tgHealthCheckTimeoutSeconds = a}) . mapping _Nat

-- | The port to use to connect with the target.
tgHealthCheckPort :: Lens' TargetGroup (Maybe Text)
tgHealthCheckPort = lens _tgHealthCheckPort (\ s a -> s{_tgHealthCheckPort = a})

-- | The name of the target group.
tgTargetGroupName :: Lens' TargetGroup (Maybe Text)
tgTargetGroupName = lens _tgTargetGroupName (\ s a -> s{_tgTargetGroupName = a})

-- | The port on which the targets are listening. Not used if the target is a Lambda function.
tgPort :: Lens' TargetGroup (Maybe Natural)
tgPort = lens _tgPort (\ s a -> s{_tgPort = a}) . mapping _Nat

instance FromXML TargetGroup where
        parseXML x
          = TargetGroup' <$>
              (x .@? "Matcher") <*> (x .@? "HealthCheckPath") <*>
                (x .@? "HealthCheckEnabled")
                <*> (x .@? "UnhealthyThresholdCount")
                <*> (x .@? "VpcId")
                <*> (x .@? "TargetGroupArn")
                <*> (x .@? "Protocol")
                <*> (x .@? "HealthCheckIntervalSeconds")
                <*> (x .@? "TargetType")
                <*> (x .@? "HealthyThresholdCount")
                <*> (x .@? "HealthCheckProtocol")
                <*>
                (x .@? "LoadBalancerArns" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "HealthCheckTimeoutSeconds")
                <*> (x .@? "HealthCheckPort")
                <*> (x .@? "TargetGroupName")
                <*> (x .@? "Port")

instance Hashable TargetGroup where

instance NFData TargetGroup where
