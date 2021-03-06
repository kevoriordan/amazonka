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
-- Module      : Network.AWS.AutoScaling.CreateAutoScalingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Auto Scaling group with the specified name and attributes.
--
--
-- If you exceed your maximum limit of Auto Scaling groups, the call fails. For information about viewing this limit, see 'DescribeAccountLimits' . For information about updating this limit, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-account-limits.html Amazon EC2 Auto Scaling Service Quotas> in the /Amazon EC2 Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.CreateAutoScalingGroup
    (
    -- * Creating a Request
      createAutoScalingGroup
    , CreateAutoScalingGroup
    -- * Request Lenses
    , casgInstanceId
    , casgTerminationPolicies
    , casgHealthCheckGracePeriod
    , casgServiceLinkedRoleARN
    , casgNewInstancesProtectedFromScaleIn
    , casgVPCZoneIdentifier
    , casgTargetGroupARNs
    , casgMaxInstanceLifetime
    , casgDefaultCooldown
    , casgAvailabilityZones
    , casgDesiredCapacity
    , casgMixedInstancesPolicy
    , casgLaunchConfigurationName
    , casgLifecycleHookSpecificationList
    , casgHealthCheckType
    , casgLaunchTemplate
    , casgPlacementGroup
    , casgLoadBalancerNames
    , casgTags
    , casgAutoScalingGroupName
    , casgMinSize
    , casgMaxSize

    -- * Destructuring the Response
    , createAutoScalingGroupResponse
    , CreateAutoScalingGroupResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAutoScalingGroup' smart constructor.
data CreateAutoScalingGroup = CreateAutoScalingGroup'{_casgInstanceId
                                                      :: !(Maybe Text),
                                                      _casgTerminationPolicies
                                                      :: !(Maybe [Text]),
                                                      _casgHealthCheckGracePeriod
                                                      :: !(Maybe Int),
                                                      _casgServiceLinkedRoleARN
                                                      :: !(Maybe Text),
                                                      _casgNewInstancesProtectedFromScaleIn
                                                      :: !(Maybe Bool),
                                                      _casgVPCZoneIdentifier ::
                                                      !(Maybe Text),
                                                      _casgTargetGroupARNs ::
                                                      !(Maybe [Text]),
                                                      _casgMaxInstanceLifetime
                                                      :: !(Maybe Int),
                                                      _casgDefaultCooldown ::
                                                      !(Maybe Int),
                                                      _casgAvailabilityZones ::
                                                      !(Maybe (List1 Text)),
                                                      _casgDesiredCapacity ::
                                                      !(Maybe Int),
                                                      _casgMixedInstancesPolicy
                                                      ::
                                                      !(Maybe
                                                          MixedInstancesPolicy),
                                                      _casgLaunchConfigurationName
                                                      :: !(Maybe Text),
                                                      _casgLifecycleHookSpecificationList
                                                      ::
                                                      !(Maybe
                                                          [LifecycleHookSpecification]),
                                                      _casgHealthCheckType ::
                                                      !(Maybe Text),
                                                      _casgLaunchTemplate ::
                                                      !(Maybe
                                                          LaunchTemplateSpecification),
                                                      _casgPlacementGroup ::
                                                      !(Maybe Text),
                                                      _casgLoadBalancerNames ::
                                                      !(Maybe [Text]),
                                                      _casgTags ::
                                                      !(Maybe [Tag]),
                                                      _casgAutoScalingGroupName
                                                      :: !Text,
                                                      _casgMinSize :: !Int,
                                                      _casgMaxSize :: !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'CreateAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casgInstanceId' - The ID of the instance used to create a launch configuration for the group. When you specify an ID of an instance, Amazon EC2 Auto Scaling creates a new launch configuration and associates it with the group. This launch configuration derives its attributes from the specified instance, except for the block device mapping. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Create an Auto Scaling Group Using an EC2 Instance> in the /Amazon EC2 Auto Scaling User Guide/ . You must specify one of the following parameters in your request: @LaunchConfigurationName@ , @LaunchTemplate@ , @InstanceId@ , or @MixedInstancesPolicy@ .
--
-- * 'casgTerminationPolicies' - One or more termination policies used to select the instance to terminate. These policies are executed in the order that they are listed. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgHealthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health Check Grace Period> in the /Amazon EC2 Auto Scaling User Guide/ . Conditional: This parameter is required if you are adding an @ELB@ health check.
--
-- * 'casgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-Linked Roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgVPCZoneIdentifier' - A comma-separated list of subnet IDs for your virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones. Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
--
-- * 'casgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Using a Load Balancer with an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgMaxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in service. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling Instances Based on Maximum Instance Lifetime> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 604800.
--
-- * 'casgDefaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling Cooldowns> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgAvailabilityZones' - One or more Availability Zones for the group. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ . Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
--
-- * 'casgDesiredCapacity' - The number of Amazon EC2 instances that the Auto Scaling group attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
--
-- * 'casgMixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used. The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacity, but also the parameters that specify the instance configuration information—the launch template and instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_MixedInstancesPolicy.html MixedInstancesPolicy> in the /Amazon EC2 Auto Scaling API Reference/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling Groups with Multiple Instance Types and Purchase Options> in the /Amazon EC2 Auto Scaling User Guide/ . You must specify one of the following parameters in your request: @LaunchConfigurationName@ , @LaunchTemplate@ , @InstanceId@ , or @MixedInstancesPolicy@ .
--
-- * 'casgLaunchConfigurationName' - The name of the launch configuration. If you do not specify @LaunchConfigurationName@ , you must specify one of the following parameters: @InstanceId@ , @LaunchTemplate@ , or @MixedInstancesPolicy@ .
--
-- * 'casgLifecycleHookSpecificationList' - One or more lifecycle hooks.
--
-- * 'casgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ . The default value is @EC2@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health Checks for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgLaunchTemplate' - The launch template to use to launch instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_LaunchTemplateSpecification.html LaunchTemplateSpecification> in the /Amazon EC2 Auto Scaling API Reference/ . If you do not specify @LaunchTemplate@ , you must specify one of the following parameters: @InstanceId@ , @LaunchConfigurationName@ , or @MixedInstancesPolicy@ .
--
-- * 'casgPlacementGroup' - The name of the placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'casgLoadBalancerNames' - A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers and Network Load Balancers, specify a list of target groups using the @TargetGroupARNs@ property instead. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Using a Load Balancer with an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgTags' - One or more tags. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling Groups and Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'casgAutoScalingGroupName' - The name of the Auto Scaling group. This name must be unique per Region per account.
--
-- * 'casgMinSize' - The minimum size of the group.
--
-- * 'casgMaxSize' - The maximum size of the group.
createAutoScalingGroup
    :: Text -- ^ 'casgAutoScalingGroupName'
    -> Int -- ^ 'casgMinSize'
    -> Int -- ^ 'casgMaxSize'
    -> CreateAutoScalingGroup
createAutoScalingGroup pAutoScalingGroupName_
  pMinSize_ pMaxSize_
  = CreateAutoScalingGroup'{_casgInstanceId = Nothing,
                            _casgTerminationPolicies = Nothing,
                            _casgHealthCheckGracePeriod = Nothing,
                            _casgServiceLinkedRoleARN = Nothing,
                            _casgNewInstancesProtectedFromScaleIn = Nothing,
                            _casgVPCZoneIdentifier = Nothing,
                            _casgTargetGroupARNs = Nothing,
                            _casgMaxInstanceLifetime = Nothing,
                            _casgDefaultCooldown = Nothing,
                            _casgAvailabilityZones = Nothing,
                            _casgDesiredCapacity = Nothing,
                            _casgMixedInstancesPolicy = Nothing,
                            _casgLaunchConfigurationName = Nothing,
                            _casgLifecycleHookSpecificationList = Nothing,
                            _casgHealthCheckType = Nothing,
                            _casgLaunchTemplate = Nothing,
                            _casgPlacementGroup = Nothing,
                            _casgLoadBalancerNames = Nothing,
                            _casgTags = Nothing,
                            _casgAutoScalingGroupName = pAutoScalingGroupName_,
                            _casgMinSize = pMinSize_, _casgMaxSize = pMaxSize_}

-- | The ID of the instance used to create a launch configuration for the group. When you specify an ID of an instance, Amazon EC2 Auto Scaling creates a new launch configuration and associates it with the group. This launch configuration derives its attributes from the specified instance, except for the block device mapping. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-from-instance.html Create an Auto Scaling Group Using an EC2 Instance> in the /Amazon EC2 Auto Scaling User Guide/ . You must specify one of the following parameters in your request: @LaunchConfigurationName@ , @LaunchTemplate@ , @InstanceId@ , or @MixedInstancesPolicy@ .
casgInstanceId :: Lens' CreateAutoScalingGroup (Maybe Text)
casgInstanceId = lens _casgInstanceId (\ s a -> s{_casgInstanceId = a})

-- | One or more termination policies used to select the instance to terminate. These policies are executed in the order that they are listed. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Amazon EC2 Auto Scaling User Guide/ .
casgTerminationPolicies :: Lens' CreateAutoScalingGroup [Text]
casgTerminationPolicies = lens _casgTerminationPolicies (\ s a -> s{_casgTerminationPolicies = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. During this time, any health check failures for the instance are ignored. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health Check Grace Period> in the /Amazon EC2 Auto Scaling User Guide/ . Conditional: This parameter is required if you are adding an @ELB@ health check.
casgHealthCheckGracePeriod :: Lens' CreateAutoScalingGroup (Maybe Int)
casgHealthCheckGracePeriod = lens _casgHealthCheckGracePeriod (\ s a -> s{_casgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. By default, Amazon EC2 Auto Scaling uses a service-linked role named AWSServiceRoleForAutoScaling, which it creates if it does not exist. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-Linked Roles> in the /Amazon EC2 Auto Scaling User Guide/ .
casgServiceLinkedRoleARN :: Lens' CreateAutoScalingGroup (Maybe Text)
casgServiceLinkedRoleARN = lens _casgServiceLinkedRoleARN (\ s a -> s{_casgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /Amazon EC2 Auto Scaling User Guide/ .
casgNewInstancesProtectedFromScaleIn :: Lens' CreateAutoScalingGroup (Maybe Bool)
casgNewInstancesProtectedFromScaleIn = lens _casgNewInstancesProtectedFromScaleIn (\ s a -> s{_casgNewInstancesProtectedFromScaleIn = a})

-- | A comma-separated list of subnet IDs for your virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones. Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into a VPC.
casgVPCZoneIdentifier :: Lens' CreateAutoScalingGroup (Maybe Text)
casgVPCZoneIdentifier = lens _casgVPCZoneIdentifier (\ s a -> s{_casgVPCZoneIdentifier = a})

-- | The Amazon Resource Names (ARN) of the target groups to associate with the Auto Scaling group. Instances are registered as targets in a target group, and traffic is routed to the target group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Using a Load Balancer with an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ .
casgTargetGroupARNs :: Lens' CreateAutoScalingGroup [Text]
casgTargetGroupARNs = lens _casgTargetGroupARNs (\ s a -> s{_casgTargetGroupARNs = a}) . _Default . _Coerce

-- | The maximum amount of time, in seconds, that an instance can be in service. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling Instances Based on Maximum Instance Lifetime> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 604800.
casgMaxInstanceLifetime :: Lens' CreateAutoScalingGroup (Maybe Int)
casgMaxInstanceLifetime = lens _casgMaxInstanceLifetime (\ s a -> s{_casgMaxInstanceLifetime = a})

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling Cooldowns> in the /Amazon EC2 Auto Scaling User Guide/ .
casgDefaultCooldown :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDefaultCooldown = lens _casgDefaultCooldown (\ s a -> s{_casgDefaultCooldown = a})

-- | One or more Availability Zones for the group. This parameter is optional if you specify one or more subnets for @VPCZoneIdentifier@ . Conditional: If your account supports EC2-Classic and VPC, this parameter is required to launch instances into EC2-Classic.
casgAvailabilityZones :: Lens' CreateAutoScalingGroup (Maybe (NonEmpty Text))
casgAvailabilityZones = lens _casgAvailabilityZones (\ s a -> s{_casgAvailabilityZones = a}) . mapping _List1

-- | The number of Amazon EC2 instances that the Auto Scaling group attempts to maintain. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group. If you do not specify a desired capacity, the default is the minimum size of the group.
casgDesiredCapacity :: Lens' CreateAutoScalingGroup (Maybe Int)
casgDesiredCapacity = lens _casgDesiredCapacity (\ s a -> s{_casgDesiredCapacity = a})

-- | An embedded object that specifies a mixed instances policy. The required parameters must be specified. If optional parameters are unspecified, their default values are used. The policy includes parameters that not only define the distribution of On-Demand Instances and Spot Instances, the maximum price to pay for Spot Instances, and how the Auto Scaling group allocates instance types to fulfill On-Demand and Spot capacity, but also the parameters that specify the instance configuration information—the launch template and instance types. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_MixedInstancesPolicy.html MixedInstancesPolicy> in the /Amazon EC2 Auto Scaling API Reference/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling Groups with Multiple Instance Types and Purchase Options> in the /Amazon EC2 Auto Scaling User Guide/ . You must specify one of the following parameters in your request: @LaunchConfigurationName@ , @LaunchTemplate@ , @InstanceId@ , or @MixedInstancesPolicy@ .
casgMixedInstancesPolicy :: Lens' CreateAutoScalingGroup (Maybe MixedInstancesPolicy)
casgMixedInstancesPolicy = lens _casgMixedInstancesPolicy (\ s a -> s{_casgMixedInstancesPolicy = a})

-- | The name of the launch configuration. If you do not specify @LaunchConfigurationName@ , you must specify one of the following parameters: @InstanceId@ , @LaunchTemplate@ , or @MixedInstancesPolicy@ .
casgLaunchConfigurationName :: Lens' CreateAutoScalingGroup (Maybe Text)
casgLaunchConfigurationName = lens _casgLaunchConfigurationName (\ s a -> s{_casgLaunchConfigurationName = a})

-- | One or more lifecycle hooks.
casgLifecycleHookSpecificationList :: Lens' CreateAutoScalingGroup [LifecycleHookSpecification]
casgLifecycleHookSpecificationList = lens _casgLifecycleHookSpecificationList (\ s a -> s{_casgLifecycleHookSpecificationList = a}) . _Default . _Coerce

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . The default value is @EC2@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health Checks for Auto Scaling Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
casgHealthCheckType :: Lens' CreateAutoScalingGroup (Maybe Text)
casgHealthCheckType = lens _casgHealthCheckType (\ s a -> s{_casgHealthCheckType = a})

-- | The launch template to use to launch instances. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_LaunchTemplateSpecification.html LaunchTemplateSpecification> in the /Amazon EC2 Auto Scaling API Reference/ . If you do not specify @LaunchTemplate@ , you must specify one of the following parameters: @InstanceId@ , @LaunchConfigurationName@ , or @MixedInstancesPolicy@ .
casgLaunchTemplate :: Lens' CreateAutoScalingGroup (Maybe LaunchTemplateSpecification)
casgLaunchTemplate = lens _casgLaunchTemplate (\ s a -> s{_casgLaunchTemplate = a})

-- | The name of the placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
casgPlacementGroup :: Lens' CreateAutoScalingGroup (Maybe Text)
casgPlacementGroup = lens _casgPlacementGroup (\ s a -> s{_casgPlacementGroup = a})

-- | A list of Classic Load Balancers associated with this Auto Scaling group. For Application Load Balancers and Network Load Balancers, specify a list of target groups using the @TargetGroupARNs@ property instead. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Using a Load Balancer with an Auto Scaling Group> in the /Amazon EC2 Auto Scaling User Guide/ .
casgLoadBalancerNames :: Lens' CreateAutoScalingGroup [Text]
casgLoadBalancerNames = lens _casgLoadBalancerNames (\ s a -> s{_casgLoadBalancerNames = a}) . _Default . _Coerce

-- | One or more tags. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling Groups and Instances> in the /Amazon EC2 Auto Scaling User Guide/ .
casgTags :: Lens' CreateAutoScalingGroup [Tag]
casgTags = lens _casgTags (\ s a -> s{_casgTags = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group. This name must be unique per Region per account.
casgAutoScalingGroupName :: Lens' CreateAutoScalingGroup Text
casgAutoScalingGroupName = lens _casgAutoScalingGroupName (\ s a -> s{_casgAutoScalingGroupName = a})

-- | The minimum size of the group.
casgMinSize :: Lens' CreateAutoScalingGroup Int
casgMinSize = lens _casgMinSize (\ s a -> s{_casgMinSize = a})

-- | The maximum size of the group.
casgMaxSize :: Lens' CreateAutoScalingGroup Int
casgMaxSize = lens _casgMaxSize (\ s a -> s{_casgMaxSize = a})

instance AWSRequest CreateAutoScalingGroup where
        type Rs CreateAutoScalingGroup =
             CreateAutoScalingGroupResponse
        request = postQuery autoScaling
        response
          = receiveNull CreateAutoScalingGroupResponse'

instance Hashable CreateAutoScalingGroup where

instance NFData CreateAutoScalingGroup where

instance ToHeaders CreateAutoScalingGroup where
        toHeaders = const mempty

instance ToPath CreateAutoScalingGroup where
        toPath = const "/"

instance ToQuery CreateAutoScalingGroup where
        toQuery CreateAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateAutoScalingGroup" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceId" =: _casgInstanceId,
               "TerminationPolicies" =:
                 toQuery
                   (toQueryList "member" <$> _casgTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _casgHealthCheckGracePeriod,
               "ServiceLinkedRoleARN" =: _casgServiceLinkedRoleARN,
               "NewInstancesProtectedFromScaleIn" =:
                 _casgNewInstancesProtectedFromScaleIn,
               "VPCZoneIdentifier" =: _casgVPCZoneIdentifier,
               "TargetGroupARNs" =:
                 toQuery
                   (toQueryList "member" <$> _casgTargetGroupARNs),
               "MaxInstanceLifetime" =: _casgMaxInstanceLifetime,
               "DefaultCooldown" =: _casgDefaultCooldown,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _casgAvailabilityZones),
               "DesiredCapacity" =: _casgDesiredCapacity,
               "MixedInstancesPolicy" =: _casgMixedInstancesPolicy,
               "LaunchConfigurationName" =:
                 _casgLaunchConfigurationName,
               "LifecycleHookSpecificationList" =:
                 toQuery
                   (toQueryList "member" <$>
                      _casgLifecycleHookSpecificationList),
               "HealthCheckType" =: _casgHealthCheckType,
               "LaunchTemplate" =: _casgLaunchTemplate,
               "PlacementGroup" =: _casgPlacementGroup,
               "LoadBalancerNames" =:
                 toQuery
                   (toQueryList "member" <$> _casgLoadBalancerNames),
               "Tags" =:
                 toQuery (toQueryList "member" <$> _casgTags),
               "AutoScalingGroupName" =: _casgAutoScalingGroupName,
               "MinSize" =: _casgMinSize, "MaxSize" =: _casgMaxSize]

-- | /See:/ 'createAutoScalingGroupResponse' smart constructor.
data CreateAutoScalingGroupResponse = CreateAutoScalingGroupResponse'
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'CreateAutoScalingGroupResponse' with the minimum fields required to make a request.
--
createAutoScalingGroupResponse
    :: CreateAutoScalingGroupResponse
createAutoScalingGroupResponse
  = CreateAutoScalingGroupResponse'

instance NFData CreateAutoScalingGroupResponse where
