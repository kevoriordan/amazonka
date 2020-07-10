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
-- Module      : Network.AWS.AutoScaling.UpdateAutoScalingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for the specified Auto Scaling group.
--
--
-- To update an Auto Scaling group, specify the name of the group and the parameter that you want to change. Any parameters that you don't specify are not changed by this update request. The new settings take effect on any scaling activities after this call returns. 
--
-- If you associate a new launch configuration or template with an Auto Scaling group, all new instances will get the updated configuration. Existing instances continue to run with the configuration that they were originally launched with. When you update a group to specify a mixed instances policy instead of a launch configuration or template, existing instances may be replaced to match the new purchasing options that you specified in the policy. For example, if the group currently has 100% On-Demand capacity and the policy specifies 50% Spot capacity, this means that half of your instances will be gradually terminated and relaunched as Spot Instances. When replacing instances, Amazon EC2 Auto Scaling launches new instances before terminating the old ones, so that updating your group does not compromise the performance or availability of your application.
--
-- Note the following about changing @DesiredCapacity@ , @MaxSize@ , or @MinSize@ :
--
--     * If a scale-in event occurs as a result of a new @DesiredCapacity@ value that is lower than the current size of the group, the Auto Scaling group uses its termination policy to determine which instances to terminate.
--
--     * If you specify a new value for @MinSize@ without specifying a value for @DesiredCapacity@ , and the new @MinSize@ is larger than the current size of the group, this sets the group's @DesiredCapacity@ to the new @MinSize@ value.
--
--     * If you specify a new value for @MaxSize@ without specifying a value for @DesiredCapacity@ , and the new @MaxSize@ is smaller than the current size of the group, this sets the group's @DesiredCapacity@ to the new @MaxSize@ value.
--
--
--
-- To see which parameters have been set, use 'DescribeAutoScalingGroups' . You can also view the scaling policies for an Auto Scaling group using 'DescribePolicies' . If the group has scaling policies, you can update them using 'PutScalingPolicy' .
--
module Network.AWS.AutoScaling.UpdateAutoScalingGroup
    (
    -- * Creating a Request
      updateAutoScalingGroup
    , UpdateAutoScalingGroup
    -- * Request Lenses
    , uasgTerminationPolicies
    , uasgHealthCheckGracePeriod
    , uasgServiceLinkedRoleARN
    , uasgNewInstancesProtectedFromScaleIn
    , uasgVPCZoneIdentifier
    , uasgMaxInstanceLifetime
    , uasgDefaultCooldown
    , uasgMaxSize
    , uasgAvailabilityZones
    , uasgDesiredCapacity
    , uasgMixedInstancesPolicy
    , uasgMinSize
    , uasgLaunchConfigurationName
    , uasgHealthCheckType
    , uasgLaunchTemplate
    , uasgPlacementGroup
    , uasgAutoScalingGroupName

    -- * Destructuring the Response
    , updateAutoScalingGroupResponse
    , UpdateAutoScalingGroupResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAutoScalingGroup' smart constructor.
data UpdateAutoScalingGroup = UpdateAutoScalingGroup'{_uasgTerminationPolicies
                                                      :: !(Maybe [Text]),
                                                      _uasgHealthCheckGracePeriod
                                                      :: !(Maybe Int),
                                                      _uasgServiceLinkedRoleARN
                                                      :: !(Maybe Text),
                                                      _uasgNewInstancesProtectedFromScaleIn
                                                      :: !(Maybe Bool),
                                                      _uasgVPCZoneIdentifier ::
                                                      !(Maybe Text),
                                                      _uasgMaxInstanceLifetime
                                                      :: !(Maybe Int),
                                                      _uasgDefaultCooldown ::
                                                      !(Maybe Int),
                                                      _uasgMaxSize ::
                                                      !(Maybe Int),
                                                      _uasgAvailabilityZones ::
                                                      !(Maybe (List1 Text)),
                                                      _uasgDesiredCapacity ::
                                                      !(Maybe Int),
                                                      _uasgMixedInstancesPolicy
                                                      ::
                                                      !(Maybe
                                                          MixedInstancesPolicy),
                                                      _uasgMinSize ::
                                                      !(Maybe Int),
                                                      _uasgLaunchConfigurationName
                                                      :: !(Maybe Text),
                                                      _uasgHealthCheckType ::
                                                      !(Maybe Text),
                                                      _uasgLaunchTemplate ::
                                                      !(Maybe
                                                          LaunchTemplateSpecification),
                                                      _uasgPlacementGroup ::
                                                      !(Maybe Text),
                                                      _uasgAutoScalingGroupName
                                                      :: !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'UpdateAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasgTerminationPolicies' - A standalone termination policy or a list of termination policies used to select the instance to terminate. The policies are executed in the order that they are listed. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'uasgHealthCheckGracePeriod' - The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health Check Grace Period> in the /Amazon EC2 Auto Scaling User Guide/ . Conditional: This parameter is required if you are adding an @ELB@ health check.
--
-- * 'uasgServiceLinkedRoleARN' - The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-Linked Roles> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'uasgNewInstancesProtectedFromScaleIn' - Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'uasgVPCZoneIdentifier' - A comma-separated list of subnet IDs for virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
--
-- * 'uasgMaxInstanceLifetime' - The maximum amount of time, in seconds, that an instance can be in service. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling Instances Based on Maximum Instance Lifetime> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 604800.
--
-- * 'uasgDefaultCooldown' - The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This cooldown period is not used when a scaling-specific cooldown is specified. Cooldown periods are not supported for target tracking scaling policies, step scaling policies, or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling Cooldowns> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'uasgMaxSize' - The maximum size of the Auto Scaling group.
--
-- * 'uasgAvailabilityZones' - One or more Availability Zones for the group.
--
-- * 'uasgDesiredCapacity' - The number of EC2 instances that should be running in the Auto Scaling group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
--
-- * 'uasgMixedInstancesPolicy' - An embedded object that specifies a mixed instances policy. In your call to @UpdateAutoScalingGroup@ , you can make changes to the policy that is specified. All optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_MixedInstancesPolicy.html MixedInstancesPolicy> in the /Amazon EC2 Auto Scaling API Reference/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling Groups with Multiple Instance Types and Purchase Options> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- * 'uasgMinSize' - The minimum size of the Auto Scaling group.
--
-- * 'uasgLaunchConfigurationName' - The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
--
-- * 'uasgHealthCheckType' - The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
--
-- * 'uasgLaunchTemplate' - The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_LaunchTemplateSpecification.html LaunchTemplateSpecification> in the /Amazon EC2 Auto Scaling API Reference/ .
--
-- * 'uasgPlacementGroup' - The name of the placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'uasgAutoScalingGroupName' - The name of the Auto Scaling group.
updateAutoScalingGroup
    :: Text -- ^ 'uasgAutoScalingGroupName'
    -> UpdateAutoScalingGroup
updateAutoScalingGroup pAutoScalingGroupName_
  = UpdateAutoScalingGroup'{_uasgTerminationPolicies =
                              Nothing,
                            _uasgHealthCheckGracePeriod = Nothing,
                            _uasgServiceLinkedRoleARN = Nothing,
                            _uasgNewInstancesProtectedFromScaleIn = Nothing,
                            _uasgVPCZoneIdentifier = Nothing,
                            _uasgMaxInstanceLifetime = Nothing,
                            _uasgDefaultCooldown = Nothing,
                            _uasgMaxSize = Nothing,
                            _uasgAvailabilityZones = Nothing,
                            _uasgDesiredCapacity = Nothing,
                            _uasgMixedInstancesPolicy = Nothing,
                            _uasgMinSize = Nothing,
                            _uasgLaunchConfigurationName = Nothing,
                            _uasgHealthCheckType = Nothing,
                            _uasgLaunchTemplate = Nothing,
                            _uasgPlacementGroup = Nothing,
                            _uasgAutoScalingGroupName = pAutoScalingGroupName_}

-- | A standalone termination policy or a list of termination policies used to select the instance to terminate. The policies are executed in the order that they are listed. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling Which Instances Auto Scaling Terminates During Scale In> in the /Amazon EC2 Auto Scaling User Guide/ .
uasgTerminationPolicies :: Lens' UpdateAutoScalingGroup [Text]
uasgTerminationPolicies = lens _uasgTerminationPolicies (\ s a -> s{_uasgTerminationPolicies = a}) . _Default . _Coerce

-- | The amount of time, in seconds, that Amazon EC2 Auto Scaling waits before checking the health status of an EC2 instance that has come into service. The default value is @0@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html#health-check-grace-period Health Check Grace Period> in the /Amazon EC2 Auto Scaling User Guide/ . Conditional: This parameter is required if you are adding an @ELB@ health check.
uasgHealthCheckGracePeriod :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgHealthCheckGracePeriod = lens _uasgHealthCheckGracePeriod (\ s a -> s{_uasgHealthCheckGracePeriod = a})

-- | The Amazon Resource Name (ARN) of the service-linked role that the Auto Scaling group uses to call other AWS services on your behalf. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-service-linked-role.html Service-Linked Roles> in the /Amazon EC2 Auto Scaling User Guide/ .
uasgServiceLinkedRoleARN :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgServiceLinkedRoleARN = lens _uasgServiceLinkedRoleARN (\ s a -> s{_uasgServiceLinkedRoleARN = a})

-- | Indicates whether newly launched instances are protected from termination by Amazon EC2 Auto Scaling when scaling in. For more information about preventing instances from terminating on scale in, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /Amazon EC2 Auto Scaling User Guide/ .
uasgNewInstancesProtectedFromScaleIn :: Lens' UpdateAutoScalingGroup (Maybe Bool)
uasgNewInstancesProtectedFromScaleIn = lens _uasgNewInstancesProtectedFromScaleIn (\ s a -> s{_uasgNewInstancesProtectedFromScaleIn = a})

-- | A comma-separated list of subnet IDs for virtual private cloud (VPC). If you specify @VPCZoneIdentifier@ with @AvailabilityZones@ , the subnets that you specify for this parameter must reside in those Availability Zones.
uasgVPCZoneIdentifier :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgVPCZoneIdentifier = lens _uasgVPCZoneIdentifier (\ s a -> s{_uasgVPCZoneIdentifier = a})

-- | The maximum amount of time, in seconds, that an instance can be in service. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-max-instance-lifetime.html Replacing Auto Scaling Instances Based on Maximum Instance Lifetime> in the /Amazon EC2 Auto Scaling User Guide/ . Valid Range: Minimum value of 604800.
uasgMaxInstanceLifetime :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMaxInstanceLifetime = lens _uasgMaxInstanceLifetime (\ s a -> s{_uasgMaxInstanceLifetime = a})

-- | The amount of time, in seconds, after a scaling activity completes before another scaling activity can start. The default value is @300@ . This cooldown period is not used when a scaling-specific cooldown is specified. Cooldown periods are not supported for target tracking scaling policies, step scaling policies, or scheduled scaling. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling Cooldowns> in the /Amazon EC2 Auto Scaling User Guide/ .
uasgDefaultCooldown :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDefaultCooldown = lens _uasgDefaultCooldown (\ s a -> s{_uasgDefaultCooldown = a})

-- | The maximum size of the Auto Scaling group.
uasgMaxSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMaxSize = lens _uasgMaxSize (\ s a -> s{_uasgMaxSize = a})

-- | One or more Availability Zones for the group.
uasgAvailabilityZones :: Lens' UpdateAutoScalingGroup (Maybe (NonEmpty Text))
uasgAvailabilityZones = lens _uasgAvailabilityZones (\ s a -> s{_uasgAvailabilityZones = a}) . mapping _List1

-- | The number of EC2 instances that should be running in the Auto Scaling group. This number must be greater than or equal to the minimum size of the group and less than or equal to the maximum size of the group.
uasgDesiredCapacity :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgDesiredCapacity = lens _uasgDesiredCapacity (\ s a -> s{_uasgDesiredCapacity = a})

-- | An embedded object that specifies a mixed instances policy. In your call to @UpdateAutoScalingGroup@ , you can make changes to the policy that is specified. All optional parameters are left unchanged if not specified. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_MixedInstancesPolicy.html MixedInstancesPolicy> in the /Amazon EC2 Auto Scaling API Reference/ and <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-purchase-options.html Auto Scaling Groups with Multiple Instance Types and Purchase Options> in the /Amazon EC2 Auto Scaling User Guide/ .
uasgMixedInstancesPolicy :: Lens' UpdateAutoScalingGroup (Maybe MixedInstancesPolicy)
uasgMixedInstancesPolicy = lens _uasgMixedInstancesPolicy (\ s a -> s{_uasgMixedInstancesPolicy = a})

-- | The minimum size of the Auto Scaling group.
uasgMinSize :: Lens' UpdateAutoScalingGroup (Maybe Int)
uasgMinSize = lens _uasgMinSize (\ s a -> s{_uasgMinSize = a})

-- | The name of the launch configuration. If you specify @LaunchConfigurationName@ in your update request, you can't specify @LaunchTemplate@ or @MixedInstancesPolicy@ .
uasgLaunchConfigurationName :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgLaunchConfigurationName = lens _uasgLaunchConfigurationName (\ s a -> s{_uasgLaunchConfigurationName = a})

-- | The service to use for the health checks. The valid values are @EC2@ and @ELB@ . If you configure an Auto Scaling group to use ELB health checks, it considers the instance unhealthy if it fails either the EC2 status checks or the load balancer health checks.
uasgHealthCheckType :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgHealthCheckType = lens _uasgHealthCheckType (\ s a -> s{_uasgHealthCheckType = a})

-- | The launch template and version to use to specify the updates. If you specify @LaunchTemplate@ in your update request, you can't specify @LaunchConfigurationName@ or @MixedInstancesPolicy@ . For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_LaunchTemplateSpecification.html LaunchTemplateSpecification> in the /Amazon EC2 Auto Scaling API Reference/ .
uasgLaunchTemplate :: Lens' UpdateAutoScalingGroup (Maybe LaunchTemplateSpecification)
uasgLaunchTemplate = lens _uasgLaunchTemplate (\ s a -> s{_uasgLaunchTemplate = a})

-- | The name of the placement group into which to launch your instances, if any. A placement group is a logical grouping of instances within a single Availability Zone. You cannot specify multiple Availability Zones and a placement group. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon EC2 User Guide for Linux Instances/ .
uasgPlacementGroup :: Lens' UpdateAutoScalingGroup (Maybe Text)
uasgPlacementGroup = lens _uasgPlacementGroup (\ s a -> s{_uasgPlacementGroup = a})

-- | The name of the Auto Scaling group.
uasgAutoScalingGroupName :: Lens' UpdateAutoScalingGroup Text
uasgAutoScalingGroupName = lens _uasgAutoScalingGroupName (\ s a -> s{_uasgAutoScalingGroupName = a})

instance AWSRequest UpdateAutoScalingGroup where
        type Rs UpdateAutoScalingGroup =
             UpdateAutoScalingGroupResponse
        request = postQuery autoScaling
        response
          = receiveNull UpdateAutoScalingGroupResponse'

instance Hashable UpdateAutoScalingGroup where

instance NFData UpdateAutoScalingGroup where

instance ToHeaders UpdateAutoScalingGroup where
        toHeaders = const mempty

instance ToPath UpdateAutoScalingGroup where
        toPath = const "/"

instance ToQuery UpdateAutoScalingGroup where
        toQuery UpdateAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAutoScalingGroup" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "TerminationPolicies" =:
                 toQuery
                   (toQueryList "member" <$> _uasgTerminationPolicies),
               "HealthCheckGracePeriod" =:
                 _uasgHealthCheckGracePeriod,
               "ServiceLinkedRoleARN" =: _uasgServiceLinkedRoleARN,
               "NewInstancesProtectedFromScaleIn" =:
                 _uasgNewInstancesProtectedFromScaleIn,
               "VPCZoneIdentifier" =: _uasgVPCZoneIdentifier,
               "MaxInstanceLifetime" =: _uasgMaxInstanceLifetime,
               "DefaultCooldown" =: _uasgDefaultCooldown,
               "MaxSize" =: _uasgMaxSize,
               "AvailabilityZones" =:
                 toQuery
                   (toQueryList "member" <$> _uasgAvailabilityZones),
               "DesiredCapacity" =: _uasgDesiredCapacity,
               "MixedInstancesPolicy" =: _uasgMixedInstancesPolicy,
               "MinSize" =: _uasgMinSize,
               "LaunchConfigurationName" =:
                 _uasgLaunchConfigurationName,
               "HealthCheckType" =: _uasgHealthCheckType,
               "LaunchTemplate" =: _uasgLaunchTemplate,
               "PlacementGroup" =: _uasgPlacementGroup,
               "AutoScalingGroupName" =: _uasgAutoScalingGroupName]

-- | /See:/ 'updateAutoScalingGroupResponse' smart constructor.
data UpdateAutoScalingGroupResponse = UpdateAutoScalingGroupResponse'
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'UpdateAutoScalingGroupResponse' with the minimum fields required to make a request.
--
updateAutoScalingGroupResponse
    :: UpdateAutoScalingGroupResponse
updateAutoScalingGroupResponse
  = UpdateAutoScalingGroupResponse'

instance NFData UpdateAutoScalingGroupResponse where
