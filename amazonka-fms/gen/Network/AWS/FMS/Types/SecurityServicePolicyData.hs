{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.SecurityServicePolicyData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.SecurityServicePolicyData where

import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the security service that is being used to protect the resources.
--
--
--
-- /See:/ 'securityServicePolicyData' smart constructor.
data SecurityServicePolicyData = SecurityServicePolicyData'{_sspdManagedServiceData
                                                            :: !(Maybe Text),
                                                            _sspdType ::
                                                            !SecurityServiceType}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'SecurityServicePolicyData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspdManagedServiceData' - Details about the service that are specific to the service type, in JSON format. For service type @SHIELD_ADVANCED@ , this is an empty string.     * Example: @WAFV2@  @"ManagedServiceData": "{\"type\":\"WAFV2\",\"defaultAction\":{\"type\":\"ALLOW\"},\"preProcessRuleGroups\":[{\"managedRuleGroupIdentifier\":null,\"ruleGroupArn\":\"rulegrouparn\",\"overrideAction\":{\"type\":\"COUNT\"},\"excludedRules\":[{\"name\":\"EntityName\"}],\"ruleGroupType\":\"RuleGroup\"}],\"postProcessRuleGroups\":[{\"managedRuleGroupIdentifier\":{\"managedRuleGroupName\":\"AWSManagedRulesAdminProtectionRuleSet\",\"vendor\":\"AWS\"},\"ruleGroupArn\":\"rulegrouparn\",\"overrideAction\":{\"type\":\"NONE\"},\"excludedRules\":[],\"ruleGroupType\":\"ManagedRuleGroup\"}],\"overrideCustomerWebACLAssociation\":false}"@      * Example: @WAF Classic@  @"ManagedServiceData": "{\"type\": \"WAF\", \"ruleGroups\": [{\"id\": \"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}@      * Example: @SECURITY_GROUPS_COMMON@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_COMMON","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_COMMON\",\"revertManualSecurityGroupChanges\":false,\"exclusiveResourceSecurityGroupManagement\":false, \"applyToAllEC2InstanceENIs\":false,\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd\"}]}"},"RemediationEnabled":false,"ResourceType":"AWS::EC2::NetworkInterface"}@      * Example: @SECURITY_GROUPS_CONTENT_AUDIT@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_CONTENT_AUDIT","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_CONTENT_AUDIT\",\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd \"}],\"securityGroupAction\":{\"type\":\"ALLOW\"}}"},"RemediationEnabled":false,"ResourceType":"AWS::EC2::NetworkInterface"}@  The security group action for content audit can be @ALLOW@ or @DENY@ . For @ALLOW@ , all in-scope security group rules must be within the allowed range of the policy's security group rules. For @DENY@ , all in-scope security group rules must not contain a value or a range that matches a rule value or range in the policy security group.     * Example: @SECURITY_GROUPS_USAGE_AUDIT@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_USAGE_AUDIT","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_USAGE_AUDIT\",\"deleteUnusedSecurityGroups\":true,\"coalesceRedundantSecurityGroups\":true}"},"RemediationEnabled":false,"Resou rceType":"AWS::EC2::SecurityGroup"}@ 
--
-- * 'sspdType' - The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
securityServicePolicyData
    :: SecurityServiceType -- ^ 'sspdType'
    -> SecurityServicePolicyData
securityServicePolicyData pType_
  = SecurityServicePolicyData'{_sspdManagedServiceData
                                 = Nothing,
                               _sspdType = pType_}

-- | Details about the service that are specific to the service type, in JSON format. For service type @SHIELD_ADVANCED@ , this is an empty string.     * Example: @WAFV2@  @"ManagedServiceData": "{\"type\":\"WAFV2\",\"defaultAction\":{\"type\":\"ALLOW\"},\"preProcessRuleGroups\":[{\"managedRuleGroupIdentifier\":null,\"ruleGroupArn\":\"rulegrouparn\",\"overrideAction\":{\"type\":\"COUNT\"},\"excludedRules\":[{\"name\":\"EntityName\"}],\"ruleGroupType\":\"RuleGroup\"}],\"postProcessRuleGroups\":[{\"managedRuleGroupIdentifier\":{\"managedRuleGroupName\":\"AWSManagedRulesAdminProtectionRuleSet\",\"vendor\":\"AWS\"},\"ruleGroupArn\":\"rulegrouparn\",\"overrideAction\":{\"type\":\"NONE\"},\"excludedRules\":[],\"ruleGroupType\":\"ManagedRuleGroup\"}],\"overrideCustomerWebACLAssociation\":false}"@      * Example: @WAF Classic@  @"ManagedServiceData": "{\"type\": \"WAF\", \"ruleGroups\": [{\"id\": \"12345678-1bcd-9012-efga-0987654321ab\", \"overrideAction\" : {\"type\": \"COUNT\"}}], \"defaultAction\": {\"type\": \"BLOCK\"}}@      * Example: @SECURITY_GROUPS_COMMON@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_COMMON","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_COMMON\",\"revertManualSecurityGroupChanges\":false,\"exclusiveResourceSecurityGroupManagement\":false, \"applyToAllEC2InstanceENIs\":false,\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd\"}]}"},"RemediationEnabled":false,"ResourceType":"AWS::EC2::NetworkInterface"}@      * Example: @SECURITY_GROUPS_CONTENT_AUDIT@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_CONTENT_AUDIT","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_CONTENT_AUDIT\",\"securityGroups\":[{\"id\":\" sg-000e55995d61a06bd \"}],\"securityGroupAction\":{\"type\":\"ALLOW\"}}"},"RemediationEnabled":false,"ResourceType":"AWS::EC2::NetworkInterface"}@  The security group action for content audit can be @ALLOW@ or @DENY@ . For @ALLOW@ , all in-scope security group rules must be within the allowed range of the policy's security group rules. For @DENY@ , all in-scope security group rules must not contain a value or a range that matches a rule value or range in the policy security group.     * Example: @SECURITY_GROUPS_USAGE_AUDIT@  @"SecurityServicePolicyData":{"Type":"SECURITY_GROUPS_USAGE_AUDIT","ManagedServiceData":"{\"type\":\"SECURITY_GROUPS_USAGE_AUDIT\",\"deleteUnusedSecurityGroups\":true,\"coalesceRedundantSecurityGroups\":true}"},"RemediationEnabled":false,"Resou rceType":"AWS::EC2::SecurityGroup"}@ 
sspdManagedServiceData :: Lens' SecurityServicePolicyData (Maybe Text)
sspdManagedServiceData = lens _sspdManagedServiceData (\ s a -> s{_sspdManagedServiceData = a})

-- | The service that the policy is using to protect the resources. This specifies the type of policy that is created, either an AWS WAF policy, a Shield Advanced policy, or a security group policy. For security group policies, Firewall Manager supports one security group for each common policy and for each content audit policy. This is an adjustable limit that you can increase by contacting AWS Support.
sspdType :: Lens' SecurityServicePolicyData SecurityServiceType
sspdType = lens _sspdType (\ s a -> s{_sspdType = a})

instance FromJSON SecurityServicePolicyData where
        parseJSON
          = withObject "SecurityServicePolicyData"
              (\ x ->
                 SecurityServicePolicyData' <$>
                   (x .:? "ManagedServiceData") <*> (x .: "Type"))

instance Hashable SecurityServicePolicyData where

instance NFData SecurityServicePolicyData where

instance ToJSON SecurityServicePolicyData where
        toJSON SecurityServicePolicyData'{..}
          = object
              (catMaybes
                 [("ManagedServiceData" .=) <$>
                    _sspdManagedServiceData,
                  Just ("Type" .= _sspdType)])
