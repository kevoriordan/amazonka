{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ResourceType (
  ResourceType (
    ..
    , AWSAPIGATEWAYV2API
    , AWSAPIGATEWAYV2Stage
    , AWSAPIGatewayRestAPI
    , AWSAPIGatewayStage
    , AWSAcmCertificate
    , AWSAutoScalingAutoScalingGroup
    , AWSAutoScalingLaunchConfiguration
    , AWSAutoScalingScalingPolicy
    , AWSAutoScalingScheduledAction
    , AWSCloudFormationStack
    , AWSCloudFrontDistribution
    , AWSCloudFrontStreamingDistribution
    , AWSCloudTrailTrail
    , AWSCloudWatchAlarm
    , AWSCodeBuildProject
    , AWSCodePipelinePipeline
    , AWSConfigResourceCompliance
    , AWSDynamoDBTable
    , AWSEC2CustomerGateway
    , AWSEC2EIP
    , AWSEC2EgressOnlyInternetGateway
    , AWSEC2FlowLog
    , AWSEC2Host
    , AWSEC2Instance
    , AWSEC2InternetGateway
    , AWSEC2NatGateway
    , AWSEC2NetworkACL
    , AWSEC2NetworkInterface
    , AWSEC2RegisteredHAInstance
    , AWSEC2RouteTable
    , AWSEC2SecurityGroup
    , AWSEC2Subnet
    , AWSEC2VPC
    , AWSEC2VPCEndpoint
    , AWSEC2VPCEndpointService
    , AWSEC2VPCPeeringConnection
    , AWSEC2VPNConnection
    , AWSEC2VPNGateway
    , AWSEC2Volume
    , AWSELASTICLOADBALANCINGV2LoadBalancer
    , AWSElasticBeanstalkApplication
    , AWSElasticBeanstalkApplicationVersion
    , AWSElasticBeanstalkEnvironment
    , AWSElasticLoadBalancingLoadBalancer
    , AWSElasticsearchDomain
    , AWSIAMGroup
    , AWSIAMPolicy
    , AWSIAMRole
    , AWSIAMUser
    , AWSKMSKey
    , AWSLambdaFunction
    , AWSQldbLedger
    , AWSRDSDBCluster
    , AWSRDSDBClusterSnapshot
    , AWSRDSDBInstance
    , AWSRDSDBSecurityGroup
    , AWSRDSDBSnapshot
    , AWSRDSDBSubnetGroup
    , AWSRDSEventSubscription
    , AWSRedshiftCluster
    , AWSRedshiftClusterParameterGroup
    , AWSRedshiftClusterSecurityGroup
    , AWSRedshiftClusterSnapshot
    , AWSRedshiftClusterSubnetGroup
    , AWSRedshiftEventSubscription
    , AWSS3AccountPublicAccessBlock
    , AWSS3Bucket
    , AWSServiceCatalogCloudFormationProduct
    , AWSServiceCatalogCloudFormationProvisionedProduct
    , AWSServiceCatalogPortfolio
    , AWSShieldProtection
    , AWSShieldRegionalProtection
    , AWSSqsQueue
    , AWSSsmAssociationCompliance
    , AWSSsmManagedInstanceInventory
    , AWSSsmPatchCompliance
    , AWSWAFRegionalRateBasedRule
    , AWSWAFRegionalRule
    , AWSWAFRegionalRuleGroup
    , AWSWAFRegionalWebACL
    , AWSWAFV2IPSet
    , AWSWAFV2ManagedRuleSet
    , AWSWAFV2RegexPatternSet
    , AWSWAFV2RuleGroup
    , AWSWAFV2WebACL
    , AWSWafRateBasedRule
    , AWSWafRule
    , AWSWafRuleGroup
    , AWSWafWebACL
    , AWSXRayEncryptionConfig
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceType = ResourceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AWSAPIGATEWAYV2API :: ResourceType
pattern AWSAPIGATEWAYV2API = ResourceType' "AWS::ApiGatewayV2::Api"

pattern AWSAPIGATEWAYV2Stage :: ResourceType
pattern AWSAPIGATEWAYV2Stage = ResourceType' "AWS::ApiGatewayV2::Stage"

pattern AWSAPIGatewayRestAPI :: ResourceType
pattern AWSAPIGatewayRestAPI = ResourceType' "AWS::ApiGateway::RestApi"

pattern AWSAPIGatewayStage :: ResourceType
pattern AWSAPIGatewayStage = ResourceType' "AWS::ApiGateway::Stage"

pattern AWSAcmCertificate :: ResourceType
pattern AWSAcmCertificate = ResourceType' "AWS::ACM::Certificate"

pattern AWSAutoScalingAutoScalingGroup :: ResourceType
pattern AWSAutoScalingAutoScalingGroup = ResourceType' "AWS::AutoScaling::AutoScalingGroup"

pattern AWSAutoScalingLaunchConfiguration :: ResourceType
pattern AWSAutoScalingLaunchConfiguration = ResourceType' "AWS::AutoScaling::LaunchConfiguration"

pattern AWSAutoScalingScalingPolicy :: ResourceType
pattern AWSAutoScalingScalingPolicy = ResourceType' "AWS::AutoScaling::ScalingPolicy"

pattern AWSAutoScalingScheduledAction :: ResourceType
pattern AWSAutoScalingScheduledAction = ResourceType' "AWS::AutoScaling::ScheduledAction"

pattern AWSCloudFormationStack :: ResourceType
pattern AWSCloudFormationStack = ResourceType' "AWS::CloudFormation::Stack"

pattern AWSCloudFrontDistribution :: ResourceType
pattern AWSCloudFrontDistribution = ResourceType' "AWS::CloudFront::Distribution"

pattern AWSCloudFrontStreamingDistribution :: ResourceType
pattern AWSCloudFrontStreamingDistribution = ResourceType' "AWS::CloudFront::StreamingDistribution"

pattern AWSCloudTrailTrail :: ResourceType
pattern AWSCloudTrailTrail = ResourceType' "AWS::CloudTrail::Trail"

pattern AWSCloudWatchAlarm :: ResourceType
pattern AWSCloudWatchAlarm = ResourceType' "AWS::CloudWatch::Alarm"

pattern AWSCodeBuildProject :: ResourceType
pattern AWSCodeBuildProject = ResourceType' "AWS::CodeBuild::Project"

pattern AWSCodePipelinePipeline :: ResourceType
pattern AWSCodePipelinePipeline = ResourceType' "AWS::CodePipeline::Pipeline"

pattern AWSConfigResourceCompliance :: ResourceType
pattern AWSConfigResourceCompliance = ResourceType' "AWS::Config::ResourceCompliance"

pattern AWSDynamoDBTable :: ResourceType
pattern AWSDynamoDBTable = ResourceType' "AWS::DynamoDB::Table"

pattern AWSEC2CustomerGateway :: ResourceType
pattern AWSEC2CustomerGateway = ResourceType' "AWS::EC2::CustomerGateway"

pattern AWSEC2EIP :: ResourceType
pattern AWSEC2EIP = ResourceType' "AWS::EC2::EIP"

pattern AWSEC2EgressOnlyInternetGateway :: ResourceType
pattern AWSEC2EgressOnlyInternetGateway = ResourceType' "AWS::EC2::EgressOnlyInternetGateway"

pattern AWSEC2FlowLog :: ResourceType
pattern AWSEC2FlowLog = ResourceType' "AWS::EC2::FlowLog"

pattern AWSEC2Host :: ResourceType
pattern AWSEC2Host = ResourceType' "AWS::EC2::Host"

pattern AWSEC2Instance :: ResourceType
pattern AWSEC2Instance = ResourceType' "AWS::EC2::Instance"

pattern AWSEC2InternetGateway :: ResourceType
pattern AWSEC2InternetGateway = ResourceType' "AWS::EC2::InternetGateway"

pattern AWSEC2NatGateway :: ResourceType
pattern AWSEC2NatGateway = ResourceType' "AWS::EC2::NatGateway"

pattern AWSEC2NetworkACL :: ResourceType
pattern AWSEC2NetworkACL = ResourceType' "AWS::EC2::NetworkAcl"

pattern AWSEC2NetworkInterface :: ResourceType
pattern AWSEC2NetworkInterface = ResourceType' "AWS::EC2::NetworkInterface"

pattern AWSEC2RegisteredHAInstance :: ResourceType
pattern AWSEC2RegisteredHAInstance = ResourceType' "AWS::EC2::RegisteredHAInstance"

pattern AWSEC2RouteTable :: ResourceType
pattern AWSEC2RouteTable = ResourceType' "AWS::EC2::RouteTable"

pattern AWSEC2SecurityGroup :: ResourceType
pattern AWSEC2SecurityGroup = ResourceType' "AWS::EC2::SecurityGroup"

pattern AWSEC2Subnet :: ResourceType
pattern AWSEC2Subnet = ResourceType' "AWS::EC2::Subnet"

pattern AWSEC2VPC :: ResourceType
pattern AWSEC2VPC = ResourceType' "AWS::EC2::VPC"

pattern AWSEC2VPCEndpoint :: ResourceType
pattern AWSEC2VPCEndpoint = ResourceType' "AWS::EC2::VPCEndpoint"

pattern AWSEC2VPCEndpointService :: ResourceType
pattern AWSEC2VPCEndpointService = ResourceType' "AWS::EC2::VPCEndpointService"

pattern AWSEC2VPCPeeringConnection :: ResourceType
pattern AWSEC2VPCPeeringConnection = ResourceType' "AWS::EC2::VPCPeeringConnection"

pattern AWSEC2VPNConnection :: ResourceType
pattern AWSEC2VPNConnection = ResourceType' "AWS::EC2::VPNConnection"

pattern AWSEC2VPNGateway :: ResourceType
pattern AWSEC2VPNGateway = ResourceType' "AWS::EC2::VPNGateway"

pattern AWSEC2Volume :: ResourceType
pattern AWSEC2Volume = ResourceType' "AWS::EC2::Volume"

pattern AWSELASTICLOADBALANCINGV2LoadBalancer :: ResourceType
pattern AWSELASTICLOADBALANCINGV2LoadBalancer = ResourceType' "AWS::ElasticLoadBalancingV2::LoadBalancer"

pattern AWSElasticBeanstalkApplication :: ResourceType
pattern AWSElasticBeanstalkApplication = ResourceType' "AWS::ElasticBeanstalk::Application"

pattern AWSElasticBeanstalkApplicationVersion :: ResourceType
pattern AWSElasticBeanstalkApplicationVersion = ResourceType' "AWS::ElasticBeanstalk::ApplicationVersion"

pattern AWSElasticBeanstalkEnvironment :: ResourceType
pattern AWSElasticBeanstalkEnvironment = ResourceType' "AWS::ElasticBeanstalk::Environment"

pattern AWSElasticLoadBalancingLoadBalancer :: ResourceType
pattern AWSElasticLoadBalancingLoadBalancer = ResourceType' "AWS::ElasticLoadBalancing::LoadBalancer"

pattern AWSElasticsearchDomain :: ResourceType
pattern AWSElasticsearchDomain = ResourceType' "AWS::Elasticsearch::Domain"

pattern AWSIAMGroup :: ResourceType
pattern AWSIAMGroup = ResourceType' "AWS::IAM::Group"

pattern AWSIAMPolicy :: ResourceType
pattern AWSIAMPolicy = ResourceType' "AWS::IAM::Policy"

pattern AWSIAMRole :: ResourceType
pattern AWSIAMRole = ResourceType' "AWS::IAM::Role"

pattern AWSIAMUser :: ResourceType
pattern AWSIAMUser = ResourceType' "AWS::IAM::User"

pattern AWSKMSKey :: ResourceType
pattern AWSKMSKey = ResourceType' "AWS::KMS::Key"

pattern AWSLambdaFunction :: ResourceType
pattern AWSLambdaFunction = ResourceType' "AWS::Lambda::Function"

pattern AWSQldbLedger :: ResourceType
pattern AWSQldbLedger = ResourceType' "AWS::QLDB::Ledger"

pattern AWSRDSDBCluster :: ResourceType
pattern AWSRDSDBCluster = ResourceType' "AWS::RDS::DBCluster"

pattern AWSRDSDBClusterSnapshot :: ResourceType
pattern AWSRDSDBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern AWSRDSDBInstance :: ResourceType
pattern AWSRDSDBInstance = ResourceType' "AWS::RDS::DBInstance"

pattern AWSRDSDBSecurityGroup :: ResourceType
pattern AWSRDSDBSecurityGroup = ResourceType' "AWS::RDS::DBSecurityGroup"

pattern AWSRDSDBSnapshot :: ResourceType
pattern AWSRDSDBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern AWSRDSDBSubnetGroup :: ResourceType
pattern AWSRDSDBSubnetGroup = ResourceType' "AWS::RDS::DBSubnetGroup"

pattern AWSRDSEventSubscription :: ResourceType
pattern AWSRDSEventSubscription = ResourceType' "AWS::RDS::EventSubscription"

pattern AWSRedshiftCluster :: ResourceType
pattern AWSRedshiftCluster = ResourceType' "AWS::Redshift::Cluster"

pattern AWSRedshiftClusterParameterGroup :: ResourceType
pattern AWSRedshiftClusterParameterGroup = ResourceType' "AWS::Redshift::ClusterParameterGroup"

pattern AWSRedshiftClusterSecurityGroup :: ResourceType
pattern AWSRedshiftClusterSecurityGroup = ResourceType' "AWS::Redshift::ClusterSecurityGroup"

pattern AWSRedshiftClusterSnapshot :: ResourceType
pattern AWSRedshiftClusterSnapshot = ResourceType' "AWS::Redshift::ClusterSnapshot"

pattern AWSRedshiftClusterSubnetGroup :: ResourceType
pattern AWSRedshiftClusterSubnetGroup = ResourceType' "AWS::Redshift::ClusterSubnetGroup"

pattern AWSRedshiftEventSubscription :: ResourceType
pattern AWSRedshiftEventSubscription = ResourceType' "AWS::Redshift::EventSubscription"

pattern AWSS3AccountPublicAccessBlock :: ResourceType
pattern AWSS3AccountPublicAccessBlock = ResourceType' "AWS::S3::AccountPublicAccessBlock"

pattern AWSS3Bucket :: ResourceType
pattern AWSS3Bucket = ResourceType' "AWS::S3::Bucket"

pattern AWSServiceCatalogCloudFormationProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProduct"

pattern AWSServiceCatalogCloudFormationProvisionedProduct :: ResourceType
pattern AWSServiceCatalogCloudFormationProvisionedProduct = ResourceType' "AWS::ServiceCatalog::CloudFormationProvisionedProduct"

pattern AWSServiceCatalogPortfolio :: ResourceType
pattern AWSServiceCatalogPortfolio = ResourceType' "AWS::ServiceCatalog::Portfolio"

pattern AWSShieldProtection :: ResourceType
pattern AWSShieldProtection = ResourceType' "AWS::Shield::Protection"

pattern AWSShieldRegionalProtection :: ResourceType
pattern AWSShieldRegionalProtection = ResourceType' "AWS::ShieldRegional::Protection"

pattern AWSSqsQueue :: ResourceType
pattern AWSSqsQueue = ResourceType' "AWS::SQS::Queue"

pattern AWSSsmAssociationCompliance :: ResourceType
pattern AWSSsmAssociationCompliance = ResourceType' "AWS::SSM::AssociationCompliance"

pattern AWSSsmManagedInstanceInventory :: ResourceType
pattern AWSSsmManagedInstanceInventory = ResourceType' "AWS::SSM::ManagedInstanceInventory"

pattern AWSSsmPatchCompliance :: ResourceType
pattern AWSSsmPatchCompliance = ResourceType' "AWS::SSM::PatchCompliance"

pattern AWSWAFRegionalRateBasedRule :: ResourceType
pattern AWSWAFRegionalRateBasedRule = ResourceType' "AWS::WAFRegional::RateBasedRule"

pattern AWSWAFRegionalRule :: ResourceType
pattern AWSWAFRegionalRule = ResourceType' "AWS::WAFRegional::Rule"

pattern AWSWAFRegionalRuleGroup :: ResourceType
pattern AWSWAFRegionalRuleGroup = ResourceType' "AWS::WAFRegional::RuleGroup"

pattern AWSWAFRegionalWebACL :: ResourceType
pattern AWSWAFRegionalWebACL = ResourceType' "AWS::WAFRegional::WebACL"

pattern AWSWAFV2IPSet :: ResourceType
pattern AWSWAFV2IPSet = ResourceType' "AWS::WAFv2::IPSet"

pattern AWSWAFV2ManagedRuleSet :: ResourceType
pattern AWSWAFV2ManagedRuleSet = ResourceType' "AWS::WAFv2::ManagedRuleSet"

pattern AWSWAFV2RegexPatternSet :: ResourceType
pattern AWSWAFV2RegexPatternSet = ResourceType' "AWS::WAFv2::RegexPatternSet"

pattern AWSWAFV2RuleGroup :: ResourceType
pattern AWSWAFV2RuleGroup = ResourceType' "AWS::WAFv2::RuleGroup"

pattern AWSWAFV2WebACL :: ResourceType
pattern AWSWAFV2WebACL = ResourceType' "AWS::WAFv2::WebACL"

pattern AWSWafRateBasedRule :: ResourceType
pattern AWSWafRateBasedRule = ResourceType' "AWS::WAF::RateBasedRule"

pattern AWSWafRule :: ResourceType
pattern AWSWafRule = ResourceType' "AWS::WAF::Rule"

pattern AWSWafRuleGroup :: ResourceType
pattern AWSWafRuleGroup = ResourceType' "AWS::WAF::RuleGroup"

pattern AWSWafWebACL :: ResourceType
pattern AWSWafWebACL = ResourceType' "AWS::WAF::WebACL"

pattern AWSXRayEncryptionConfig :: ResourceType
pattern AWSXRayEncryptionConfig = ResourceType' "AWS::XRay::EncryptionConfig"

{-# COMPLETE
  AWSAPIGATEWAYV2API,
  AWSAPIGATEWAYV2Stage,
  AWSAPIGatewayRestAPI,
  AWSAPIGatewayStage,
  AWSAcmCertificate,
  AWSAutoScalingAutoScalingGroup,
  AWSAutoScalingLaunchConfiguration,
  AWSAutoScalingScalingPolicy,
  AWSAutoScalingScheduledAction,
  AWSCloudFormationStack,
  AWSCloudFrontDistribution,
  AWSCloudFrontStreamingDistribution,
  AWSCloudTrailTrail,
  AWSCloudWatchAlarm,
  AWSCodeBuildProject,
  AWSCodePipelinePipeline,
  AWSConfigResourceCompliance,
  AWSDynamoDBTable,
  AWSEC2CustomerGateway,
  AWSEC2EIP,
  AWSEC2EgressOnlyInternetGateway,
  AWSEC2FlowLog,
  AWSEC2Host,
  AWSEC2Instance,
  AWSEC2InternetGateway,
  AWSEC2NatGateway,
  AWSEC2NetworkACL,
  AWSEC2NetworkInterface,
  AWSEC2RegisteredHAInstance,
  AWSEC2RouteTable,
  AWSEC2SecurityGroup,
  AWSEC2Subnet,
  AWSEC2VPC,
  AWSEC2VPCEndpoint,
  AWSEC2VPCEndpointService,
  AWSEC2VPCPeeringConnection,
  AWSEC2VPNConnection,
  AWSEC2VPNGateway,
  AWSEC2Volume,
  AWSELASTICLOADBALANCINGV2LoadBalancer,
  AWSElasticBeanstalkApplication,
  AWSElasticBeanstalkApplicationVersion,
  AWSElasticBeanstalkEnvironment,
  AWSElasticLoadBalancingLoadBalancer,
  AWSElasticsearchDomain,
  AWSIAMGroup,
  AWSIAMPolicy,
  AWSIAMRole,
  AWSIAMUser,
  AWSKMSKey,
  AWSLambdaFunction,
  AWSQldbLedger,
  AWSRDSDBCluster,
  AWSRDSDBClusterSnapshot,
  AWSRDSDBInstance,
  AWSRDSDBSecurityGroup,
  AWSRDSDBSnapshot,
  AWSRDSDBSubnetGroup,
  AWSRDSEventSubscription,
  AWSRedshiftCluster,
  AWSRedshiftClusterParameterGroup,
  AWSRedshiftClusterSecurityGroup,
  AWSRedshiftClusterSnapshot,
  AWSRedshiftClusterSubnetGroup,
  AWSRedshiftEventSubscription,
  AWSS3AccountPublicAccessBlock,
  AWSS3Bucket,
  AWSServiceCatalogCloudFormationProduct,
  AWSServiceCatalogCloudFormationProvisionedProduct,
  AWSServiceCatalogPortfolio,
  AWSShieldProtection,
  AWSShieldRegionalProtection,
  AWSSqsQueue,
  AWSSsmAssociationCompliance,
  AWSSsmManagedInstanceInventory,
  AWSSsmPatchCompliance,
  AWSWAFRegionalRateBasedRule,
  AWSWAFRegionalRule,
  AWSWAFRegionalRuleGroup,
  AWSWAFRegionalWebACL,
  AWSWAFV2IPSet,
  AWSWAFV2ManagedRuleSet,
  AWSWAFV2RegexPatternSet,
  AWSWAFV2RuleGroup,
  AWSWAFV2WebACL,
  AWSWafRateBasedRule,
  AWSWafRule,
  AWSWafRuleGroup,
  AWSWafWebACL,
  AWSXRayEncryptionConfig,
  ResourceType' #-}

instance FromText ResourceType where
    parser = (ResourceType' . mk) <$> takeText

instance ToText ResourceType where
    toText (ResourceType' ci) = original ci

-- | Represents an enum of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceType where
    toEnum i = case i of
        0 -> AWSAPIGATEWAYV2API
        1 -> AWSAPIGATEWAYV2Stage
        2 -> AWSAPIGatewayRestAPI
        3 -> AWSAPIGatewayStage
        4 -> AWSAcmCertificate
        5 -> AWSAutoScalingAutoScalingGroup
        6 -> AWSAutoScalingLaunchConfiguration
        7 -> AWSAutoScalingScalingPolicy
        8 -> AWSAutoScalingScheduledAction
        9 -> AWSCloudFormationStack
        10 -> AWSCloudFrontDistribution
        11 -> AWSCloudFrontStreamingDistribution
        12 -> AWSCloudTrailTrail
        13 -> AWSCloudWatchAlarm
        14 -> AWSCodeBuildProject
        15 -> AWSCodePipelinePipeline
        16 -> AWSConfigResourceCompliance
        17 -> AWSDynamoDBTable
        18 -> AWSEC2CustomerGateway
        19 -> AWSEC2EIP
        20 -> AWSEC2EgressOnlyInternetGateway
        21 -> AWSEC2FlowLog
        22 -> AWSEC2Host
        23 -> AWSEC2Instance
        24 -> AWSEC2InternetGateway
        25 -> AWSEC2NatGateway
        26 -> AWSEC2NetworkACL
        27 -> AWSEC2NetworkInterface
        28 -> AWSEC2RegisteredHAInstance
        29 -> AWSEC2RouteTable
        30 -> AWSEC2SecurityGroup
        31 -> AWSEC2Subnet
        32 -> AWSEC2VPC
        33 -> AWSEC2VPCEndpoint
        34 -> AWSEC2VPCEndpointService
        35 -> AWSEC2VPCPeeringConnection
        36 -> AWSEC2VPNConnection
        37 -> AWSEC2VPNGateway
        38 -> AWSEC2Volume
        39 -> AWSELASTICLOADBALANCINGV2LoadBalancer
        40 -> AWSElasticBeanstalkApplication
        41 -> AWSElasticBeanstalkApplicationVersion
        42 -> AWSElasticBeanstalkEnvironment
        43 -> AWSElasticLoadBalancingLoadBalancer
        44 -> AWSElasticsearchDomain
        45 -> AWSIAMGroup
        46 -> AWSIAMPolicy
        47 -> AWSIAMRole
        48 -> AWSIAMUser
        49 -> AWSKMSKey
        50 -> AWSLambdaFunction
        51 -> AWSQldbLedger
        52 -> AWSRDSDBCluster
        53 -> AWSRDSDBClusterSnapshot
        54 -> AWSRDSDBInstance
        55 -> AWSRDSDBSecurityGroup
        56 -> AWSRDSDBSnapshot
        57 -> AWSRDSDBSubnetGroup
        58 -> AWSRDSEventSubscription
        59 -> AWSRedshiftCluster
        60 -> AWSRedshiftClusterParameterGroup
        61 -> AWSRedshiftClusterSecurityGroup
        62 -> AWSRedshiftClusterSnapshot
        63 -> AWSRedshiftClusterSubnetGroup
        64 -> AWSRedshiftEventSubscription
        65 -> AWSS3AccountPublicAccessBlock
        66 -> AWSS3Bucket
        67 -> AWSServiceCatalogCloudFormationProduct
        68 -> AWSServiceCatalogCloudFormationProvisionedProduct
        69 -> AWSServiceCatalogPortfolio
        70 -> AWSShieldProtection
        71 -> AWSShieldRegionalProtection
        72 -> AWSSqsQueue
        73 -> AWSSsmAssociationCompliance
        74 -> AWSSsmManagedInstanceInventory
        75 -> AWSSsmPatchCompliance
        76 -> AWSWAFRegionalRateBasedRule
        77 -> AWSWAFRegionalRule
        78 -> AWSWAFRegionalRuleGroup
        79 -> AWSWAFRegionalWebACL
        80 -> AWSWAFV2IPSet
        81 -> AWSWAFV2ManagedRuleSet
        82 -> AWSWAFV2RegexPatternSet
        83 -> AWSWAFV2RuleGroup
        84 -> AWSWAFV2WebACL
        85 -> AWSWafRateBasedRule
        86 -> AWSWafRule
        87 -> AWSWafRuleGroup
        88 -> AWSWafWebACL
        89 -> AWSXRayEncryptionConfig
        _ -> (error . showText) $ "Unknown index for ResourceType: " <> toText i
    fromEnum x = case x of
        AWSAPIGATEWAYV2API -> 0
        AWSAPIGATEWAYV2Stage -> 1
        AWSAPIGatewayRestAPI -> 2
        AWSAPIGatewayStage -> 3
        AWSAcmCertificate -> 4
        AWSAutoScalingAutoScalingGroup -> 5
        AWSAutoScalingLaunchConfiguration -> 6
        AWSAutoScalingScalingPolicy -> 7
        AWSAutoScalingScheduledAction -> 8
        AWSCloudFormationStack -> 9
        AWSCloudFrontDistribution -> 10
        AWSCloudFrontStreamingDistribution -> 11
        AWSCloudTrailTrail -> 12
        AWSCloudWatchAlarm -> 13
        AWSCodeBuildProject -> 14
        AWSCodePipelinePipeline -> 15
        AWSConfigResourceCompliance -> 16
        AWSDynamoDBTable -> 17
        AWSEC2CustomerGateway -> 18
        AWSEC2EIP -> 19
        AWSEC2EgressOnlyInternetGateway -> 20
        AWSEC2FlowLog -> 21
        AWSEC2Host -> 22
        AWSEC2Instance -> 23
        AWSEC2InternetGateway -> 24
        AWSEC2NatGateway -> 25
        AWSEC2NetworkACL -> 26
        AWSEC2NetworkInterface -> 27
        AWSEC2RegisteredHAInstance -> 28
        AWSEC2RouteTable -> 29
        AWSEC2SecurityGroup -> 30
        AWSEC2Subnet -> 31
        AWSEC2VPC -> 32
        AWSEC2VPCEndpoint -> 33
        AWSEC2VPCEndpointService -> 34
        AWSEC2VPCPeeringConnection -> 35
        AWSEC2VPNConnection -> 36
        AWSEC2VPNGateway -> 37
        AWSEC2Volume -> 38
        AWSELASTICLOADBALANCINGV2LoadBalancer -> 39
        AWSElasticBeanstalkApplication -> 40
        AWSElasticBeanstalkApplicationVersion -> 41
        AWSElasticBeanstalkEnvironment -> 42
        AWSElasticLoadBalancingLoadBalancer -> 43
        AWSElasticsearchDomain -> 44
        AWSIAMGroup -> 45
        AWSIAMPolicy -> 46
        AWSIAMRole -> 47
        AWSIAMUser -> 48
        AWSKMSKey -> 49
        AWSLambdaFunction -> 50
        AWSQldbLedger -> 51
        AWSRDSDBCluster -> 52
        AWSRDSDBClusterSnapshot -> 53
        AWSRDSDBInstance -> 54
        AWSRDSDBSecurityGroup -> 55
        AWSRDSDBSnapshot -> 56
        AWSRDSDBSubnetGroup -> 57
        AWSRDSEventSubscription -> 58
        AWSRedshiftCluster -> 59
        AWSRedshiftClusterParameterGroup -> 60
        AWSRedshiftClusterSecurityGroup -> 61
        AWSRedshiftClusterSnapshot -> 62
        AWSRedshiftClusterSubnetGroup -> 63
        AWSRedshiftEventSubscription -> 64
        AWSS3AccountPublicAccessBlock -> 65
        AWSS3Bucket -> 66
        AWSServiceCatalogCloudFormationProduct -> 67
        AWSServiceCatalogCloudFormationProvisionedProduct -> 68
        AWSServiceCatalogPortfolio -> 69
        AWSShieldProtection -> 70
        AWSShieldRegionalProtection -> 71
        AWSSqsQueue -> 72
        AWSSsmAssociationCompliance -> 73
        AWSSsmManagedInstanceInventory -> 74
        AWSSsmPatchCompliance -> 75
        AWSWAFRegionalRateBasedRule -> 76
        AWSWAFRegionalRule -> 77
        AWSWAFRegionalRuleGroup -> 78
        AWSWAFRegionalWebACL -> 79
        AWSWAFV2IPSet -> 80
        AWSWAFV2ManagedRuleSet -> 81
        AWSWAFV2RegexPatternSet -> 82
        AWSWAFV2RuleGroup -> 83
        AWSWAFV2WebACL -> 84
        AWSWafRateBasedRule -> 85
        AWSWafRule -> 86
        AWSWafRuleGroup -> 87
        AWSWafWebACL -> 88
        AWSXRayEncryptionConfig -> 89
        ResourceType' name -> (error . showText) $ "Unknown ResourceType: " <> original name

-- | Represents the bounds of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceType where
    minBound = AWSAPIGATEWAYV2API
    maxBound = AWSXRayEncryptionConfig

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"
