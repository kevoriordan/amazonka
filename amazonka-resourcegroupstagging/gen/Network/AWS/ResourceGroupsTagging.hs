{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Resource Groups Tagging API__ 
--
-- This guide describes the API operations for the resource groups tagging.
--
-- A tag is a label that you assign to an AWS resource. A tag consists of a key and a value, both of which you define. For example, if you have two Amazon EC2 instances, you might assign both a tag key of "Stack." But the value of "Stack" might be "Testing" for one and "Production" for the other.
--
-- Tagging can help you organize your resources and enables you to simplify resource management, access management and cost allocation. 
--
-- You can use the resource groups tagging API operations to complete the following tasks:
--
--     * Tag and untag supported resources located in the specified Region for the AWS account.
--
--     * Use tag-based filters to search for resources located in the specified Region for the AWS account.
--
--     * List all existing tag keys in the specified Region for the AWS account.
--
--     * List all existing values for the specified key in the specified Region for the AWS account.
--
--
--
-- To use resource groups tagging API operations, you must add the following permissions to your IAM policy:
--
--     * @tag:GetResources@ 
--
--     * @tag:TagResources@ 
--
--     * @tag:UntagResources@ 
--
--     * @tag:GetTagKeys@ 
--
--     * @tag:GetTagValues@ 
--
--
--
-- You'll also need permissions to access the resources of individual services so that you can tag and untag those resources.
--
-- For more information on IAM policies, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_manage.html Managing IAM Policies> in the /IAM User Guide/ .
--
-- You can use the Resource Groups Tagging API to tag resources for the following AWS services.
--
--     * Alexa for Business (a4b)
--
--     * API Gateway
--
--     * Amazon AppStream
--
--     * AWS AppSync
--
--     * AWS App Mesh
--
--     * Amazon Athena
--
--     * Amazon Aurora
--
--     * AWS Backup
--
--     * AWS Certificate Manager
--
--     * AWS Certificate Manager Private CA
--
--     * Amazon Cloud Directory
--
--     * AWS CloudFormation
--
--     * Amazon CloudFront
--
--     * AWS CloudHSM
--
--     * AWS CloudTrail
--
--     * Amazon CloudWatch (alarms only)
--
--     * Amazon CloudWatch Events
--
--     * Amazon CloudWatch Logs
--
--     * AWS CodeBuild
--
--     * AWS CodeCommit
--
--     * AWS CodePipeline
--
--     * AWS CodeStar
--
--     * Amazon Cognito Identity
--
--     * Amazon Cognito User Pools
--
--     * Amazon Comprehend
--
--     * AWS Config
--
--     * AWS Data Exchange
--
--     * AWS Data Pipeline
--
--     * AWS Database Migration Service
--
--     * AWS DataSync
--
--     * AWS Device Farm
--
--     * AWS Direct Connect
--
--     * AWS Directory Service
--
--     * Amazon DynamoDB
--
--     * Amazon EBS
--
--     * Amazon EC2
--
--     * Amazon ECR
--
--     * Amazon ECS
--
--     * Amazon EKS
--
--     * AWS Elastic Beanstalk
--
--     * Amazon Elastic File System
--
--     * Elastic Load Balancing
--
--     * Amazon ElastiCache
--
--     * Amazon Elasticsearch Service
--
--     * AWS Elemental MediaLive
--
--     * AWS Elemental MediaPackage
--
--     * AWS Elemental MediaTailor
--
--     * Amazon EMR
--
--     * Amazon FSx
--
--     * Amazon S3 Glacier
--
--     * AWS Glue
--
--     * Amazon GuardDuty
--
--     * Amazon Inspector
--
--     * AWS IoT Analytics
--
--     * AWS IoT Core
--
--     * AWS IoT Device Defender
--
--     * AWS IoT Device Management
--
--     * AWS IoT Events
--
--     * AWS IoT Greengrass
--
--     * AWS IoT 1-Click
--
--     * AWS Key Management Service
--
--     * Amazon Kinesis
--
--     * Amazon Kinesis Data Analytics
--
--     * Amazon Kinesis Data Firehose
--
--     * AWS Lambda
--
--     * AWS License Manager
--
--     * Amazon Machine Learning
--
--     * Amazon MQ
--
--     * Amazon MSK
--
--     * Amazon Neptune
--
--     * AWS OpsWorks
--
--     * AWS Organizations
--
--     * Amazon Quantum Ledger Database (QLDB)
--
--     * Amazon RDS
--
--     * Amazon Redshift
--
--     * AWS Resource Access Manager
--
--     * AWS Resource Groups
--
--     * AWS RoboMaker
--
--     * Amazon Route 53
--
--     * Amazon Route 53 Resolver
--
--     * Amazon S3 (buckets only)
--
--     * Amazon SageMaker
--
--     * AWS Secrets Manager
--
--     * AWS Security Hub
--
--     * AWS Service Catalog
--
--     * Amazon Simple Notification Service (SNS)
--
--     * Amazon Simple Queue Service (SQS)
--
--     * Amazon Simple Workflow Service
--
--     * AWS Step Functions
--
--     * AWS Storage Gateway
--
--     * AWS Systems Manager
--
--     * AWS Transfer for SFTP
--
--     * Amazon VPC
--
--     * Amazon WorkSpaces
--
--
--
module Network.AWS.ResourceGroupsTagging
    (
    -- * Service Configuration
      resourceGroupsTagging

    -- * Errors
    -- $errors

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** ConstraintViolationException
    , _ConstraintViolationException

    -- ** ThrottledException
    , _ThrottledException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** PaginationTokenExpiredException
    , _PaginationTokenExpiredException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetComplianceSummary (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetComplianceSummary

    -- ** DescribeReportCreation 
    , module Network.AWS.ResourceGroupsTagging.DescribeReportCreation

    -- ** GetTagKeys (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagKeys

    -- ** TagResources 
    , module Network.AWS.ResourceGroupsTagging.TagResources

    -- ** GetTagValues (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetTagValues

    -- ** StartReportCreation 
    , module Network.AWS.ResourceGroupsTagging.StartReportCreation

    -- ** GetResources (Paginated)
    , module Network.AWS.ResourceGroupsTagging.GetResources

    -- ** UntagResources 
    , module Network.AWS.ResourceGroupsTagging.UntagResources

    -- * Types

    -- ** GroupByAttribute
    , GroupByAttribute (..)

    -- ** ResourceErrorCode
    , ResourceErrorCode (..)

    -- ** TargetIdType
    , TargetIdType (..)

    -- ** ComplianceDetails
    , ComplianceDetails
    , complianceDetails
    , cdKeysWithNoncompliantValues
    , cdComplianceStatus
    , cdNoncompliantKeys

    -- ** FailureInfo
    , FailureInfo
    , failureInfo
    , fiErrorCode
    , fiErrorMessage
    , fiStatusCode

    -- ** ResourceTagMapping
    , ResourceTagMapping
    , resourceTagMapping
    , rtmComplianceDetails
    , rtmResourceARN
    , rtmTags

    -- ** Summary
    , Summary
    , summary
    , sTargetId
    , sLastUpdated
    , sResourceType
    , sNonCompliantResources
    , sTargetIdType
    , sRegion

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TagFilter
    , TagFilter
    , tagFilter
    , tfValues
    , tfKey
    ) where

import Network.AWS.ResourceGroupsTagging.DescribeReportCreation
import Network.AWS.ResourceGroupsTagging.GetComplianceSummary
import Network.AWS.ResourceGroupsTagging.GetResources
import Network.AWS.ResourceGroupsTagging.GetTagKeys
import Network.AWS.ResourceGroupsTagging.GetTagValues
import Network.AWS.ResourceGroupsTagging.StartReportCreation
import Network.AWS.ResourceGroupsTagging.TagResources
import Network.AWS.ResourceGroupsTagging.Types
import Network.AWS.ResourceGroupsTagging.UntagResources
import Network.AWS.ResourceGroupsTagging.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ResourceGroupsTagging'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
