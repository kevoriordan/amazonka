{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Security Token Service__ 
--
-- The AWS Security Token Service (STS) is a web service that enables you to request temporary, limited-privilege credentials for AWS Identity and Access Management (IAM) users or for users that you authenticate (federated users). This guide provides descriptions of the STS API. For more detailed information about using this service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp.html Temporary Security Credentials> . 
--
-- For information about setting up signatures and authorization through the API, go to <https://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API Requests> in the /AWS General Reference/ . For general information about the Query API, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making Query Requests> in /Using IAM/ . For information about using security tokens with other AWS products, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_aws-services-that-work-with-iam.html AWS Services That Work with IAM> in the /IAM User Guide/ . 
--
-- If you're new to AWS and need additional technical information about a specific AWS product, you can find the product's technical documentation at <http://aws.amazon.com/documentation/ http://aws.amazon.com/documentation/> . 
--
-- __Endpoints__ 
--
-- By default, AWS Security Token Service (STS) is available as a global service, and all AWS STS requests go to a single endpoint at @https://sts.amazonaws.com@ . Global requests map to the US East (N. Virginia) region. AWS recommends using Regional AWS STS endpoints instead of the global endpoint to reduce latency, build in redundancy, and increase session token validity. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Managing AWS STS in an AWS Region> in the /IAM User Guide/ .
--
-- Most AWS Regions are enabled for operations in all AWS services by default. Those Regions are automatically activated for use with AWS STS. Some Regions, such as Asia Pacific (Hong Kong), must be manually enabled. To learn more about enabling and disabling AWS Regions, see <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing AWS Regions> in the /AWS General Reference/ . When you enable these AWS Regions, they are automatically activated for use with AWS STS. You cannot activate the STS endpoint for a Region that is disabled. Tokens that are valid in all AWS Regions are longer than tokens that are valid in Regions that are enabled by default. Changing this setting might affect existing systems where you temporarily store tokens. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html#sts-regions-manage-tokens Managing Global Endpoint Session Tokens> in the /IAM User Guide/ .
--
-- After you activate a Region for use with AWS STS, you can direct AWS STS API calls to that Region. AWS STS recommends that you provide both the Region and endpoint when you make calls to a Regional endpoint. You can provide the Region alone for manually enabled Regions, such as Asia Pacific (Hong Kong). In this case, the calls are directed to the STS Regional endpoint. However, if you provide the Region alone for Regions enabled by default, the calls are directed to the global endpoint of @https://sts.amazonaws.com@ .
--
-- To view the list of AWS STS endpoints and whether they are active by default, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html#id_credentials_temp_enable-regions_writing_code Writing Code to Use AWS STS Regions> in the /IAM User Guide/ .
--
-- __Recording API requests__ 
--
-- STS supports AWS CloudTrail, which is a service that records AWS calls for your AWS account and delivers log files to an Amazon S3 bucket. By using information collected by CloudTrail, you can determine what requests were successfully made to STS, who made the request, when it was made, and so on.
--
-- If you activate AWS STS endpoints in Regions other than the default global endpoint, then you must also turn on CloudTrail logging in those Regions. This is necessary to record any AWS STS API calls that are made in those Regions. For more information, see <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/aggregating_logs_regions_turn_on_ct.html Turning On CloudTrail in Additional Regions> in the /AWS CloudTrail User Guide/ .
--
-- AWS Security Token Service (STS) is a global service with a single endpoint at @https://sts.amazonaws.com@ . Calls to this endpoint are logged as calls to a global service. However, because this endpoint is physically located in the US East (N. Virginia) Region, your logs list @us-east-1@ as the event Region. CloudTrail does not write these logs to the US East (Ohio) Region unless you choose to include global service logs in that Region. CloudTrail writes calls to all Regional endpoints to their respective Regions. For example, calls to sts.us-east-2.amazonaws.com are published to the US East (Ohio) Region and calls to sts.eu-central-1.amazonaws.com are published to the EU (Frankfurt) Region.
--
-- To learn more about CloudTrail, including how to turn it on and find your log files, see the <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html AWS CloudTrail User Guide> .
--
module Network.AWS.STS
    (
    -- * Service Configuration
      sts

    -- * Errors
    -- $errors

    -- ** IdPCommunicationErrorException
    , _IdPCommunicationErrorException

    -- ** IdPRejectedClaimException
    , _IdPRejectedClaimException

    -- ** InvalidAuthorizationMessageException
    , _InvalidAuthorizationMessageException

    -- ** ExpiredTokenException
    , _ExpiredTokenException

    -- ** InvalidIdentityTokenException
    , _InvalidIdentityTokenException

    -- ** MalformedPolicyDocumentException
    , _MalformedPolicyDocumentException

    -- ** RegionDisabledException
    , _RegionDisabledException

    -- ** PackedPolicyTooLargeException
    , _PackedPolicyTooLargeException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetCallerIdentity 
    , module Network.AWS.STS.GetCallerIdentity

    -- ** AssumeRole 
    , module Network.AWS.STS.AssumeRole

    -- ** GetAccessKeyInfo 
    , module Network.AWS.STS.GetAccessKeyInfo

    -- ** DecodeAuthorizationMessage 
    , module Network.AWS.STS.DecodeAuthorizationMessage

    -- ** AssumeRoleWithWebIdentity 
    , module Network.AWS.STS.AssumeRoleWithWebIdentity

    -- ** GetFederationToken 
    , module Network.AWS.STS.GetFederationToken

    -- ** GetSessionToken 
    , module Network.AWS.STS.GetSessionToken

    -- ** AssumeRoleWithSAML 
    , module Network.AWS.STS.AssumeRoleWithSAML

    -- * Types

    -- ** AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruARN

    -- ** FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuARN

    -- ** PolicyDescriptorType
    , PolicyDescriptorType
    , policyDescriptorType
    , pdtArn

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.STS.AssumeRole
import Network.AWS.STS.AssumeRoleWithSAML
import Network.AWS.STS.AssumeRoleWithWebIdentity
import Network.AWS.STS.DecodeAuthorizationMessage
import Network.AWS.STS.GetAccessKeyInfo
import Network.AWS.STS.GetCallerIdentity
import Network.AWS.STS.GetFederationToken
import Network.AWS.STS.GetSessionToken
import Network.AWS.STS.Types
import Network.AWS.STS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'STS'.
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
