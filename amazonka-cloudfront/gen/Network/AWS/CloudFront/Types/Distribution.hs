{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Distribution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Distribution where

import Network.AWS.CloudFront.Types.ActiveTrustedSigners
import Network.AWS.CloudFront.Types.AliasICPRecordal
import Network.AWS.CloudFront.Types.DistributionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A distribution tells CloudFront where you want content to be delivered from, and the details about how to track and manage content delivery.
--
--
--
-- /See:/ 'distribution' smart constructor.
data Distribution = Distribution'{_dAliasICPRecordals
                                  :: !(Maybe [AliasICPRecordal]),
                                  _dId :: !Text, _dARN :: !Text,
                                  _dStatus :: !Text,
                                  _dLastModifiedTime :: !ISO8601,
                                  _dInProgressInvalidationBatches :: !Int,
                                  _dDomainName :: !Text,
                                  _dActiveTrustedSigners ::
                                  !ActiveTrustedSigners,
                                  _dDistributionConfig :: !DistributionConfig}
                      deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Distribution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAliasICPRecordals' - AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
--
-- * 'dId' - The identifier for the distribution. For example: @EDFDVBD632BHDS5@ . 
--
-- * 'dARN' - The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- * 'dStatus' - This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations. 
--
-- * 'dLastModifiedTime' - The date and time the distribution was last modified. 
--
-- * 'dInProgressInvalidationBatches' - The number of invalidation batches currently in progress. 
--
-- * 'dDomainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ . 
--
-- * 'dActiveTrustedSigners' - CloudFront automatically adds this element to the response only if you've set up the distribution to serve private content with signed URLs. The element lists the key pair IDs that CloudFront is aware of for each trusted signer. The @Signer@ child element lists the AWS account number of the trusted signer (or an empty @Self@ element if the signer is you). The @Signer@ element also includes the IDs of any active key pairs associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create working signed URLs.
--
-- * 'dDistributionConfig' - The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
distribution
    :: Text -- ^ 'dId'
    -> Text -- ^ 'dARN'
    -> Text -- ^ 'dStatus'
    -> UTCTime -- ^ 'dLastModifiedTime'
    -> Int -- ^ 'dInProgressInvalidationBatches'
    -> Text -- ^ 'dDomainName'
    -> ActiveTrustedSigners -- ^ 'dActiveTrustedSigners'
    -> DistributionConfig -- ^ 'dDistributionConfig'
    -> Distribution
distribution pId_ pARN_ pStatus_ pLastModifiedTime_
  pInProgressInvalidationBatches_ pDomainName_
  pActiveTrustedSigners_ pDistributionConfig_
  = Distribution'{_dAliasICPRecordals = Nothing,
                  _dId = pId_, _dARN = pARN_, _dStatus = pStatus_,
                  _dLastModifiedTime = _Time # pLastModifiedTime_,
                  _dInProgressInvalidationBatches =
                    pInProgressInvalidationBatches_,
                  _dDomainName = pDomainName_,
                  _dActiveTrustedSigners = pActiveTrustedSigners_,
                  _dDistributionConfig = pDistributionConfig_}

-- | AWS services in China customers must file for an Internet Content Provider (ICP) recordal if they want to serve content publicly on an alternate domain name, also known as a CNAME, that they've added to CloudFront. AliasICPRecordal provides the ICP recordal status for CNAMEs associated with distributions. For more information about ICP recordals, see <https://docs.amazonaws.cn/en_us/aws/latest/userguide/accounts-and-credentials.html Signup, Accounts, and Credentials> in /Getting Started with AWS services in China/ .
dAliasICPRecordals :: Lens' Distribution [AliasICPRecordal]
dAliasICPRecordals = lens _dAliasICPRecordals (\ s a -> s{_dAliasICPRecordals = a}) . _Default . _Coerce

-- | The identifier for the distribution. For example: @EDFDVBD632BHDS5@ . 
dId :: Lens' Distribution Text
dId = lens _dId (\ s a -> s{_dId = a})

-- | The ARN (Amazon Resource Name) for the distribution. For example: @arn:aws:cloudfront::123456789012:distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
dARN :: Lens' Distribution Text
dARN = lens _dARN (\ s a -> s{_dARN = a})

-- | This response element indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated to all CloudFront edge locations. 
dStatus :: Lens' Distribution Text
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The date and time the distribution was last modified. 
dLastModifiedTime :: Lens' Distribution UTCTime
dLastModifiedTime = lens _dLastModifiedTime (\ s a -> s{_dLastModifiedTime = a}) . _Time

-- | The number of invalidation batches currently in progress. 
dInProgressInvalidationBatches :: Lens' Distribution Int
dInProgressInvalidationBatches = lens _dInProgressInvalidationBatches (\ s a -> s{_dInProgressInvalidationBatches = a})

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ . 
dDomainName :: Lens' Distribution Text
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a})

-- | CloudFront automatically adds this element to the response only if you've set up the distribution to serve private content with signed URLs. The element lists the key pair IDs that CloudFront is aware of for each trusted signer. The @Signer@ child element lists the AWS account number of the trusted signer (or an empty @Self@ element if the signer is you). The @Signer@ element also includes the IDs of any active key pairs associated with the trusted signer's AWS account. If no @KeyPairId@ element appears for a @Signer@ , that signer can't create working signed URLs.
dActiveTrustedSigners :: Lens' Distribution ActiveTrustedSigners
dActiveTrustedSigners = lens _dActiveTrustedSigners (\ s a -> s{_dActiveTrustedSigners = a})

-- | The current configuration information for the distribution. Send a @GET@ request to the @//CloudFront API version/ /distribution ID/config@ resource.
dDistributionConfig :: Lens' Distribution DistributionConfig
dDistributionConfig = lens _dDistributionConfig (\ s a -> s{_dDistributionConfig = a})

instance FromXML Distribution where
        parseXML x
          = Distribution' <$>
              (x .@? "AliasICPRecordals" .!@ mempty >>=
                 may (parseXMLList "AliasICPRecordal"))
                <*> (x .@ "Id")
                <*> (x .@ "ARN")
                <*> (x .@ "Status")
                <*> (x .@ "LastModifiedTime")
                <*> (x .@ "InProgressInvalidationBatches")
                <*> (x .@ "DomainName")
                <*> (x .@ "ActiveTrustedSigners")
                <*> (x .@ "DistributionConfig")

instance Hashable Distribution where

instance NFData Distribution where
