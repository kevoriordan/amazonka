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
-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication configuration or replaces an existing one. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon S3 Developer Guide/ . 
--
--
-- Specify the replication configuration in the request body. In the replication configuration, you provide the name of the destination bucket where you want Amazon S3 to replicate objects, the IAM role that Amazon S3 can assume to replicate objects on your behalf, and other relevant information.
--
-- A replication configuration must include at least one rule, and can contain a maximum of 1,000. Each rule identifies a subset of objects to replicate by filtering the objects in the source bucket. To choose additional subsets of objects to replicate, add a rule for each subset. All rules must specify the same destination bucket.
--
-- To specify a subset of the objects in the source bucket to apply a replication rule to, add the Filter element as a child of the Rule element. You can filter objects based on an object key prefix, one or more object tags, or both. When you add the Filter element in the configuration, you must also add the following elements: @DeleteMarkerReplication@ , @Status@ , and @Priority@ .
--
-- For information about enabling versioning on a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html Using Versioning> .
--
-- By default, a resource owner, in this case the AWS account that created the bucket, can perform this operation. The resource owner can also grant others permissions to perform the operation. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- __Handling Replication of Encrypted Objects__ 
--
-- By default, Amazon S3 doesn't replicate objects that are stored at rest using server-side encryption with CMKs stored in AWS KMS. To replicate AWS KMS-encrypted objects, add the following: @SourceSelectionCriteria@ , @SseKmsEncryptedObjects@ , @Status@ , @EncryptionConfiguration@ , and @ReplicaKmsKeyID@ . For information about replication configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-config-for-kms-objects.html Replicating Objects Created with SSE Using CMKs stored in AWS KMS> .
--
-- For information on @PutBucketReplication@ errors, see 'ReplicationErrorCodeList' 
--
-- The following operations are related to @PutBucketReplication@ :
--
--     * 'GetBucketReplication' 
--
--     * 'DeleteBucketReplication' 
--
--
--
module Network.AWS.S3.PutBucketReplication
    (
    -- * Creating a Request
      putBucketReplication
    , PutBucketReplication
    -- * Request Lenses
    , pbrToken
    , pbrContentMD5
    , pbrBucket
    , pbrReplicationConfiguration

    -- * Destructuring the Response
    , putBucketReplicationResponse
    , PutBucketReplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketReplication' smart constructor.
data PutBucketReplication = PutBucketReplication'{_pbrToken
                                                  :: !(Maybe Text),
                                                  _pbrContentMD5 ::
                                                  !(Maybe Text),
                                                  _pbrBucket :: !BucketName,
                                                  _pbrReplicationConfiguration
                                                  :: !ReplicationConfiguration}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrToken' - 
--
-- * 'pbrContentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- * 'pbrBucket' - The name of the bucket
--
-- * 'pbrReplicationConfiguration' - Undocumented member.
putBucketReplication
    :: BucketName -- ^ 'pbrBucket'
    -> ReplicationConfiguration -- ^ 'pbrReplicationConfiguration'
    -> PutBucketReplication
putBucketReplication pBucket_
  pReplicationConfiguration_
  = PutBucketReplication'{_pbrToken = Nothing,
                          _pbrContentMD5 = Nothing, _pbrBucket = pBucket_,
                          _pbrReplicationConfiguration =
                            pReplicationConfiguration_}

-- | 
pbrToken :: Lens' PutBucketReplication (Maybe Text)
pbrToken = lens _pbrToken (\ s a -> s{_pbrToken = a})

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
pbrContentMD5 :: Lens' PutBucketReplication (Maybe Text)
pbrContentMD5 = lens _pbrContentMD5 (\ s a -> s{_pbrContentMD5 = a})

-- | The name of the bucket
pbrBucket :: Lens' PutBucketReplication BucketName
pbrBucket = lens _pbrBucket (\ s a -> s{_pbrBucket = a})

-- | Undocumented member.
pbrReplicationConfiguration :: Lens' PutBucketReplication ReplicationConfiguration
pbrReplicationConfiguration = lens _pbrReplicationConfiguration (\ s a -> s{_pbrReplicationConfiguration = a})

instance AWSRequest PutBucketReplication where
        type Rs PutBucketReplication =
             PutBucketReplicationResponse
        request = putXML s3
        response = receiveNull PutBucketReplicationResponse'

instance Hashable PutBucketReplication where

instance NFData PutBucketReplication where

instance ToElement PutBucketReplication where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}ReplicationConfiguration"
              .
              _pbrReplicationConfiguration

instance ToHeaders PutBucketReplication where
        toHeaders PutBucketReplication'{..}
          = mconcat
              ["x-amz-bucket-object-lock-token" =# _pbrToken,
               "Content-MD5" =# _pbrContentMD5]

instance ToPath PutBucketReplication where
        toPath PutBucketReplication'{..}
          = mconcat ["/", toBS _pbrBucket]

instance ToQuery PutBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'putBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse = PutBucketReplicationResponse'
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'PutBucketReplicationResponse' with the minimum fields required to make a request.
--
putBucketReplicationResponse
    :: PutBucketReplicationResponse
putBucketReplicationResponse
  = PutBucketReplicationResponse'

instance NFData PutBucketReplicationResponse where
