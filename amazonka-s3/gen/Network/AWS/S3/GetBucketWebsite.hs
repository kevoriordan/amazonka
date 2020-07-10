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
-- Module      : Network.AWS.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the website configuration for a bucket. To host website on Amazon S3, you can configure a bucket as website by adding a website configuration. For more information about hosting websites, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3> . 
--
--
-- This GET operation requires the @S3:GetBucketWebsite@ permission. By default, only the bucket owner can read the bucket website configuration. However, bucket owners can allow other users to read the website configuration by writing a bucket policy granting them the @S3:GetBucketWebsite@ permission.
--
-- The following operations are related to @DeleteBucketWebsite@ :
--
--     * 'DeleteBucketWebsite' 
--
--     * 'PutBucketWebsite' 
--
--
--
module Network.AWS.S3.GetBucketWebsite
    (
    -- * Creating a Request
      getBucketWebsite
    , GetBucketWebsite
    -- * Request Lenses
    , gbwBucket

    -- * Destructuring the Response
    , getBucketWebsiteResponse
    , GetBucketWebsiteResponse
    -- * Response Lenses
    , gbwrsRedirectAllRequestsTo
    , gbwrsErrorDocument
    , gbwrsIndexDocument
    , gbwrsRoutingRules
    , gbwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketWebsite' smart constructor.
newtype GetBucketWebsite = GetBucketWebsite'{_gbwBucket
                                             :: BucketName}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbwBucket' - The bucket name for which to get the website configuration.
getBucketWebsite
    :: BucketName -- ^ 'gbwBucket'
    -> GetBucketWebsite
getBucketWebsite pBucket_
  = GetBucketWebsite'{_gbwBucket = pBucket_}

-- | The bucket name for which to get the website configuration.
gbwBucket :: Lens' GetBucketWebsite BucketName
gbwBucket = lens _gbwBucket (\ s a -> s{_gbwBucket = a})

instance AWSRequest GetBucketWebsite where
        type Rs GetBucketWebsite = GetBucketWebsiteResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketWebsiteResponse' <$>
                   (x .@? "RedirectAllRequestsTo") <*>
                     (x .@? "ErrorDocument")
                     <*> (x .@? "IndexDocument")
                     <*>
                     (x .@? "RoutingRules" .!@ mempty >>=
                        may (parseXMLList "RoutingRule"))
                     <*> (pure (fromEnum s)))

instance Hashable GetBucketWebsite where

instance NFData GetBucketWebsite where

instance ToHeaders GetBucketWebsite where
        toHeaders = const mempty

instance ToPath GetBucketWebsite where
        toPath GetBucketWebsite'{..}
          = mconcat ["/", toBS _gbwBucket]

instance ToQuery GetBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'getBucketWebsiteResponse' smart constructor.
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'{_gbwrsRedirectAllRequestsTo
                                                          ::
                                                          !(Maybe
                                                              RedirectAllRequestsTo),
                                                          _gbwrsErrorDocument ::
                                                          !(Maybe
                                                              ErrorDocument),
                                                          _gbwrsIndexDocument ::
                                                          !(Maybe
                                                              IndexDocument),
                                                          _gbwrsRoutingRules ::
                                                          !(Maybe
                                                              [RoutingRule]),
                                                          _gbwrsResponseStatus
                                                          :: !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'GetBucketWebsiteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbwrsRedirectAllRequestsTo' - Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
--
-- * 'gbwrsErrorDocument' - The name of the error document for the website.
--
-- * 'gbwrsIndexDocument' - The name of the index document for the website.
--
-- * 'gbwrsRoutingRules' - Rules that define when a redirect is applied and the redirect behavior.
--
-- * 'gbwrsResponseStatus' - -- | The response status code.
getBucketWebsiteResponse
    :: Int -- ^ 'gbwrsResponseStatus'
    -> GetBucketWebsiteResponse
getBucketWebsiteResponse pResponseStatus_
  = GetBucketWebsiteResponse'{_gbwrsRedirectAllRequestsTo
                                = Nothing,
                              _gbwrsErrorDocument = Nothing,
                              _gbwrsIndexDocument = Nothing,
                              _gbwrsRoutingRules = Nothing,
                              _gbwrsResponseStatus = pResponseStatus_}

-- | Specifies the redirect behavior of all requests to a website endpoint of an Amazon S3 bucket.
gbwrsRedirectAllRequestsTo :: Lens' GetBucketWebsiteResponse (Maybe RedirectAllRequestsTo)
gbwrsRedirectAllRequestsTo = lens _gbwrsRedirectAllRequestsTo (\ s a -> s{_gbwrsRedirectAllRequestsTo = a})

-- | The name of the error document for the website.
gbwrsErrorDocument :: Lens' GetBucketWebsiteResponse (Maybe ErrorDocument)
gbwrsErrorDocument = lens _gbwrsErrorDocument (\ s a -> s{_gbwrsErrorDocument = a})

-- | The name of the index document for the website.
gbwrsIndexDocument :: Lens' GetBucketWebsiteResponse (Maybe IndexDocument)
gbwrsIndexDocument = lens _gbwrsIndexDocument (\ s a -> s{_gbwrsIndexDocument = a})

-- | Rules that define when a redirect is applied and the redirect behavior.
gbwrsRoutingRules :: Lens' GetBucketWebsiteResponse [RoutingRule]
gbwrsRoutingRules = lens _gbwrsRoutingRules (\ s a -> s{_gbwrsRoutingRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gbwrsResponseStatus :: Lens' GetBucketWebsiteResponse Int
gbwrsResponseStatus = lens _gbwrsResponseStatus (\ s a -> s{_gbwrsResponseStatus = a})

instance NFData GetBucketWebsiteResponse where
