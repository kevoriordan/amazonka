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
-- Module      : Network.AWS.S3.GetObjectTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag-set of an object. You send the GET request against the tagging subresource associated with the object.
--
--
-- To use this operation, you must have permission to perform the @s3:GetObjectTagging@ action. By default, the GET operation returns information about current version of an object. For a versioned bucket, you can have multiple versions of an object in your bucket. To retrieve tags of any other version, use the versionId query parameter. You also need permission for the @s3:GetObjectVersionTagging@ action.
--
-- By default, the bucket owner has this permission and can grant this permission to others.
--
-- For information about the Amazon S3 object tagging feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging> .
--
-- The following operation is related to @GetObjectTagging@ :
--
--     * 'PutObjectTagging' 
--
--
--
module Network.AWS.S3.GetObjectTagging
    (
    -- * Creating a Request
      getObjectTagging
    , GetObjectTagging
    -- * Request Lenses
    , gtobjcttggngVersionId
    , gtobjcttggngBucket
    , gtobjcttggngKey

    -- * Destructuring the Response
    , getObjectTaggingResponse
    , GetObjectTaggingResponse
    -- * Response Lenses
    , gotrsVersionId
    , gotrsResponseStatus
    , gotrsTagSet
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectTagging' smart constructor.
data GetObjectTagging = GetObjectTagging'{_gtobjcttggngVersionId
                                          :: !(Maybe ObjectVersionId),
                                          _gtobjcttggngBucket :: !BucketName,
                                          _gtobjcttggngKey :: !ObjectKey}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtobjcttggngVersionId' - The versionId of the object for which to get the tagging information.
--
-- * 'gtobjcttggngBucket' - The bucket name containing the object for which to get the tagging information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation using an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'gtobjcttggngKey' - Object key for which to get the tagging information.
getObjectTagging
    :: BucketName -- ^ 'gtobjcttggngBucket'
    -> ObjectKey -- ^ 'gtobjcttggngKey'
    -> GetObjectTagging
getObjectTagging pBucket_ pKey_
  = GetObjectTagging'{_gtobjcttggngVersionId = Nothing,
                      _gtobjcttggngBucket = pBucket_,
                      _gtobjcttggngKey = pKey_}

-- | The versionId of the object for which to get the tagging information.
gtobjcttggngVersionId :: Lens' GetObjectTagging (Maybe ObjectVersionId)
gtobjcttggngVersionId = lens _gtobjcttggngVersionId (\ s a -> s{_gtobjcttggngVersionId = a})

-- | The bucket name containing the object for which to get the tagging information.  When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation using an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
gtobjcttggngBucket :: Lens' GetObjectTagging BucketName
gtobjcttggngBucket = lens _gtobjcttggngBucket (\ s a -> s{_gtobjcttggngBucket = a})

-- | Object key for which to get the tagging information.
gtobjcttggngKey :: Lens' GetObjectTagging ObjectKey
gtobjcttggngKey = lens _gtobjcttggngKey (\ s a -> s{_gtobjcttggngKey = a})

instance AWSRequest GetObjectTagging where
        type Rs GetObjectTagging = GetObjectTaggingResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetObjectTaggingResponse' <$>
                   (h .#? "x-amz-version-id") <*> (pure (fromEnum s))
                     <*>
                     (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag"))

instance Hashable GetObjectTagging where

instance NFData GetObjectTagging where

instance ToHeaders GetObjectTagging where
        toHeaders = const mempty

instance ToPath GetObjectTagging where
        toPath GetObjectTagging'{..}
          = mconcat
              ["/", toBS _gtobjcttggngBucket, "/",
               toBS _gtobjcttggngKey]

instance ToQuery GetObjectTagging where
        toQuery GetObjectTagging'{..}
          = mconcat
              ["versionId" =: _gtobjcttggngVersionId, "tagging"]

-- | /See:/ 'getObjectTaggingResponse' smart constructor.
data GetObjectTaggingResponse = GetObjectTaggingResponse'{_gotrsVersionId
                                                          ::
                                                          !(Maybe
                                                              ObjectVersionId),
                                                          _gotrsResponseStatus
                                                          :: !Int,
                                                          _gotrsTagSet ::
                                                          ![Tag]}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'GetObjectTaggingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gotrsVersionId' - The versionId of the object for which you got the tagging information.
--
-- * 'gotrsResponseStatus' - -- | The response status code.
--
-- * 'gotrsTagSet' - Contains the tag set.
getObjectTaggingResponse
    :: Int -- ^ 'gotrsResponseStatus'
    -> GetObjectTaggingResponse
getObjectTaggingResponse pResponseStatus_
  = GetObjectTaggingResponse'{_gotrsVersionId =
                                Nothing,
                              _gotrsResponseStatus = pResponseStatus_,
                              _gotrsTagSet = mempty}

-- | The versionId of the object for which you got the tagging information.
gotrsVersionId :: Lens' GetObjectTaggingResponse (Maybe ObjectVersionId)
gotrsVersionId = lens _gotrsVersionId (\ s a -> s{_gotrsVersionId = a})

-- | -- | The response status code.
gotrsResponseStatus :: Lens' GetObjectTaggingResponse Int
gotrsResponseStatus = lens _gotrsResponseStatus (\ s a -> s{_gotrsResponseStatus = a})

-- | Contains the tag set.
gotrsTagSet :: Lens' GetObjectTaggingResponse [Tag]
gotrsTagSet = lens _gotrsTagSet (\ s a -> s{_gotrsTagSet = a}) . _Coerce

instance NFData GetObjectTaggingResponse where
