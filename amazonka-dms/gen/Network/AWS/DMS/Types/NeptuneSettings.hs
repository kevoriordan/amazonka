{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.NeptuneSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.NeptuneSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines an Amazon Neptune endpoint.
--
--
--
-- /See:/ 'neptuneSettings' smart constructor.
data NeptuneSettings = NeptuneSettings'{_nsMaxFileSize
                                        :: !(Maybe Int),
                                        _nsMaxRetryCount :: !(Maybe Int),
                                        _nsServiceAccessRoleARN ::
                                        !(Maybe Text),
                                        _nsIAMAuthEnabled :: !(Maybe Bool),
                                        _nsErrorRetryDuration :: !(Maybe Int),
                                        _nsS3BucketName :: !Text,
                                        _nsS3BucketFolder :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NeptuneSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsMaxFileSize' - The maximum size in KB of migrated graph data stored in a CSV file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1048576 KB. If successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
--
-- * 'nsMaxRetryCount' - The number of times for AWS DMS to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 5.
--
-- * 'nsServiceAccessRoleARN' - The ARN of the service role you have created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole> in the /AWS Database Migration Service User Guide./ 
--
-- * 'nsIAMAuthEnabled' - If you want IAM authorization enabled for this endpoint, set this parameter to @true@ and attach the appropriate role policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
--
-- * 'nsErrorRetryDuration' - The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
--
-- * 'nsS3BucketName' - The name of the S3 bucket for AWS DMS to temporarily store migrated graph data in CSV files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these CSV files.
--
-- * 'nsS3BucketFolder' - A folder path where you where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@ 
neptuneSettings
    :: Text -- ^ 'nsS3BucketName'
    -> Text -- ^ 'nsS3BucketFolder'
    -> NeptuneSettings
neptuneSettings pS3BucketName_ pS3BucketFolder_
  = NeptuneSettings'{_nsMaxFileSize = Nothing,
                     _nsMaxRetryCount = Nothing,
                     _nsServiceAccessRoleARN = Nothing,
                     _nsIAMAuthEnabled = Nothing,
                     _nsErrorRetryDuration = Nothing,
                     _nsS3BucketName = pS3BucketName_,
                     _nsS3BucketFolder = pS3BucketFolder_}

-- | The maximum size in KB of migrated graph data stored in a CSV file before AWS DMS bulk-loads the data to the Neptune target database. The default is 1048576 KB. If successful, AWS DMS clears the bucket, ready to store the next batch of migrated graph data.
nsMaxFileSize :: Lens' NeptuneSettings (Maybe Int)
nsMaxFileSize = lens _nsMaxFileSize (\ s a -> s{_nsMaxFileSize = a})

-- | The number of times for AWS DMS to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 5.
nsMaxRetryCount :: Lens' NeptuneSettings (Maybe Int)
nsMaxRetryCount = lens _nsMaxRetryCount (\ s a -> s{_nsMaxRetryCount = a})

-- | The ARN of the service role you have created for the Neptune target endpoint. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole> in the /AWS Database Migration Service User Guide./ 
nsServiceAccessRoleARN :: Lens' NeptuneSettings (Maybe Text)
nsServiceAccessRoleARN = lens _nsServiceAccessRoleARN (\ s a -> s{_nsServiceAccessRoleARN = a})

-- | If you want IAM authorization enabled for this endpoint, set this parameter to @true@ and attach the appropriate role policy document to your service role specified by @ServiceAccessRoleArn@ . The default is @false@ .
nsIAMAuthEnabled :: Lens' NeptuneSettings (Maybe Bool)
nsIAMAuthEnabled = lens _nsIAMAuthEnabled (\ s a -> s{_nsIAMAuthEnabled = a})

-- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of migrated graph data to the Neptune target database before raising an error. The default is 250.
nsErrorRetryDuration :: Lens' NeptuneSettings (Maybe Int)
nsErrorRetryDuration = lens _nsErrorRetryDuration (\ s a -> s{_nsErrorRetryDuration = a})

-- | The name of the S3 bucket for AWS DMS to temporarily store migrated graph data in CSV files before bulk-loading it to the Neptune target database. AWS DMS maps the SQL source data to graph data before storing it in these CSV files.
nsS3BucketName :: Lens' NeptuneSettings Text
nsS3BucketName = lens _nsS3BucketName (\ s a -> s{_nsS3BucketName = a})

-- | A folder path where you where you want AWS DMS to store migrated graph data in the S3 bucket specified by @S3BucketName@ 
nsS3BucketFolder :: Lens' NeptuneSettings Text
nsS3BucketFolder = lens _nsS3BucketFolder (\ s a -> s{_nsS3BucketFolder = a})

instance FromJSON NeptuneSettings where
        parseJSON
          = withObject "NeptuneSettings"
              (\ x ->
                 NeptuneSettings' <$>
                   (x .:? "MaxFileSize") <*> (x .:? "MaxRetryCount") <*>
                     (x .:? "ServiceAccessRoleArn")
                     <*> (x .:? "IamAuthEnabled")
                     <*> (x .:? "ErrorRetryDuration")
                     <*> (x .: "S3BucketName")
                     <*> (x .: "S3BucketFolder"))

instance Hashable NeptuneSettings where

instance NFData NeptuneSettings where

instance ToJSON NeptuneSettings where
        toJSON NeptuneSettings'{..}
          = object
              (catMaybes
                 [("MaxFileSize" .=) <$> _nsMaxFileSize,
                  ("MaxRetryCount" .=) <$> _nsMaxRetryCount,
                  ("ServiceAccessRoleArn" .=) <$>
                    _nsServiceAccessRoleARN,
                  ("IamAuthEnabled" .=) <$> _nsIAMAuthEnabled,
                  ("ErrorRetryDuration" .=) <$> _nsErrorRetryDuration,
                  Just ("S3BucketName" .= _nsS3BucketName),
                  Just ("S3BucketFolder" .= _nsS3BucketFolder)])
