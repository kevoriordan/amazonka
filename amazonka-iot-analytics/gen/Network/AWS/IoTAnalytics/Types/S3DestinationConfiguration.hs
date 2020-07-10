{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration where

import Network.AWS.IoTAnalytics.Types.GlueConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information for delivery of data set contents to Amazon S3.
--
--
--
-- /See:/ 's3DestinationConfiguration' smart constructor.
data S3DestinationConfiguration = S3DestinationConfiguration'{_sdcGlueConfiguration
                                                              ::
                                                              !(Maybe
                                                                  GlueConfiguration),
                                                              _sdcBucket ::
                                                              !Text,
                                                              _sdcKey :: !Text,
                                                              _sdcRoleARN ::
                                                              !Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'S3DestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcGlueConfiguration' - Configuration information for coordination with the AWS Glue ETL (extract, transform and load) service.
--
-- * 'sdcBucket' - The name of the Amazon S3 bucket to which data set contents are delivered.
--
-- * 'sdcKey' - The key of the data set contents object. Each object in an Amazon S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). To produce a unique key, you can use "!{iotanalytics:scheduledTime}" to insert the time of the scheduled SQL query run, or "!{iotanalytics:versioned} to insert a unique hash identifying the data set, for example: "/DataSet/!{iotanalytics:scheduledTime}/!{iotanalytics:versioned}.csv".
--
-- * 'sdcRoleARN' - The ARN of the role which grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
s3DestinationConfiguration
    :: Text -- ^ 'sdcBucket'
    -> Text -- ^ 'sdcKey'
    -> Text -- ^ 'sdcRoleARN'
    -> S3DestinationConfiguration
s3DestinationConfiguration pBucket_ pKey_ pRoleARN_
  = S3DestinationConfiguration'{_sdcGlueConfiguration =
                                  Nothing,
                                _sdcBucket = pBucket_, _sdcKey = pKey_,
                                _sdcRoleARN = pRoleARN_}

-- | Configuration information for coordination with the AWS Glue ETL (extract, transform and load) service.
sdcGlueConfiguration :: Lens' S3DestinationConfiguration (Maybe GlueConfiguration)
sdcGlueConfiguration = lens _sdcGlueConfiguration (\ s a -> s{_sdcGlueConfiguration = a})

-- | The name of the Amazon S3 bucket to which data set contents are delivered.
sdcBucket :: Lens' S3DestinationConfiguration Text
sdcBucket = lens _sdcBucket (\ s a -> s{_sdcBucket = a})

-- | The key of the data set contents object. Each object in an Amazon S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). To produce a unique key, you can use "!{iotanalytics:scheduledTime}" to insert the time of the scheduled SQL query run, or "!{iotanalytics:versioned} to insert a unique hash identifying the data set, for example: "/DataSet/!{iotanalytics:scheduledTime}/!{iotanalytics:versioned}.csv".
sdcKey :: Lens' S3DestinationConfiguration Text
sdcKey = lens _sdcKey (\ s a -> s{_sdcKey = a})

-- | The ARN of the role which grants AWS IoT Analytics permission to interact with your Amazon S3 and AWS Glue resources.
sdcRoleARN :: Lens' S3DestinationConfiguration Text
sdcRoleARN = lens _sdcRoleARN (\ s a -> s{_sdcRoleARN = a})

instance FromJSON S3DestinationConfiguration where
        parseJSON
          = withObject "S3DestinationConfiguration"
              (\ x ->
                 S3DestinationConfiguration' <$>
                   (x .:? "glueConfiguration") <*> (x .: "bucket") <*>
                     (x .: "key")
                     <*> (x .: "roleArn"))

instance Hashable S3DestinationConfiguration where

instance NFData S3DestinationConfiguration where

instance ToJSON S3DestinationConfiguration where
        toJSON S3DestinationConfiguration'{..}
          = object
              (catMaybes
                 [("glueConfiguration" .=) <$> _sdcGlueConfiguration,
                  Just ("bucket" .= _sdcBucket),
                  Just ("key" .= _sdcKey),
                  Just ("roleArn" .= _sdcRoleARN)])
