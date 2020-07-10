{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.CustomerManagedChannelS3Storage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use this to store channel data in an S3 bucket that you manage. If customer managed storage is selected, the "retentionPeriod" parameter is ignored. The choice of service-managed or customer-managed S3 storage cannot be changed after creation of the channel.
--
--
--
-- /See:/ 'customerManagedChannelS3Storage' smart constructor.
data CustomerManagedChannelS3Storage = CustomerManagedChannelS3Storage'{_cmcssKeyPrefix
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _cmcssBucket
                                                                        ::
                                                                        !Text,
                                                                        _cmcssRoleARN
                                                                        ::
                                                                        !Text}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'CustomerManagedChannelS3Storage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmcssKeyPrefix' - [Optional] The prefix used to create the keys of the channel data objects. Each object in an Amazon S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a '/'.
--
-- * 'cmcssBucket' - The name of the Amazon S3 bucket in which channel data is stored.
--
-- * 'cmcssRoleARN' - The ARN of the role which grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
customerManagedChannelS3Storage
    :: Text -- ^ 'cmcssBucket'
    -> Text -- ^ 'cmcssRoleARN'
    -> CustomerManagedChannelS3Storage
customerManagedChannelS3Storage pBucket_ pRoleARN_
  = CustomerManagedChannelS3Storage'{_cmcssKeyPrefix =
                                       Nothing,
                                     _cmcssBucket = pBucket_,
                                     _cmcssRoleARN = pRoleARN_}

-- | [Optional] The prefix used to create the keys of the channel data objects. Each object in an Amazon S3 bucket has a key that is its unique identifier within the bucket (each object in a bucket has exactly one key). The prefix must end with a '/'.
cmcssKeyPrefix :: Lens' CustomerManagedChannelS3Storage (Maybe Text)
cmcssKeyPrefix = lens _cmcssKeyPrefix (\ s a -> s{_cmcssKeyPrefix = a})

-- | The name of the Amazon S3 bucket in which channel data is stored.
cmcssBucket :: Lens' CustomerManagedChannelS3Storage Text
cmcssBucket = lens _cmcssBucket (\ s a -> s{_cmcssBucket = a})

-- | The ARN of the role which grants AWS IoT Analytics permission to interact with your Amazon S3 resources.
cmcssRoleARN :: Lens' CustomerManagedChannelS3Storage Text
cmcssRoleARN = lens _cmcssRoleARN (\ s a -> s{_cmcssRoleARN = a})

instance FromJSON CustomerManagedChannelS3Storage
         where
        parseJSON
          = withObject "CustomerManagedChannelS3Storage"
              (\ x ->
                 CustomerManagedChannelS3Storage' <$>
                   (x .:? "keyPrefix") <*> (x .: "bucket") <*>
                     (x .: "roleArn"))

instance Hashable CustomerManagedChannelS3Storage
         where

instance NFData CustomerManagedChannelS3Storage where

instance ToJSON CustomerManagedChannelS3Storage where
        toJSON CustomerManagedChannelS3Storage'{..}
          = object
              (catMaybes
                 [("keyPrefix" .=) <$> _cmcssKeyPrefix,
                  Just ("bucket" .= _cmcssBucket),
                  Just ("roleArn" .= _cmcssRoleARN)])
