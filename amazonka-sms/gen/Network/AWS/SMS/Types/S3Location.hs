{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.S3Location
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Location of the Amazon S3 object in the customer's account.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'{_slBucket ::
                              !(Maybe Text),
                              _slKey :: !(Maybe Text)}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - Amazon S3 bucket name.
--
-- * 'slKey' - Amazon S3 bucket key.
s3Location
    :: S3Location
s3Location
  = S3Location'{_slBucket = Nothing, _slKey = Nothing}

-- | Amazon S3 bucket name.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a})

-- | Amazon S3 bucket key.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a})

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$> (x .:? "bucket") <*> (x .:? "key"))

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("bucket" .=) <$> _slBucket, ("key" .=) <$> _slKey])
