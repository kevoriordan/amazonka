{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.Media
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.Media where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the input media file in a transcription request.
--
--
--
-- /See:/ 'media' smart constructor.
newtype Media = Media'{_mMediaFileURI :: Maybe Text}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Media' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mMediaFileURI' - The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @s3://<bucket-name>/<keyprefix>/<objectkey> @  For example: @s3://examplebucket/example.mp4@  @s3://examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
media
    :: Media
media = Media'{_mMediaFileURI = Nothing}

-- | The S3 object location of the input media file. The URI must be in the same region as the API endpoint that you are calling. The general form is: @s3://<bucket-name>/<keyprefix>/<objectkey> @  For example: @s3://examplebucket/example.mp4@  @s3://examplebucket/mediadocs/example.mp4@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ .
mMediaFileURI :: Lens' Media (Maybe Text)
mMediaFileURI = lens _mMediaFileURI (\ s a -> s{_mMediaFileURI = a})

instance FromJSON Media where
        parseJSON
          = withObject "Media"
              (\ x -> Media' <$> (x .:? "MediaFileUri"))

instance Hashable Media where

instance NFData Media where

instance ToJSON Media where
        toJSON Media'{..}
          = object
              (catMaybes [("MediaFileUri" .=) <$> _mMediaFileURI])
