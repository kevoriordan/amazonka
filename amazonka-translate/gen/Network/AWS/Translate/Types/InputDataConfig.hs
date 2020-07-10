{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.InputDataConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.InputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input configuration properties for requesting a batch translation job.
--
--
--
-- /See:/ 'inputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'{_idcS3URI ::
                                        !Text,
                                        _idcContentType :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcS3URI' - The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
--
-- * 'idcContentType' - The multipurpose internet mail extension (MIME) type of the input files. Valid values are @text/plain@ for plaintext files and @text/html@ for HTML files.
inputDataConfig
    :: Text -- ^ 'idcS3URI'
    -> Text -- ^ 'idcContentType'
    -> InputDataConfig
inputDataConfig pS3URI_ pContentType_
  = InputDataConfig'{_idcS3URI = pS3URI_,
                     _idcContentType = pContentType_}

-- | The URI of the AWS S3 folder that contains the input file. The folder must be in the same Region as the API endpoint you are calling.
idcS3URI :: Lens' InputDataConfig Text
idcS3URI = lens _idcS3URI (\ s a -> s{_idcS3URI = a})

-- | The multipurpose internet mail extension (MIME) type of the input files. Valid values are @text/plain@ for plaintext files and @text/html@ for HTML files.
idcContentType :: Lens' InputDataConfig Text
idcContentType = lens _idcContentType (\ s a -> s{_idcContentType = a})

instance FromJSON InputDataConfig where
        parseJSON
          = withObject "InputDataConfig"
              (\ x ->
                 InputDataConfig' <$>
                   (x .: "S3Uri") <*> (x .: "ContentType"))

instance Hashable InputDataConfig where

instance NFData InputDataConfig where

instance ToJSON InputDataConfig where
        toJSON InputDataConfig'{..}
          = object
              (catMaybes
                 [Just ("S3Uri" .= _idcS3URI),
                  Just ("ContentType" .= _idcContentType)])
