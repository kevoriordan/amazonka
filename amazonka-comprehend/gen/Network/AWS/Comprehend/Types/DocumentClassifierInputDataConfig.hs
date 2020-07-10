{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input properties for training a document classifier. 
--
--
-- For more information on how the input file is formatted, see 'how-document-classification-training-data' . 
--
--
-- /See:/ 'documentClassifierInputDataConfig' smart constructor.
data DocumentClassifierInputDataConfig = DocumentClassifierInputDataConfig'{_dcidcLabelDelimiter
                                                                            ::
                                                                            !(Maybe
                                                                                Text),
                                                                            _dcidcS3URI
                                                                            ::
                                                                            !Text}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'DocumentClassifierInputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcidcLabelDelimiter' - Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
--
-- * 'dcidcS3URI' - The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files. For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
documentClassifierInputDataConfig
    :: Text -- ^ 'dcidcS3URI'
    -> DocumentClassifierInputDataConfig
documentClassifierInputDataConfig pS3URI_
  = DocumentClassifierInputDataConfig'{_dcidcLabelDelimiter
                                         = Nothing,
                                       _dcidcS3URI = pS3URI_}

-- | Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
dcidcLabelDelimiter :: Lens' DocumentClassifierInputDataConfig (Maybe Text)
dcidcLabelDelimiter = lens _dcidcLabelDelimiter (\ s a -> s{_dcidcLabelDelimiter = a})

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files. For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
dcidcS3URI :: Lens' DocumentClassifierInputDataConfig Text
dcidcS3URI = lens _dcidcS3URI (\ s a -> s{_dcidcS3URI = a})

instance FromJSON DocumentClassifierInputDataConfig
         where
        parseJSON
          = withObject "DocumentClassifierInputDataConfig"
              (\ x ->
                 DocumentClassifierInputDataConfig' <$>
                   (x .:? "LabelDelimiter") <*> (x .: "S3Uri"))

instance Hashable DocumentClassifierInputDataConfig
         where

instance NFData DocumentClassifierInputDataConfig
         where

instance ToJSON DocumentClassifierInputDataConfig
         where
        toJSON DocumentClassifierInputDataConfig'{..}
          = object
              (catMaybes
                 [("LabelDelimiter" .=) <$> _dcidcLabelDelimiter,
                  Just ("S3Uri" .= _dcidcS3URI)])
