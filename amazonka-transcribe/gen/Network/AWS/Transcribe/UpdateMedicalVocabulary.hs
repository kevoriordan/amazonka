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
-- Module      : Network.AWS.Transcribe.UpdateMedicalVocabulary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing vocabulary with new values in a different text file. The @UpdateMedicalVocabulary@ operation overwrites all of the existing information with the values that you provide in the request.
--
--
module Network.AWS.Transcribe.UpdateMedicalVocabulary
    (
    -- * Creating a Request
      updateMedicalVocabulary
    , UpdateMedicalVocabulary
    -- * Request Lenses
    , umvVocabularyFileURI
    , umvVocabularyName
    , umvLanguageCode

    -- * Destructuring the Response
    , updateMedicalVocabularyResponse
    , UpdateMedicalVocabularyResponse
    -- * Response Lenses
    , umvrsLanguageCode
    , umvrsVocabularyName
    , umvrsLastModifiedTime
    , umvrsVocabularyState
    , umvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'updateMedicalVocabulary' smart constructor.
data UpdateMedicalVocabulary = UpdateMedicalVocabulary'{_umvVocabularyFileURI
                                                        :: !(Maybe Text),
                                                        _umvVocabularyName ::
                                                        !Text,
                                                        _umvLanguageCode ::
                                                        !LanguageCode}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'UpdateMedicalVocabulary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umvVocabularyFileURI' - The Amazon S3 location of the text file containing the definition of the custom vocabulary. The URI must be in the same AWS region as the API endpoint you are calling. You can see the fields you need to enter for you Amazon S3 location in the example URI here: @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3.us-east-1.amazonaws.com/examplebucket/vocab.txt@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
--
-- * 'umvVocabularyName' - The name of the vocabulary to update. The name is case-sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
--
-- * 'umvLanguageCode' - The language code of the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
updateMedicalVocabulary
    :: Text -- ^ 'umvVocabularyName'
    -> LanguageCode -- ^ 'umvLanguageCode'
    -> UpdateMedicalVocabulary
updateMedicalVocabulary pVocabularyName_
  pLanguageCode_
  = UpdateMedicalVocabulary'{_umvVocabularyFileURI =
                               Nothing,
                             _umvVocabularyName = pVocabularyName_,
                             _umvLanguageCode = pLanguageCode_}

-- | The Amazon S3 location of the text file containing the definition of the custom vocabulary. The URI must be in the same AWS region as the API endpoint you are calling. You can see the fields you need to enter for you Amazon S3 location in the example URI here: @https://s3.<aws-region>.amazonaws.com/<bucket-name>/<keyprefix>/<objectkey> @  For example: @https://s3.us-east-1.amazonaws.com/examplebucket/vocab.txt@  For more information about S3 object names, see <http://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMetadata.html#object-keys Object Keys> in the /Amazon S3 Developer Guide/ . For more information about custom vocabularies in Amazon Transcribe Medical, see <http://docs.aws.amazon.com/transcribe/latest/dg/how-it-works.html#how-vocabulary Medical Custom Vocabularies> .
umvVocabularyFileURI :: Lens' UpdateMedicalVocabulary (Maybe Text)
umvVocabularyFileURI = lens _umvVocabularyFileURI (\ s a -> s{_umvVocabularyFileURI = a})

-- | The name of the vocabulary to update. The name is case-sensitive. If you try to update a vocabulary with the same name as a previous vocabulary you will receive a @ConflictException@ error.
umvVocabularyName :: Lens' UpdateMedicalVocabulary Text
umvVocabularyName = lens _umvVocabularyName (\ s a -> s{_umvVocabularyName = a})

-- | The language code of the entries in the updated vocabulary. US English (en-US) is the only valid language code in Amazon Transcribe Medical.
umvLanguageCode :: Lens' UpdateMedicalVocabulary LanguageCode
umvLanguageCode = lens _umvLanguageCode (\ s a -> s{_umvLanguageCode = a})

instance AWSRequest UpdateMedicalVocabulary where
        type Rs UpdateMedicalVocabulary =
             UpdateMedicalVocabularyResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 UpdateMedicalVocabularyResponse' <$>
                   (x .?> "LanguageCode") <*> (x .?> "VocabularyName")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "VocabularyState")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateMedicalVocabulary where

instance NFData UpdateMedicalVocabulary where

instance ToHeaders UpdateMedicalVocabulary where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.UpdateMedicalVocabulary" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateMedicalVocabulary where
        toJSON UpdateMedicalVocabulary'{..}
          = object
              (catMaybes
                 [("VocabularyFileUri" .=) <$> _umvVocabularyFileURI,
                  Just ("VocabularyName" .= _umvVocabularyName),
                  Just ("LanguageCode" .= _umvLanguageCode)])

instance ToPath UpdateMedicalVocabulary where
        toPath = const "/"

instance ToQuery UpdateMedicalVocabulary where
        toQuery = const mempty

-- | /See:/ 'updateMedicalVocabularyResponse' smart constructor.
data UpdateMedicalVocabularyResponse = UpdateMedicalVocabularyResponse'{_umvrsLanguageCode
                                                                        ::
                                                                        !(Maybe
                                                                            LanguageCode),
                                                                        _umvrsVocabularyName
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _umvrsLastModifiedTime
                                                                        ::
                                                                        !(Maybe
                                                                            POSIX),
                                                                        _umvrsVocabularyState
                                                                        ::
                                                                        !(Maybe
                                                                            VocabularyState),
                                                                        _umvrsResponseStatus
                                                                        :: !Int}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'UpdateMedicalVocabularyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umvrsLanguageCode' - The language code for the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
--
-- * 'umvrsVocabularyName' - The name of the updated vocabulary.
--
-- * 'umvrsLastModifiedTime' - The date and time the vocabulary was updated.
--
-- * 'umvrsVocabularyState' - The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
--
-- * 'umvrsResponseStatus' - -- | The response status code.
updateMedicalVocabularyResponse
    :: Int -- ^ 'umvrsResponseStatus'
    -> UpdateMedicalVocabularyResponse
updateMedicalVocabularyResponse pResponseStatus_
  = UpdateMedicalVocabularyResponse'{_umvrsLanguageCode
                                       = Nothing,
                                     _umvrsVocabularyName = Nothing,
                                     _umvrsLastModifiedTime = Nothing,
                                     _umvrsVocabularyState = Nothing,
                                     _umvrsResponseStatus = pResponseStatus_}

-- | The language code for the text file used to update the custom vocabulary. US English (en-US) is the only language supported in Amazon Transcribe Medical.
umvrsLanguageCode :: Lens' UpdateMedicalVocabularyResponse (Maybe LanguageCode)
umvrsLanguageCode = lens _umvrsLanguageCode (\ s a -> s{_umvrsLanguageCode = a})

-- | The name of the updated vocabulary.
umvrsVocabularyName :: Lens' UpdateMedicalVocabularyResponse (Maybe Text)
umvrsVocabularyName = lens _umvrsVocabularyName (\ s a -> s{_umvrsVocabularyName = a})

-- | The date and time the vocabulary was updated.
umvrsLastModifiedTime :: Lens' UpdateMedicalVocabularyResponse (Maybe UTCTime)
umvrsLastModifiedTime = lens _umvrsLastModifiedTime (\ s a -> s{_umvrsLastModifiedTime = a}) . mapping _Time

-- | The processing state of the update to the vocabulary. When the @VocabularyState@ field is @READY@ the vocabulary is ready to be used in a @StartMedicalTranscriptionJob@ request.
umvrsVocabularyState :: Lens' UpdateMedicalVocabularyResponse (Maybe VocabularyState)
umvrsVocabularyState = lens _umvrsVocabularyState (\ s a -> s{_umvrsVocabularyState = a})

-- | -- | The response status code.
umvrsResponseStatus :: Lens' UpdateMedicalVocabularyResponse Int
umvrsResponseStatus = lens _umvrsResponseStatus (\ s a -> s{_umvrsResponseStatus = a})

instance NFData UpdateMedicalVocabularyResponse where
