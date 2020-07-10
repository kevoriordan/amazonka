{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.TranscriptionJobSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.TranscriptionJobSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Transcribe.Types.ContentRedaction
import Network.AWS.Transcribe.Types.LanguageCode
import Network.AWS.Transcribe.Types.OutputLocationType
import Network.AWS.Transcribe.Types.TranscriptionJobStatus

-- | Provides a summary of information about a transcription job.
--
--
--
-- /See:/ 'transcriptionJobSummary' smart constructor.
data TranscriptionJobSummary = TranscriptionJobSummary'{_tjsCreationTime
                                                        :: !(Maybe POSIX),
                                                        _tjsFailureReason ::
                                                        !(Maybe Text),
                                                        _tjsContentRedaction ::
                                                        !(Maybe
                                                            ContentRedaction),
                                                        _tjsLanguageCode ::
                                                        !(Maybe LanguageCode),
                                                        _tjsOutputLocationType
                                                        ::
                                                        !(Maybe
                                                            OutputLocationType),
                                                        _tjsStartTime ::
                                                        !(Maybe POSIX),
                                                        _tjsCompletionTime ::
                                                        !(Maybe POSIX),
                                                        _tjsTranscriptionJobStatus
                                                        ::
                                                        !(Maybe
                                                            TranscriptionJobStatus),
                                                        _tjsTranscriptionJobName
                                                        :: !(Maybe Text)}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'TranscriptionJobSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjsCreationTime' - A timestamp that shows when the job was created.
--
-- * 'tjsFailureReason' - If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
--
-- * 'tjsContentRedaction' - The content redaction settings of the transcription job.
--
-- * 'tjsLanguageCode' - The language code for the input speech.
--
-- * 'tjsOutputLocationType' - Indicates the location of the output of the transcription job. If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation. If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
--
-- * 'tjsStartTime' - A timestamp that shows when the job started processing.
--
-- * 'tjsCompletionTime' - A timestamp that shows when the job was completed.
--
-- * 'tjsTranscriptionJobStatus' - The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
--
-- * 'tjsTranscriptionJobName' - The name of the transcription job.
transcriptionJobSummary
    :: TranscriptionJobSummary
transcriptionJobSummary
  = TranscriptionJobSummary'{_tjsCreationTime =
                               Nothing,
                             _tjsFailureReason = Nothing,
                             _tjsContentRedaction = Nothing,
                             _tjsLanguageCode = Nothing,
                             _tjsOutputLocationType = Nothing,
                             _tjsStartTime = Nothing,
                             _tjsCompletionTime = Nothing,
                             _tjsTranscriptionJobStatus = Nothing,
                             _tjsTranscriptionJobName = Nothing}

-- | A timestamp that shows when the job was created.
tjsCreationTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCreationTime = lens _tjsCreationTime (\ s a -> s{_tjsCreationTime = a}) . mapping _Time

-- | If the @TranscriptionJobStatus@ field is @FAILED@ , a description of the error.
tjsFailureReason :: Lens' TranscriptionJobSummary (Maybe Text)
tjsFailureReason = lens _tjsFailureReason (\ s a -> s{_tjsFailureReason = a})

-- | The content redaction settings of the transcription job.
tjsContentRedaction :: Lens' TranscriptionJobSummary (Maybe ContentRedaction)
tjsContentRedaction = lens _tjsContentRedaction (\ s a -> s{_tjsContentRedaction = a})

-- | The language code for the input speech.
tjsLanguageCode :: Lens' TranscriptionJobSummary (Maybe LanguageCode)
tjsLanguageCode = lens _tjsLanguageCode (\ s a -> s{_tjsLanguageCode = a})

-- | Indicates the location of the output of the transcription job. If the value is @CUSTOMER_BUCKET@ then the location is the S3 bucket specified in the @outputBucketName@ field when the transcription job was started with the @StartTranscriptionJob@ operation. If the value is @SERVICE_BUCKET@ then the output is stored by Amazon Transcribe and can be retrieved using the URI in the @GetTranscriptionJob@ response's @TranscriptFileUri@ field.
tjsOutputLocationType :: Lens' TranscriptionJobSummary (Maybe OutputLocationType)
tjsOutputLocationType = lens _tjsOutputLocationType (\ s a -> s{_tjsOutputLocationType = a})

-- | A timestamp that shows when the job started processing.
tjsStartTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsStartTime = lens _tjsStartTime (\ s a -> s{_tjsStartTime = a}) . mapping _Time

-- | A timestamp that shows when the job was completed.
tjsCompletionTime :: Lens' TranscriptionJobSummary (Maybe UTCTime)
tjsCompletionTime = lens _tjsCompletionTime (\ s a -> s{_tjsCompletionTime = a}) . mapping _Time

-- | The status of the transcription job. When the status is @COMPLETED@ , use the @GetTranscriptionJob@ operation to get the results of the transcription.
tjsTranscriptionJobStatus :: Lens' TranscriptionJobSummary (Maybe TranscriptionJobStatus)
tjsTranscriptionJobStatus = lens _tjsTranscriptionJobStatus (\ s a -> s{_tjsTranscriptionJobStatus = a})

-- | The name of the transcription job.
tjsTranscriptionJobName :: Lens' TranscriptionJobSummary (Maybe Text)
tjsTranscriptionJobName = lens _tjsTranscriptionJobName (\ s a -> s{_tjsTranscriptionJobName = a})

instance FromJSON TranscriptionJobSummary where
        parseJSON
          = withObject "TranscriptionJobSummary"
              (\ x ->
                 TranscriptionJobSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "FailureReason")
                     <*> (x .:? "ContentRedaction")
                     <*> (x .:? "LanguageCode")
                     <*> (x .:? "OutputLocationType")
                     <*> (x .:? "StartTime")
                     <*> (x .:? "CompletionTime")
                     <*> (x .:? "TranscriptionJobStatus")
                     <*> (x .:? "TranscriptionJobName"))

instance Hashable TranscriptionJobSummary where

instance NFData TranscriptionJobSummary where
