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
-- Module      : Network.AWS.Transcribe.StartTranscriptionJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous job to transcribe speech to text. 
--
--
module Network.AWS.Transcribe.StartTranscriptionJob
    (
    -- * Creating a Request
      startTranscriptionJob
    , StartTranscriptionJob
    -- * Request Lenses
    , stjContentRedaction
    , stjSettings
    , stjOutputBucketName
    , stjMediaFormat
    , stjOutputEncryptionKMSKeyId
    , stjJobExecutionSettings
    , stjMediaSampleRateHertz
    , stjTranscriptionJobName
    , stjLanguageCode
    , stjMedia

    -- * Destructuring the Response
    , startTranscriptionJobResponse
    , StartTranscriptionJobResponse
    -- * Response Lenses
    , stjrsTranscriptionJob
    , stjrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Transcribe.Types
import Network.AWS.Transcribe.Types.Product

-- | /See:/ 'startTranscriptionJob' smart constructor.
data StartTranscriptionJob = StartTranscriptionJob'{_stjContentRedaction
                                                    ::
                                                    !(Maybe ContentRedaction),
                                                    _stjSettings ::
                                                    !(Maybe Settings),
                                                    _stjOutputBucketName ::
                                                    !(Maybe Text),
                                                    _stjMediaFormat ::
                                                    !(Maybe MediaFormat),
                                                    _stjOutputEncryptionKMSKeyId
                                                    :: !(Maybe Text),
                                                    _stjJobExecutionSettings ::
                                                    !(Maybe
                                                        JobExecutionSettings),
                                                    _stjMediaSampleRateHertz ::
                                                    !(Maybe Nat),
                                                    _stjTranscriptionJobName ::
                                                    !Text,
                                                    _stjLanguageCode ::
                                                    !LanguageCode,
                                                    _stjMedia :: !Media}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'StartTranscriptionJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjContentRedaction' - An object that contains the request parameters for content redaction.
--
-- * 'stjSettings' - A @Settings@ object that provides optional settings for a transcription job.
--
-- * 'stjOutputBucketName' - The location where the transcription is stored. If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> . You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket. If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
--
-- * 'stjMediaFormat' - The format of the input media file.
--
-- * 'stjOutputEncryptionKMSKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the @StartTranscriptionJob@ operation must have permission to use the specified KMS key. You can use either of the following to identify a KMS key in the current account:     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"     * KMS Key Alias: "alias/ExampleAlias" You can use either of the following to identify a KMS key in the current account or another account:     * Amazon Resource Name (ARN) of a KMS Key: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias" If you don't specify an encryption key, the output of the transcription job is encrypted with the default Amazon S3 key (SSE-S3).  If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
--
-- * 'stjJobExecutionSettings' - Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
--
-- * 'stjMediaSampleRateHertz' - The sample rate, in Hertz, of the audio track in the input media file.  If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
--
-- * 'stjTranscriptionJobName' - The name of the job. Note that you can't use the strings "." or ".." by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job you will receive a @ConflictException@ error.
--
-- * 'stjLanguageCode' - The language code for the language used in the input media file.
--
-- * 'stjMedia' - An object that describes the input media for a transcription job.
startTranscriptionJob
    :: Text -- ^ 'stjTranscriptionJobName'
    -> LanguageCode -- ^ 'stjLanguageCode'
    -> Media -- ^ 'stjMedia'
    -> StartTranscriptionJob
startTranscriptionJob pTranscriptionJobName_
  pLanguageCode_ pMedia_
  = StartTranscriptionJob'{_stjContentRedaction =
                             Nothing,
                           _stjSettings = Nothing,
                           _stjOutputBucketName = Nothing,
                           _stjMediaFormat = Nothing,
                           _stjOutputEncryptionKMSKeyId = Nothing,
                           _stjJobExecutionSettings = Nothing,
                           _stjMediaSampleRateHertz = Nothing,
                           _stjTranscriptionJobName = pTranscriptionJobName_,
                           _stjLanguageCode = pLanguageCode_,
                           _stjMedia = pMedia_}

-- | An object that contains the request parameters for content redaction.
stjContentRedaction :: Lens' StartTranscriptionJob (Maybe ContentRedaction)
stjContentRedaction = lens _stjContentRedaction (\ s a -> s{_stjContentRedaction = a})

-- | A @Settings@ object that provides optional settings for a transcription job.
stjSettings :: Lens' StartTranscriptionJob (Maybe Settings)
stjSettings = lens _stjSettings (\ s a -> s{_stjSettings = a})

-- | The location where the transcription is stored. If you set the @OutputBucketName@ , Amazon Transcribe puts the transcript in the specified S3 bucket. When you call the 'GetTranscriptionJob' operation, the operation returns this location in the @TranscriptFileUri@ field. If you enable content redaction, the redacted transcript appears in @RedactedTranscriptFileUri@ . If you enable content redaction and choose to output an unredacted transcript, that transcript's location still appears in the @TranscriptFileUri@ . The S3 bucket must have permissions that allow Amazon Transcribe to put files in the bucket. For more information, see <https://docs.aws.amazon.com/transcribe/latest/dg/security_iam_id-based-policy-examples.html#auth-role-iam-user Permissions Required for IAM User Roles> . You can specify an AWS Key Management Service (KMS) key to encrypt the output of your transcription using the @OutputEncryptionKMSKeyId@ parameter. If you don't specify a KMS key, Amazon Transcribe uses the default Amazon S3 key for server-side encryption of transcripts that are placed in your S3 bucket. If you don't set the @OutputBucketName@ , Amazon Transcribe generates a pre-signed URL, a shareable URL that provides secure access to your transcription, and returns it in the @TranscriptFileUri@ field. Use this URL to download the transcription.
stjOutputBucketName :: Lens' StartTranscriptionJob (Maybe Text)
stjOutputBucketName = lens _stjOutputBucketName (\ s a -> s{_stjOutputBucketName = a})

-- | The format of the input media file.
stjMediaFormat :: Lens' StartTranscriptionJob (Maybe MediaFormat)
stjMediaFormat = lens _stjMediaFormat (\ s a -> s{_stjMediaFormat = a})

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (KMS) key used to encrypt the output of the transcription job. The user calling the @StartTranscriptionJob@ operation must have permission to use the specified KMS key. You can use either of the following to identify a KMS key in the current account:     * KMS Key ID: "1234abcd-12ab-34cd-56ef-1234567890ab"     * KMS Key Alias: "alias/ExampleAlias" You can use either of the following to identify a KMS key in the current account or another account:     * Amazon Resource Name (ARN) of a KMS Key: "arn:aws:kms:region:account ID:key/1234abcd-12ab-34cd-56ef-1234567890ab"     * ARN of a KMS Key Alias: "arn:aws:kms:region:account ID:alias/ExampleAlias" If you don't specify an encryption key, the output of the transcription job is encrypted with the default Amazon S3 key (SSE-S3).  If you specify a KMS key to encrypt your output, you must also specify an output location in the @OutputBucketName@ parameter.
stjOutputEncryptionKMSKeyId :: Lens' StartTranscriptionJob (Maybe Text)
stjOutputEncryptionKMSKeyId = lens _stjOutputEncryptionKMSKeyId (\ s a -> s{_stjOutputEncryptionKMSKeyId = a})

-- | Provides information about how a transcription job is executed. Use this field to indicate that the job can be queued for deferred execution if the concurrency limit is reached and there are no slots available to immediately run the job.
stjJobExecutionSettings :: Lens' StartTranscriptionJob (Maybe JobExecutionSettings)
stjJobExecutionSettings = lens _stjJobExecutionSettings (\ s a -> s{_stjJobExecutionSettings = a})

-- | The sample rate, in Hertz, of the audio track in the input media file.  If you do not specify the media sample rate, Amazon Transcribe determines the sample rate. If you specify the sample rate, it must match the sample rate detected by Amazon Transcribe. In most cases, you should leave the @MediaSampleRateHertz@ field blank and let Amazon Transcribe determine the sample rate.
stjMediaSampleRateHertz :: Lens' StartTranscriptionJob (Maybe Natural)
stjMediaSampleRateHertz = lens _stjMediaSampleRateHertz (\ s a -> s{_stjMediaSampleRateHertz = a}) . mapping _Nat

-- | The name of the job. Note that you can't use the strings "." or ".." by themselves as the job name. The name must also be unique within an AWS account. If you try to create a transcription job with the same name as a previous transcription job you will receive a @ConflictException@ error.
stjTranscriptionJobName :: Lens' StartTranscriptionJob Text
stjTranscriptionJobName = lens _stjTranscriptionJobName (\ s a -> s{_stjTranscriptionJobName = a})

-- | The language code for the language used in the input media file.
stjLanguageCode :: Lens' StartTranscriptionJob LanguageCode
stjLanguageCode = lens _stjLanguageCode (\ s a -> s{_stjLanguageCode = a})

-- | An object that describes the input media for a transcription job.
stjMedia :: Lens' StartTranscriptionJob Media
stjMedia = lens _stjMedia (\ s a -> s{_stjMedia = a})

instance AWSRequest StartTranscriptionJob where
        type Rs StartTranscriptionJob =
             StartTranscriptionJobResponse
        request = postJSON transcribe
        response
          = receiveJSON
              (\ s h x ->
                 StartTranscriptionJobResponse' <$>
                   (x .?> "TranscriptionJob") <*> (pure (fromEnum s)))

instance Hashable StartTranscriptionJob where

instance NFData StartTranscriptionJob where

instance ToHeaders StartTranscriptionJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Transcribe.StartTranscriptionJob" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartTranscriptionJob where
        toJSON StartTranscriptionJob'{..}
          = object
              (catMaybes
                 [("ContentRedaction" .=) <$> _stjContentRedaction,
                  ("Settings" .=) <$> _stjSettings,
                  ("OutputBucketName" .=) <$> _stjOutputBucketName,
                  ("MediaFormat" .=) <$> _stjMediaFormat,
                  ("OutputEncryptionKMSKeyId" .=) <$>
                    _stjOutputEncryptionKMSKeyId,
                  ("JobExecutionSettings" .=) <$>
                    _stjJobExecutionSettings,
                  ("MediaSampleRateHertz" .=) <$>
                    _stjMediaSampleRateHertz,
                  Just
                    ("TranscriptionJobName" .= _stjTranscriptionJobName),
                  Just ("LanguageCode" .= _stjLanguageCode),
                  Just ("Media" .= _stjMedia)])

instance ToPath StartTranscriptionJob where
        toPath = const "/"

instance ToQuery StartTranscriptionJob where
        toQuery = const mempty

-- | /See:/ 'startTranscriptionJobResponse' smart constructor.
data StartTranscriptionJobResponse = StartTranscriptionJobResponse'{_stjrsTranscriptionJob
                                                                    ::
                                                                    !(Maybe
                                                                        TranscriptionJob),
                                                                    _stjrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'StartTranscriptionJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stjrsTranscriptionJob' - An object containing details of the asynchronous transcription job.
--
-- * 'stjrsResponseStatus' - -- | The response status code.
startTranscriptionJobResponse
    :: Int -- ^ 'stjrsResponseStatus'
    -> StartTranscriptionJobResponse
startTranscriptionJobResponse pResponseStatus_
  = StartTranscriptionJobResponse'{_stjrsTranscriptionJob
                                     = Nothing,
                                   _stjrsResponseStatus = pResponseStatus_}

-- | An object containing details of the asynchronous transcription job.
stjrsTranscriptionJob :: Lens' StartTranscriptionJobResponse (Maybe TranscriptionJob)
stjrsTranscriptionJob = lens _stjrsTranscriptionJob (\ s a -> s{_stjrsTranscriptionJob = a})

-- | -- | The response status code.
stjrsResponseStatus :: Lens' StartTranscriptionJobResponse Int
stjrsResponseStatus = lens _stjrsResponseStatus (\ s a -> s{_stjrsResponseStatus = a})

instance NFData StartTranscriptionJobResponse where
