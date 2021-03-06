{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types
    (
    -- * Service Configuration
      polly

    -- * Errors
    , _LexiconSizeExceededException
    , _InvalidS3BucketException
    , _SynthesisTaskNotFoundException
    , _TextLengthExceededException
    , _EngineNotSupportedException
    , _UnsupportedPlsLanguageException
    , _InvalidSNSTopicARNException
    , _InvalidS3KeyException
    , _LanguageNotSupportedException
    , _InvalidTaskIdException
    , _MaxLexemeLengthExceededException
    , _SsmlMarksNotSupportedForTextTypeException
    , _MarksNotSupportedForFormatException
    , _MaxLexiconsNumberExceededException
    , _InvalidNextTokenException
    , _UnsupportedPlsAlphabetException
    , _ServiceFailureException
    , _InvalidSampleRateException
    , _InvalidSsmlException
    , _InvalidLexiconException
    , _LexiconNotFoundException

    -- * Engine
    , Engine (..)

    -- * Gender
    , Gender (..)

    -- * LanguageCode
    , LanguageCode (..)

    -- * OutputFormat
    , OutputFormat (..)

    -- * SpeechMarkType
    , SpeechMarkType (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * TextType
    , TextType (..)

    -- * VoiceId
    , VoiceId (..)

    -- * Lexicon
    , Lexicon
    , lexicon
    , lContent
    , lName

    -- * LexiconAttributes
    , LexiconAttributes
    , lexiconAttributes
    , laLanguageCode
    , laSize
    , laLexemesCount
    , laLexiconARN
    , laAlphabet
    , laLastModified

    -- * LexiconDescription
    , LexiconDescription
    , lexiconDescription
    , ldAttributes
    , ldName

    -- * SynthesisTask
    , SynthesisTask
    , synthesisTask
    , stCreationTime
    , stLanguageCode
    , stSNSTopicARN
    , stTaskStatusReason
    , stTaskId
    , stRequestCharacters
    , stEngine
    , stSpeechMarkTypes
    , stSampleRate
    , stOutputFormat
    , stTextType
    , stVoiceId
    , stLexiconNames
    , stTaskStatus
    , stOutputURI

    -- * Voice
    , Voice
    , voice
    , vLanguageCode
    , vLanguageName
    , vGender
    , vName
    , vId
    , vAdditionalLanguageCodes
    , vSupportedEngines
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Polly.Types.Engine
import Network.AWS.Polly.Types.Gender
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Polly.Types.OutputFormat
import Network.AWS.Polly.Types.SpeechMarkType
import Network.AWS.Polly.Types.TaskStatus
import Network.AWS.Polly.Types.TextType
import Network.AWS.Polly.Types.VoiceId
import Network.AWS.Polly.Types.Lexicon
import Network.AWS.Polly.Types.LexiconAttributes
import Network.AWS.Polly.Types.LexiconDescription
import Network.AWS.Polly.Types.SynthesisTask
import Network.AWS.Polly.Types.Voice

-- | API version @2016-06-10@ of the Amazon Polly SDK configuration.
polly :: Service
polly
  = Service{_svcAbbrev = "Polly", _svcSigner = v4,
            _svcPrefix = "polly", _svcVersion = "2016-06-10",
            _svcEndpoint = defaultEndpoint polly,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Polly",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The maximum size of the specified lexicon would be exceeded by this operation.
--
--
_LexiconSizeExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LexiconSizeExceededException
  = _MatchServiceError polly
      "LexiconSizeExceededException"
      . hasStatus 400

-- | The provided Amazon S3 bucket name is invalid. Please check your input with S3 bucket naming requirements and try again.
--
--
_InvalidS3BucketException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3BucketException
  = _MatchServiceError polly "InvalidS3BucketException"
      . hasStatus 400

-- | The Speech Synthesis task with requested Task ID cannot be found.
--
--
_SynthesisTaskNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_SynthesisTaskNotFoundException
  = _MatchServiceError polly
      "SynthesisTaskNotFoundException"
      . hasStatus 400

-- | The value of the "Text" parameter is longer than the accepted limits. For the @SynthesizeSpeech@ API, the limit for input text is a maximum of 6000 characters total, of which no more than 3000 can be billed characters. For the @StartSpeechSynthesisTask@ API, the maximum is 200,000 characters, of which no more than 100,000 can be billed characters. SSML tags are not counted as billed characters.
--
--
_TextLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextLengthExceededException
  = _MatchServiceError polly
      "TextLengthExceededException"
      . hasStatus 400

-- | This engine is not compatible with the voice that you have designated. Choose a new voice that is compatible with the engine or change the engine and restart the operation.
--
--
_EngineNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_EngineNotSupportedException
  = _MatchServiceError polly
      "EngineNotSupportedException"
      . hasStatus 400

-- | The language specified in the lexicon is unsupported. For a list of supported languages, see <https://docs.aws.amazon.com/polly/latest/dg/API_LexiconAttributes.html Lexicon Attributes> .
--
--
_UnsupportedPlsLanguageException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlsLanguageException
  = _MatchServiceError polly
      "UnsupportedPlsLanguageException"
      . hasStatus 400

-- | The provided SNS topic ARN is invalid. Please provide a valid SNS topic ARN and try again.
--
--
_InvalidSNSTopicARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException
  = _MatchServiceError polly
      "InvalidSnsTopicArnException"
      . hasStatus 400

-- | The provided Amazon S3 key prefix is invalid. Please provide a valid S3 object key name.
--
--
_InvalidS3KeyException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyException
  = _MatchServiceError polly "InvalidS3KeyException" .
      hasStatus 400

-- | The language specified is not currently supported by Amazon Polly in this capacity.
--
--
_LanguageNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_LanguageNotSupportedException
  = _MatchServiceError polly
      "LanguageNotSupportedException"
      . hasStatus 400

-- | The provided Task ID is not valid. Please provide a valid Task ID and try again.
--
--
_InvalidTaskIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTaskIdException
  = _MatchServiceError polly "InvalidTaskIdException" .
      hasStatus 400

-- | The maximum size of the lexeme would be exceeded by this operation.
--
--
_MaxLexemeLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxLexemeLengthExceededException
  = _MatchServiceError polly
      "MaxLexemeLengthExceededException"
      . hasStatus 400

-- | SSML speech marks are not supported for plain text-type input.
--
--
_SsmlMarksNotSupportedForTextTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_SsmlMarksNotSupportedForTextTypeException
  = _MatchServiceError polly
      "SsmlMarksNotSupportedForTextTypeException"
      . hasStatus 400

-- | Speech marks are not supported for the @OutputFormat@ selected. Speech marks are only available for content in @json@ format.
--
--
_MarksNotSupportedForFormatException :: AsError a => Getting (First ServiceError) a ServiceError
_MarksNotSupportedForFormatException
  = _MatchServiceError polly
      "MarksNotSupportedForFormatException"
      . hasStatus 400

-- | The maximum number of lexicons would be exceeded by this operation.
--
--
_MaxLexiconsNumberExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxLexiconsNumberExceededException
  = _MatchServiceError polly
      "MaxLexiconsNumberExceededException"
      . hasStatus 400

-- | The NextToken is invalid. Verify that it's spelled correctly, and then try again.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException
  = _MatchServiceError polly
      "InvalidNextTokenException"
      . hasStatus 400

-- | The alphabet specified by the lexicon is not a supported alphabet. Valid values are @x-sampa@ and @ipa@ .
--
--
_UnsupportedPlsAlphabetException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedPlsAlphabetException
  = _MatchServiceError polly
      "UnsupportedPlsAlphabetException"
      . hasStatus 400

-- | An unknown condition has caused a service failure.
--
--
_ServiceFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceFailureException
  = _MatchServiceError polly "ServiceFailureException"
      . hasStatus 500

-- | The specified sample rate is not valid.
--
--
_InvalidSampleRateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSampleRateException
  = _MatchServiceError polly
      "InvalidSampleRateException"
      . hasStatus 400

-- | The SSML you provided is invalid. Verify the SSML syntax, spelling of tags and values, and then try again.
--
--
_InvalidSsmlException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSsmlException
  = _MatchServiceError polly "InvalidSsmlException" .
      hasStatus 400

-- | Amazon Polly can't find the specified lexicon. Verify that the lexicon's name is spelled correctly, and then try again.
--
--
_InvalidLexiconException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLexiconException
  = _MatchServiceError polly "InvalidLexiconException"
      . hasStatus 400

-- | Amazon Polly can't find the specified lexicon. This could be caused by a lexicon that is missing, its name is misspelled or specifying a lexicon that is in a different region.
--
--
-- Verify that the lexicon exists, is in the region (see 'ListLexicons' ) and that you spelled its name is spelled correctly. Then try again.
--
_LexiconNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_LexiconNotFoundException
  = _MatchServiceError polly "LexiconNotFoundException"
      . hasStatus 404
