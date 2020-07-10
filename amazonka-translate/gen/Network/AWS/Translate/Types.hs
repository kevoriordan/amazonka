{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types
    (
    -- * Service Configuration
      translate

    -- * Errors
    , _TooManyRequestsException
    , _InvalidRequestException
    , _InvalidParameterValueException
    , _ResourceNotFoundException
    , _DetectedLanguageLowConfidenceException
    , _ServiceUnavailableException
    , _InternalServerException
    , _UnsupportedLanguagePairException
    , _LimitExceededException
    , _TextSizeLimitExceededException
    , _InvalidFilterException

    -- * EncryptionKeyType
    , EncryptionKeyType (..)

    -- * JobStatus
    , JobStatus (..)

    -- * MergeStrategy
    , MergeStrategy (..)

    -- * TerminologyDataFormat
    , TerminologyDataFormat (..)

    -- * AppliedTerminology
    , AppliedTerminology
    , appliedTerminology
    , atTerms
    , atName

    -- * EncryptionKey
    , EncryptionKey
    , encryptionKey
    , ekType
    , ekId

    -- * InputDataConfig
    , InputDataConfig
    , inputDataConfig
    , idcS3URI
    , idcContentType

    -- * JobDetails
    , JobDetails
    , jobDetails
    , jdTranslatedDocumentsCount
    , jdDocumentsWithErrorsCount
    , jdInputDocumentsCount

    -- * OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcS3URI

    -- * Term
    , Term
    , term
    , tTargetText
    , tSourceText

    -- * TerminologyData
    , TerminologyData
    , terminologyData
    , tdFile
    , tdFormat

    -- * TerminologyDataLocation
    , TerminologyDataLocation
    , terminologyDataLocation
    , tdlRepositoryType
    , tdlLocation

    -- * TerminologyProperties
    , TerminologyProperties
    , terminologyProperties
    , tpSizeBytes
    , tpLastUpdatedAt
    , tpARN
    , tpTargetLanguageCodes
    , tpCreatedAt
    , tpName
    , tpSourceLanguageCode
    , tpTermCount
    , tpEncryptionKey
    , tpDescription

    -- * TextTranslationJobFilter
    , TextTranslationJobFilter
    , textTranslationJobFilter
    , ttjfSubmittedBeforeTime
    , ttjfSubmittedAfterTime
    , ttjfJobName
    , ttjfJobStatus

    -- * TextTranslationJobProperties
    , TextTranslationJobProperties
    , textTranslationJobProperties
    , ttjpJobId
    , ttjpTargetLanguageCodes
    , ttjpJobName
    , ttjpSubmittedTime
    , ttjpInputDataConfig
    , ttjpTerminologyNames
    , ttjpSourceLanguageCode
    , ttjpEndTime
    , ttjpOutputDataConfig
    , ttjpJobDetails
    , ttjpDataAccessRoleARN
    , ttjpJobStatus
    , ttjpMessage
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Translate.Types.EncryptionKeyType
import Network.AWS.Translate.Types.JobStatus
import Network.AWS.Translate.Types.MergeStrategy
import Network.AWS.Translate.Types.TerminologyDataFormat
import Network.AWS.Translate.Types.AppliedTerminology
import Network.AWS.Translate.Types.EncryptionKey
import Network.AWS.Translate.Types.InputDataConfig
import Network.AWS.Translate.Types.JobDetails
import Network.AWS.Translate.Types.OutputDataConfig
import Network.AWS.Translate.Types.Term
import Network.AWS.Translate.Types.TerminologyData
import Network.AWS.Translate.Types.TerminologyDataLocation
import Network.AWS.Translate.Types.TerminologyProperties
import Network.AWS.Translate.Types.TextTranslationJobFilter
import Network.AWS.Translate.Types.TextTranslationJobProperties

-- | API version @2017-07-01@ of the Amazon Translate SDK configuration.
translate :: Service
translate
  = Service{_svcAbbrev = "Translate", _svcSigner = v4,
            _svcPrefix = "translate", _svcVersion = "2017-07-01",
            _svcEndpoint = defaultEndpoint translate,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "Translate",
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

-- | You have made too many requests within a short period of time. Wait for a short time and then try your request again.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException
  = _MatchServiceError translate
      "TooManyRequestsException"

-- | The request that you made is invalid. Check your request to determine why it's invalid and then retry the request. 
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException
  = _MatchServiceError translate
      "InvalidRequestException"

-- | The value of the parameter is invalid. Review the value of the parameter you are using to correct it, and then retry your operation.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException
  = _MatchServiceError translate
      "InvalidParameterValueException"

-- | The resource you are looking for has not been found. Review the resource you're looking for and see if a different resource will accomplish your needs before retrying the revised request.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError translate
      "ResourceNotFoundException"

-- | The confidence that Amazon Comprehend accurately detected the source language is low. If a low confidence level is acceptable for your application, you can use the language in the exception to call Amazon Translate again. For more information, see the <https://docs.aws.amazon.com/comprehend/latest/dg/API_DetectDominantLanguage.html DetectDominantLanguage> operation in the /Amazon Comprehend Developer Guide/ . 
--
--
_DetectedLanguageLowConfidenceException :: AsError a => Getting (First ServiceError) a ServiceError
_DetectedLanguageLowConfidenceException
  = _MatchServiceError translate
      "DetectedLanguageLowConfidenceException"

-- | The Amazon Translate service is temporarily unavailable. Please wait a bit and then retry your request.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException
  = _MatchServiceError translate
      "ServiceUnavailableException"

-- | An internal server error occurred. Retry your request.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException
  = _MatchServiceError translate
      "InternalServerException"

-- | Amazon Translate does not support translation from the language of the source text into the requested target language. For more information, see 'how-to-error-msg' . 
--
--
_UnsupportedLanguagePairException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedLanguagePairException
  = _MatchServiceError translate
      "UnsupportedLanguagePairException"

-- | The specified limit has been exceeded. Review your request and retry it with a quantity below the stated limit.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError translate
      "LimitExceededException"

-- | The size of the text you submitted exceeds the size limit. Reduce the size of the text or use a smaller document and then retry your request. 
--
--
_TextSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TextSizeLimitExceededException
  = _MatchServiceError translate
      "TextSizeLimitExceededException"

-- | The filter specified for the operation is invalid. Specify a different filter.
--
--
_InvalidFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilterException
  = _MatchServiceError translate
      "InvalidFilterException"
