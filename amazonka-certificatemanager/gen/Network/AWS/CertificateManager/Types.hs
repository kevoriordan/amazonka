{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types
    (
    -- * Service Configuration
      certificateManager

    -- * Errors
    , _TagPolicyException
    , _InvalidDomainValidationOptionsException
    , _InvalidParameterException
    , _InvalidTagException
    , _RequestInProgressException
    , _InvalidStateException
    , _TooManyTagsException
    , _InvalidArgsException
    , _ResourceNotFoundException
    , _InvalidARNException
    , _LimitExceededException
    , _ResourceInUseException

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * CertificateTransparencyLoggingPreference
    , CertificateTransparencyLoggingPreference (..)

    -- * CertificateType
    , CertificateType (..)

    -- * DomainStatus
    , DomainStatus (..)

    -- * ExtendedKeyUsageName
    , ExtendedKeyUsageName (..)

    -- * FailureReason
    , FailureReason (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * KeyUsageName
    , KeyUsageName (..)

    -- * RecordType
    , RecordType (..)

    -- * RenewalEligibility
    , RenewalEligibility (..)

    -- * RenewalStatus
    , RenewalStatus (..)

    -- * RevocationReason
    , RevocationReason (..)

    -- * ValidationMethod
    , ValidationMethod (..)

    -- * CertificateDetail
    , CertificateDetail
    , certificateDetail
    , cdSubject
    , cdStatus
    , cdFailureReason
    , cdSubjectAlternativeNames
    , cdInUseBy
    , cdCreatedAt
    , cdCertificateARN
    , cdSerial
    , cdRenewalEligibility
    , cdExtendedKeyUsages
    , cdImportedAt
    , cdKeyUsages
    , cdRevokedAt
    , cdNotBefore
    , cdRevocationReason
    , cdDomainName
    , cdRenewalSummary
    , cdKeyAlgorithm
    , cdType
    , cdOptions
    , cdIssuedAt
    , cdSignatureAlgorithm
    , cdDomainValidationOptions
    , cdIssuer
    , cdNotAfter
    , cdCertificateAuthorityARN

    -- * CertificateOptions
    , CertificateOptions
    , certificateOptions
    , coCertificateTransparencyLoggingPreference

    -- * CertificateSummary
    , CertificateSummary
    , certificateSummary
    , csCertificateARN
    , csDomainName

    -- * DomainValidation
    , DomainValidation
    , domainValidation
    , dvValidationEmails
    , dvValidationMethod
    , dvResourceRecord
    , dvValidationStatus
    , dvValidationDomain
    , dvDomainName

    -- * DomainValidationOption
    , DomainValidationOption
    , domainValidationOption
    , dvoDomainName
    , dvoValidationDomain

    -- * ExtendedKeyUsage
    , ExtendedKeyUsage
    , extendedKeyUsage
    , ekuOId
    , ekuName

    -- * Filters
    , Filters
    , filters
    , fKeyTypes
    , fKeyUsage
    , fExtendedKeyUsage

    -- * KeyUsage
    , KeyUsage
    , keyUsage
    , kuName

    -- * RenewalSummary
    , RenewalSummary
    , renewalSummary
    , rsRenewalStatusReason
    , rsRenewalStatus
    , rsDomainValidationOptions
    , rsUpdatedAt

    -- * ResourceRecord
    , ResourceRecord
    , resourceRecord
    , rrName
    , rrType
    , rrValue

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.CertificateManager.Types.CertificateStatus
import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import Network.AWS.CertificateManager.Types.CertificateType
import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.FailureReason
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsageName
import Network.AWS.CertificateManager.Types.RecordType
import Network.AWS.CertificateManager.Types.RenewalEligibility
import Network.AWS.CertificateManager.Types.RenewalStatus
import Network.AWS.CertificateManager.Types.RevocationReason
import Network.AWS.CertificateManager.Types.ValidationMethod
import Network.AWS.CertificateManager.Types.CertificateDetail
import Network.AWS.CertificateManager.Types.CertificateOptions
import Network.AWS.CertificateManager.Types.CertificateSummary
import Network.AWS.CertificateManager.Types.DomainValidation
import Network.AWS.CertificateManager.Types.DomainValidationOption
import Network.AWS.CertificateManager.Types.ExtendedKeyUsage
import Network.AWS.CertificateManager.Types.Filters
import Network.AWS.CertificateManager.Types.KeyUsage
import Network.AWS.CertificateManager.Types.RenewalSummary
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.Tag

-- | API version @2015-12-08@ of the Amazon Certificate Manager SDK configuration.
certificateManager :: Service
certificateManager
  = Service{_svcAbbrev = "CertificateManager",
            _svcSigner = v4, _svcPrefix = "acm",
            _svcVersion = "2015-12-08",
            _svcEndpoint = defaultEndpoint certificateManager,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "CertificateManager",
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

-- | A specified tag did not comply with an existing tag policy and was rejected.
--
--
_TagPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_TagPolicyException
  = _MatchServiceError certificateManager
      "TagPolicyException"

-- | One or more values in the 'DomainValidationOption' structure is incorrect.
--
--
_InvalidDomainValidationOptionsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDomainValidationOptionsException
  = _MatchServiceError certificateManager
      "InvalidDomainValidationOptionsException"

-- | An input parameter was invalid.
--
--
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException
  = _MatchServiceError certificateManager
      "InvalidParameterException"

-- | One or both of the values that make up the key-value pair is not valid. For example, you cannot specify a tag value that begins with @aws:@ .
--
--
_InvalidTagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagException
  = _MatchServiceError certificateManager
      "InvalidTagException"

-- | The certificate request is in process and the certificate in your account has not yet been issued.
--
--
_RequestInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_RequestInProgressException
  = _MatchServiceError certificateManager
      "RequestInProgressException"

-- | Processing has reached an invalid state.
--
--
_InvalidStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidStateException
  = _MatchServiceError certificateManager
      "InvalidStateException"

-- | The request contains too many tags. Try the request again with fewer tags.
--
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException
  = _MatchServiceError certificateManager
      "TooManyTagsException"

-- | One or more of of request parameters specified is not valid.
--
--
_InvalidArgsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgsException
  = _MatchServiceError certificateManager
      "InvalidArgsException"

-- | The specified certificate cannot be found in the caller's account or the caller's account cannot be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError certificateManager
      "ResourceNotFoundException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
--
--
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException
  = _MatchServiceError certificateManager
      "InvalidArnException"

-- | An ACM quota has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError certificateManager
      "LimitExceededException"

-- | The certificate is in use by another AWS service in the caller's account. Remove the association and try again.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError certificateManager
      "ResourceInUseException"
