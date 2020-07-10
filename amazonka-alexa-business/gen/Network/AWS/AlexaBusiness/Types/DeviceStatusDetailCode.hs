{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode (
  DeviceStatusDetailCode (
    ..
    , AssociationRejection
    , AuthenticationFailure
    , CertificateIssuingLimitExceeded
    , CredentialsAccessFailure
    , DHCPFailure
    , DNSFailure
    , DeviceSoftwareUpdateNeeded
    , DeviceWasOffline
    , InternetUnavailable
    , InvalidCertificateAuthority
    , InvalidPasswordState
    , NetworkProfileNotFound
    , PasswordNotFound
    , TLSVersionMismatch
    , UnknownFailure
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceStatusDetailCode = DeviceStatusDetailCode' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AssociationRejection :: DeviceStatusDetailCode
pattern AssociationRejection = DeviceStatusDetailCode' "ASSOCIATION_REJECTION"

pattern AuthenticationFailure :: DeviceStatusDetailCode
pattern AuthenticationFailure = DeviceStatusDetailCode' "AUTHENTICATION_FAILURE"

pattern CertificateIssuingLimitExceeded :: DeviceStatusDetailCode
pattern CertificateIssuingLimitExceeded = DeviceStatusDetailCode' "CERTIFICATE_ISSUING_LIMIT_EXCEEDED"

pattern CredentialsAccessFailure :: DeviceStatusDetailCode
pattern CredentialsAccessFailure = DeviceStatusDetailCode' "CREDENTIALS_ACCESS_FAILURE"

pattern DHCPFailure :: DeviceStatusDetailCode
pattern DHCPFailure = DeviceStatusDetailCode' "DHCP_FAILURE"

pattern DNSFailure :: DeviceStatusDetailCode
pattern DNSFailure = DeviceStatusDetailCode' "DNS_FAILURE"

pattern DeviceSoftwareUpdateNeeded :: DeviceStatusDetailCode
pattern DeviceSoftwareUpdateNeeded = DeviceStatusDetailCode' "DEVICE_SOFTWARE_UPDATE_NEEDED"

pattern DeviceWasOffline :: DeviceStatusDetailCode
pattern DeviceWasOffline = DeviceStatusDetailCode' "DEVICE_WAS_OFFLINE"

pattern InternetUnavailable :: DeviceStatusDetailCode
pattern InternetUnavailable = DeviceStatusDetailCode' "INTERNET_UNAVAILABLE"

pattern InvalidCertificateAuthority :: DeviceStatusDetailCode
pattern InvalidCertificateAuthority = DeviceStatusDetailCode' "INVALID_CERTIFICATE_AUTHORITY"

pattern InvalidPasswordState :: DeviceStatusDetailCode
pattern InvalidPasswordState = DeviceStatusDetailCode' "INVALID_PASSWORD_STATE"

pattern NetworkProfileNotFound :: DeviceStatusDetailCode
pattern NetworkProfileNotFound = DeviceStatusDetailCode' "NETWORK_PROFILE_NOT_FOUND"

pattern PasswordNotFound :: DeviceStatusDetailCode
pattern PasswordNotFound = DeviceStatusDetailCode' "PASSWORD_NOT_FOUND"

pattern TLSVersionMismatch :: DeviceStatusDetailCode
pattern TLSVersionMismatch = DeviceStatusDetailCode' "TLS_VERSION_MISMATCH"

pattern UnknownFailure :: DeviceStatusDetailCode
pattern UnknownFailure = DeviceStatusDetailCode' "UNKNOWN_FAILURE"

{-# COMPLETE
  AssociationRejection,
  AuthenticationFailure,
  CertificateIssuingLimitExceeded,
  CredentialsAccessFailure,
  DHCPFailure,
  DNSFailure,
  DeviceSoftwareUpdateNeeded,
  DeviceWasOffline,
  InternetUnavailable,
  InvalidCertificateAuthority,
  InvalidPasswordState,
  NetworkProfileNotFound,
  PasswordNotFound,
  TLSVersionMismatch,
  UnknownFailure,
  DeviceStatusDetailCode' #-}

instance FromText DeviceStatusDetailCode where
    parser = (DeviceStatusDetailCode' . mk) <$> takeText

instance ToText DeviceStatusDetailCode where
    toText (DeviceStatusDetailCode' ci) = original ci

-- | Represents an enum of /known/ $DeviceStatusDetailCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceStatusDetailCode where
    toEnum i = case i of
        0 -> AssociationRejection
        1 -> AuthenticationFailure
        2 -> CertificateIssuingLimitExceeded
        3 -> CredentialsAccessFailure
        4 -> DHCPFailure
        5 -> DNSFailure
        6 -> DeviceSoftwareUpdateNeeded
        7 -> DeviceWasOffline
        8 -> InternetUnavailable
        9 -> InvalidCertificateAuthority
        10 -> InvalidPasswordState
        11 -> NetworkProfileNotFound
        12 -> PasswordNotFound
        13 -> TLSVersionMismatch
        14 -> UnknownFailure
        _ -> (error . showText) $ "Unknown index for DeviceStatusDetailCode: " <> toText i
    fromEnum x = case x of
        AssociationRejection -> 0
        AuthenticationFailure -> 1
        CertificateIssuingLimitExceeded -> 2
        CredentialsAccessFailure -> 3
        DHCPFailure -> 4
        DNSFailure -> 5
        DeviceSoftwareUpdateNeeded -> 6
        DeviceWasOffline -> 7
        InternetUnavailable -> 8
        InvalidCertificateAuthority -> 9
        InvalidPasswordState -> 10
        NetworkProfileNotFound -> 11
        PasswordNotFound -> 12
        TLSVersionMismatch -> 13
        UnknownFailure -> 14
        DeviceStatusDetailCode' name -> (error . showText) $ "Unknown DeviceStatusDetailCode: " <> original name

-- | Represents the bounds of /known/ $DeviceStatusDetailCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceStatusDetailCode where
    minBound = AssociationRejection
    maxBound = UnknownFailure

instance Hashable     DeviceStatusDetailCode
instance NFData       DeviceStatusDetailCode
instance ToByteString DeviceStatusDetailCode
instance ToQuery      DeviceStatusDetailCode
instance ToHeader     DeviceStatusDetailCode

instance FromJSON DeviceStatusDetailCode where
    parseJSON = parseJSONText "DeviceStatusDetailCode"
