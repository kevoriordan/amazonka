{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason (
  LoadBalancerTLSCertificateFailureReason (
    ..
    , AdditionalVerificationRequired
    , DomainNotAllowed
    , InvalidPublicDomain
    , NoAvailableContacts
    , Other
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerTLSCertificateFailureReason = LoadBalancerTLSCertificateFailureReason' (CI
                                                                                           Text)
                                                 deriving (Eq, Ord, Read, Show,
                                                           Data, Typeable,
                                                           Generic)

pattern AdditionalVerificationRequired :: LoadBalancerTLSCertificateFailureReason
pattern AdditionalVerificationRequired = LoadBalancerTLSCertificateFailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern DomainNotAllowed :: LoadBalancerTLSCertificateFailureReason
pattern DomainNotAllowed = LoadBalancerTLSCertificateFailureReason' "DOMAIN_NOT_ALLOWED"

pattern InvalidPublicDomain :: LoadBalancerTLSCertificateFailureReason
pattern InvalidPublicDomain = LoadBalancerTLSCertificateFailureReason' "INVALID_PUBLIC_DOMAIN"

pattern NoAvailableContacts :: LoadBalancerTLSCertificateFailureReason
pattern NoAvailableContacts = LoadBalancerTLSCertificateFailureReason' "NO_AVAILABLE_CONTACTS"

pattern Other :: LoadBalancerTLSCertificateFailureReason
pattern Other = LoadBalancerTLSCertificateFailureReason' "OTHER"

{-# COMPLETE
  AdditionalVerificationRequired,
  DomainNotAllowed,
  InvalidPublicDomain,
  NoAvailableContacts,
  Other,
  LoadBalancerTLSCertificateFailureReason' #-}

instance FromText LoadBalancerTLSCertificateFailureReason where
    parser = (LoadBalancerTLSCertificateFailureReason' . mk) <$> takeText

instance ToText LoadBalancerTLSCertificateFailureReason where
    toText (LoadBalancerTLSCertificateFailureReason' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerTLSCertificateFailureReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerTLSCertificateFailureReason where
    toEnum i = case i of
        0 -> AdditionalVerificationRequired
        1 -> DomainNotAllowed
        2 -> InvalidPublicDomain
        3 -> NoAvailableContacts
        4 -> Other
        _ -> (error . showText) $ "Unknown index for LoadBalancerTLSCertificateFailureReason: " <> toText i
    fromEnum x = case x of
        AdditionalVerificationRequired -> 0
        DomainNotAllowed -> 1
        InvalidPublicDomain -> 2
        NoAvailableContacts -> 3
        Other -> 4
        LoadBalancerTLSCertificateFailureReason' name -> (error . showText) $ "Unknown LoadBalancerTLSCertificateFailureReason: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerTLSCertificateFailureReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerTLSCertificateFailureReason where
    minBound = AdditionalVerificationRequired
    maxBound = Other

instance Hashable     LoadBalancerTLSCertificateFailureReason
instance NFData       LoadBalancerTLSCertificateFailureReason
instance ToByteString LoadBalancerTLSCertificateFailureReason
instance ToQuery      LoadBalancerTLSCertificateFailureReason
instance ToHeader     LoadBalancerTLSCertificateFailureReason

instance FromJSON LoadBalancerTLSCertificateFailureReason where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateFailureReason"
