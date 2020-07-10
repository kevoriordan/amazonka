{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus (
  LoadBalancerTLSCertificateDomainStatus (
    ..
    , LBTCDSFailed
    , LBTCDSPendingValidation
    , LBTCDSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerTLSCertificateDomainStatus = LoadBalancerTLSCertificateDomainStatus' (CI
                                                                                         Text)
                                                deriving (Eq, Ord, Read, Show,
                                                          Data, Typeable,
                                                          Generic)

pattern LBTCDSFailed :: LoadBalancerTLSCertificateDomainStatus
pattern LBTCDSFailed = LoadBalancerTLSCertificateDomainStatus' "FAILED"

pattern LBTCDSPendingValidation :: LoadBalancerTLSCertificateDomainStatus
pattern LBTCDSPendingValidation = LoadBalancerTLSCertificateDomainStatus' "PENDING_VALIDATION"

pattern LBTCDSSuccess :: LoadBalancerTLSCertificateDomainStatus
pattern LBTCDSSuccess = LoadBalancerTLSCertificateDomainStatus' "SUCCESS"

{-# COMPLETE
  LBTCDSFailed,
  LBTCDSPendingValidation,
  LBTCDSSuccess,
  LoadBalancerTLSCertificateDomainStatus' #-}

instance FromText LoadBalancerTLSCertificateDomainStatus where
    parser = (LoadBalancerTLSCertificateDomainStatus' . mk) <$> takeText

instance ToText LoadBalancerTLSCertificateDomainStatus where
    toText (LoadBalancerTLSCertificateDomainStatus' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerTLSCertificateDomainStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerTLSCertificateDomainStatus where
    toEnum i = case i of
        0 -> LBTCDSFailed
        1 -> LBTCDSPendingValidation
        2 -> LBTCDSSuccess
        _ -> (error . showText) $ "Unknown index for LoadBalancerTLSCertificateDomainStatus: " <> toText i
    fromEnum x = case x of
        LBTCDSFailed -> 0
        LBTCDSPendingValidation -> 1
        LBTCDSSuccess -> 2
        LoadBalancerTLSCertificateDomainStatus' name -> (error . showText) $ "Unknown LoadBalancerTLSCertificateDomainStatus: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerTLSCertificateDomainStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerTLSCertificateDomainStatus where
    minBound = LBTCDSFailed
    maxBound = LBTCDSSuccess

instance Hashable     LoadBalancerTLSCertificateDomainStatus
instance NFData       LoadBalancerTLSCertificateDomainStatus
instance ToByteString LoadBalancerTLSCertificateDomainStatus
instance ToQuery      LoadBalancerTLSCertificateDomainStatus
instance ToHeader     LoadBalancerTLSCertificateDomainStatus

instance FromJSON LoadBalancerTLSCertificateDomainStatus where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateDomainStatus"
