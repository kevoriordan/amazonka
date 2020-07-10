{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.CertificateStatus (
  CertificateStatus (
    ..
    , CSExpired
    , CSFailed
    , CSInactive
    , CSIssued
    , CSPendingValidation
    , CSRevoked
    , CSValidationTimedOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateStatus = CertificateStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern CSExpired :: CertificateStatus
pattern CSExpired = CertificateStatus' "EXPIRED"

pattern CSFailed :: CertificateStatus
pattern CSFailed = CertificateStatus' "FAILED"

pattern CSInactive :: CertificateStatus
pattern CSInactive = CertificateStatus' "INACTIVE"

pattern CSIssued :: CertificateStatus
pattern CSIssued = CertificateStatus' "ISSUED"

pattern CSPendingValidation :: CertificateStatus
pattern CSPendingValidation = CertificateStatus' "PENDING_VALIDATION"

pattern CSRevoked :: CertificateStatus
pattern CSRevoked = CertificateStatus' "REVOKED"

pattern CSValidationTimedOut :: CertificateStatus
pattern CSValidationTimedOut = CertificateStatus' "VALIDATION_TIMED_OUT"

{-# COMPLETE
  CSExpired,
  CSFailed,
  CSInactive,
  CSIssued,
  CSPendingValidation,
  CSRevoked,
  CSValidationTimedOut,
  CertificateStatus' #-}

instance FromText CertificateStatus where
    parser = (CertificateStatus' . mk) <$> takeText

instance ToText CertificateStatus where
    toText (CertificateStatus' ci) = original ci

-- | Represents an enum of /known/ $CertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CertificateStatus where
    toEnum i = case i of
        0 -> CSExpired
        1 -> CSFailed
        2 -> CSInactive
        3 -> CSIssued
        4 -> CSPendingValidation
        5 -> CSRevoked
        6 -> CSValidationTimedOut
        _ -> (error . showText) $ "Unknown index for CertificateStatus: " <> toText i
    fromEnum x = case x of
        CSExpired -> 0
        CSFailed -> 1
        CSInactive -> 2
        CSIssued -> 3
        CSPendingValidation -> 4
        CSRevoked -> 5
        CSValidationTimedOut -> 6
        CertificateStatus' name -> (error . showText) $ "Unknown CertificateStatus: " <> original name

-- | Represents the bounds of /known/ $CertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateStatus where
    minBound = CSExpired
    maxBound = CSValidationTimedOut

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"
