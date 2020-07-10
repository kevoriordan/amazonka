{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CertificateStatus (
  CertificateStatus (
    ..
    , CSActive
    , CSInactive
    , CSPendingActivation
    , CSPendingTransfer
    , CSRegisterInactive
    , CSRevoked
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CertificateStatus = CertificateStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern CSActive :: CertificateStatus
pattern CSActive = CertificateStatus' "ACTIVE"

pattern CSInactive :: CertificateStatus
pattern CSInactive = CertificateStatus' "INACTIVE"

pattern CSPendingActivation :: CertificateStatus
pattern CSPendingActivation = CertificateStatus' "PENDING_ACTIVATION"

pattern CSPendingTransfer :: CertificateStatus
pattern CSPendingTransfer = CertificateStatus' "PENDING_TRANSFER"

pattern CSRegisterInactive :: CertificateStatus
pattern CSRegisterInactive = CertificateStatus' "REGISTER_INACTIVE"

pattern CSRevoked :: CertificateStatus
pattern CSRevoked = CertificateStatus' "REVOKED"

{-# COMPLETE
  CSActive,
  CSInactive,
  CSPendingActivation,
  CSPendingTransfer,
  CSRegisterInactive,
  CSRevoked,
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
        0 -> CSActive
        1 -> CSInactive
        2 -> CSPendingActivation
        3 -> CSPendingTransfer
        4 -> CSRegisterInactive
        5 -> CSRevoked
        _ -> (error . showText) $ "Unknown index for CertificateStatus: " <> toText i
    fromEnum x = case x of
        CSActive -> 0
        CSInactive -> 1
        CSPendingActivation -> 2
        CSPendingTransfer -> 3
        CSRegisterInactive -> 4
        CSRevoked -> 5
        CertificateStatus' name -> (error . showText) $ "Unknown CertificateStatus: " <> original name

-- | Represents the bounds of /known/ $CertificateStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CertificateStatus where
    minBound = CSActive
    maxBound = CSRevoked

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"
