{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RevocationReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.RevocationReason (
  RevocationReason (
    ..
    , AACompromise
    , AffiliationChanged
    , CaCompromise
    , CertificateHold
    , CessationOfOperation
    , KeyCompromise
    , PrivilegeWithdrawn
    , RemoveFromCrl
    , Superceded
    , Unspecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RevocationReason = RevocationReason' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AACompromise :: RevocationReason
pattern AACompromise = RevocationReason' "A_A_COMPROMISE"

pattern AffiliationChanged :: RevocationReason
pattern AffiliationChanged = RevocationReason' "AFFILIATION_CHANGED"

pattern CaCompromise :: RevocationReason
pattern CaCompromise = RevocationReason' "CA_COMPROMISE"

pattern CertificateHold :: RevocationReason
pattern CertificateHold = RevocationReason' "CERTIFICATE_HOLD"

pattern CessationOfOperation :: RevocationReason
pattern CessationOfOperation = RevocationReason' "CESSATION_OF_OPERATION"

pattern KeyCompromise :: RevocationReason
pattern KeyCompromise = RevocationReason' "KEY_COMPROMISE"

pattern PrivilegeWithdrawn :: RevocationReason
pattern PrivilegeWithdrawn = RevocationReason' "PRIVILEGE_WITHDRAWN"

pattern RemoveFromCrl :: RevocationReason
pattern RemoveFromCrl = RevocationReason' "REMOVE_FROM_CRL"

pattern Superceded :: RevocationReason
pattern Superceded = RevocationReason' "SUPERCEDED"

pattern Unspecified :: RevocationReason
pattern Unspecified = RevocationReason' "UNSPECIFIED"

{-# COMPLETE
  AACompromise,
  AffiliationChanged,
  CaCompromise,
  CertificateHold,
  CessationOfOperation,
  KeyCompromise,
  PrivilegeWithdrawn,
  RemoveFromCrl,
  Superceded,
  Unspecified,
  RevocationReason' #-}

instance FromText RevocationReason where
    parser = (RevocationReason' . mk) <$> takeText

instance ToText RevocationReason where
    toText (RevocationReason' ci) = original ci

-- | Represents an enum of /known/ $RevocationReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RevocationReason where
    toEnum i = case i of
        0 -> AACompromise
        1 -> AffiliationChanged
        2 -> CaCompromise
        3 -> CertificateHold
        4 -> CessationOfOperation
        5 -> KeyCompromise
        6 -> PrivilegeWithdrawn
        7 -> RemoveFromCrl
        8 -> Superceded
        9 -> Unspecified
        _ -> (error . showText) $ "Unknown index for RevocationReason: " <> toText i
    fromEnum x = case x of
        AACompromise -> 0
        AffiliationChanged -> 1
        CaCompromise -> 2
        CertificateHold -> 3
        CessationOfOperation -> 4
        KeyCompromise -> 5
        PrivilegeWithdrawn -> 6
        RemoveFromCrl -> 7
        Superceded -> 8
        Unspecified -> 9
        RevocationReason' name -> (error . showText) $ "Unknown RevocationReason: " <> original name

-- | Represents the bounds of /known/ $RevocationReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RevocationReason where
    minBound = AACompromise
    maxBound = Unspecified

instance Hashable     RevocationReason
instance NFData       RevocationReason
instance ToByteString RevocationReason
instance ToQuery      RevocationReason
instance ToHeader     RevocationReason

instance FromJSON RevocationReason where
    parseJSON = parseJSONText "RevocationReason"
