{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RenewalStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.RenewalStatus (
  RenewalStatus (
    ..
    , RSFailed
    , RSPendingAutoRenewal
    , RSPendingValidation
    , RSSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RenewalStatus = RenewalStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern RSFailed :: RenewalStatus
pattern RSFailed = RenewalStatus' "FAILED"

pattern RSPendingAutoRenewal :: RenewalStatus
pattern RSPendingAutoRenewal = RenewalStatus' "PENDING_AUTO_RENEWAL"

pattern RSPendingValidation :: RenewalStatus
pattern RSPendingValidation = RenewalStatus' "PENDING_VALIDATION"

pattern RSSuccess :: RenewalStatus
pattern RSSuccess = RenewalStatus' "SUCCESS"

{-# COMPLETE
  RSFailed,
  RSPendingAutoRenewal,
  RSPendingValidation,
  RSSuccess,
  RenewalStatus' #-}

instance FromText RenewalStatus where
    parser = (RenewalStatus' . mk) <$> takeText

instance ToText RenewalStatus where
    toText (RenewalStatus' ci) = original ci

-- | Represents an enum of /known/ $RenewalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RenewalStatus where
    toEnum i = case i of
        0 -> RSFailed
        1 -> RSPendingAutoRenewal
        2 -> RSPendingValidation
        3 -> RSSuccess
        _ -> (error . showText) $ "Unknown index for RenewalStatus: " <> toText i
    fromEnum x = case x of
        RSFailed -> 0
        RSPendingAutoRenewal -> 1
        RSPendingValidation -> 2
        RSSuccess -> 3
        RenewalStatus' name -> (error . showText) $ "Unknown RenewalStatus: " <> original name

-- | Represents the bounds of /known/ $RenewalStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RenewalStatus where
    minBound = RSFailed
    maxBound = RSSuccess

instance Hashable     RenewalStatus
instance NFData       RenewalStatus
instance ToByteString RenewalStatus
instance ToQuery      RenewalStatus
instance ToHeader     RenewalStatus

instance FromJSON RenewalStatus where
    parseJSON = parseJSONText "RenewalStatus"
