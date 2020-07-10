{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsageName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.KeyUsageName (
  KeyUsageName (
    ..
    , KUNAny
    , KUNCertificateSigning
    , KUNCrlSigning
    , KUNCustom
    , KUNDataEncipherment
    , KUNDecipherOnly
    , KUNDigitalSignature
    , KUNEncipherOnly
    , KUNKeyAgreement
    , KUNKeyEncipherment
    , KUNNonRepudiation
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyUsageName = KeyUsageName' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern KUNAny :: KeyUsageName
pattern KUNAny = KeyUsageName' "ANY"

pattern KUNCertificateSigning :: KeyUsageName
pattern KUNCertificateSigning = KeyUsageName' "CERTIFICATE_SIGNING"

pattern KUNCrlSigning :: KeyUsageName
pattern KUNCrlSigning = KeyUsageName' "CRL_SIGNING"

pattern KUNCustom :: KeyUsageName
pattern KUNCustom = KeyUsageName' "CUSTOM"

pattern KUNDataEncipherment :: KeyUsageName
pattern KUNDataEncipherment = KeyUsageName' "DATA_ENCIPHERMENT"

pattern KUNDecipherOnly :: KeyUsageName
pattern KUNDecipherOnly = KeyUsageName' "DECIPHER_ONLY"

pattern KUNDigitalSignature :: KeyUsageName
pattern KUNDigitalSignature = KeyUsageName' "DIGITAL_SIGNATURE"

pattern KUNEncipherOnly :: KeyUsageName
pattern KUNEncipherOnly = KeyUsageName' "ENCIPHER_ONLY"

pattern KUNKeyAgreement :: KeyUsageName
pattern KUNKeyAgreement = KeyUsageName' "KEY_AGREEMENT"

pattern KUNKeyEncipherment :: KeyUsageName
pattern KUNKeyEncipherment = KeyUsageName' "KEY_ENCIPHERMENT"

pattern KUNNonRepudiation :: KeyUsageName
pattern KUNNonRepudiation = KeyUsageName' "NON_REPUDIATION"

{-# COMPLETE
  KUNAny,
  KUNCertificateSigning,
  KUNCrlSigning,
  KUNCustom,
  KUNDataEncipherment,
  KUNDecipherOnly,
  KUNDigitalSignature,
  KUNEncipherOnly,
  KUNKeyAgreement,
  KUNKeyEncipherment,
  KUNNonRepudiation,
  KeyUsageName' #-}

instance FromText KeyUsageName where
    parser = (KeyUsageName' . mk) <$> takeText

instance ToText KeyUsageName where
    toText (KeyUsageName' ci) = original ci

-- | Represents an enum of /known/ $KeyUsageName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyUsageName where
    toEnum i = case i of
        0 -> KUNAny
        1 -> KUNCertificateSigning
        2 -> KUNCrlSigning
        3 -> KUNCustom
        4 -> KUNDataEncipherment
        5 -> KUNDecipherOnly
        6 -> KUNDigitalSignature
        7 -> KUNEncipherOnly
        8 -> KUNKeyAgreement
        9 -> KUNKeyEncipherment
        10 -> KUNNonRepudiation
        _ -> (error . showText) $ "Unknown index for KeyUsageName: " <> toText i
    fromEnum x = case x of
        KUNAny -> 0
        KUNCertificateSigning -> 1
        KUNCrlSigning -> 2
        KUNCustom -> 3
        KUNDataEncipherment -> 4
        KUNDecipherOnly -> 5
        KUNDigitalSignature -> 6
        KUNEncipherOnly -> 7
        KUNKeyAgreement -> 8
        KUNKeyEncipherment -> 9
        KUNNonRepudiation -> 10
        KeyUsageName' name -> (error . showText) $ "Unknown KeyUsageName: " <> original name

-- | Represents the bounds of /known/ $KeyUsageName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyUsageName where
    minBound = KUNAny
    maxBound = KUNNonRepudiation

instance Hashable     KeyUsageName
instance NFData       KeyUsageName
instance ToByteString KeyUsageName
instance ToQuery      KeyUsageName
instance ToHeader     KeyUsageName

instance ToJSON KeyUsageName where
    toJSON = toJSONText

instance FromJSON KeyUsageName where
    parseJSON = parseJSONText "KeyUsageName"
