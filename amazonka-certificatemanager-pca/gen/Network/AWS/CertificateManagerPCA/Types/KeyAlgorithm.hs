{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm (
  KeyAlgorithm (
    ..
    , EcPRIME256V1
    , EcSECP384R1
    , Rsa2048
    , Rsa4096
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyAlgorithm = KeyAlgorithm' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern EcPRIME256V1 :: KeyAlgorithm
pattern EcPRIME256V1 = KeyAlgorithm' "EC_prime256v1"

pattern EcSECP384R1 :: KeyAlgorithm
pattern EcSECP384R1 = KeyAlgorithm' "EC_secp384r1"

pattern Rsa2048 :: KeyAlgorithm
pattern Rsa2048 = KeyAlgorithm' "RSA_2048"

pattern Rsa4096 :: KeyAlgorithm
pattern Rsa4096 = KeyAlgorithm' "RSA_4096"

{-# COMPLETE
  EcPRIME256V1,
  EcSECP384R1,
  Rsa2048,
  Rsa4096,
  KeyAlgorithm' #-}

instance FromText KeyAlgorithm where
    parser = (KeyAlgorithm' . mk) <$> takeText

instance ToText KeyAlgorithm where
    toText (KeyAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $KeyAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyAlgorithm where
    toEnum i = case i of
        0 -> EcPRIME256V1
        1 -> EcSECP384R1
        2 -> Rsa2048
        3 -> Rsa4096
        _ -> (error . showText) $ "Unknown index for KeyAlgorithm: " <> toText i
    fromEnum x = case x of
        EcPRIME256V1 -> 0
        EcSECP384R1 -> 1
        Rsa2048 -> 2
        Rsa4096 -> 3
        KeyAlgorithm' name -> (error . showText) $ "Unknown KeyAlgorithm: " <> original name

-- | Represents the bounds of /known/ $KeyAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyAlgorithm where
    minBound = EcPRIME256V1
    maxBound = Rsa4096

instance Hashable     KeyAlgorithm
instance NFData       KeyAlgorithm
instance ToByteString KeyAlgorithm
instance ToQuery      KeyAlgorithm
instance ToHeader     KeyAlgorithm

instance ToJSON KeyAlgorithm where
    toJSON = toJSONText

instance FromJSON KeyAlgorithm where
    parseJSON = parseJSONText "KeyAlgorithm"
