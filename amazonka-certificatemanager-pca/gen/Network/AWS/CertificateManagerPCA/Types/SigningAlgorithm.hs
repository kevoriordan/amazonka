{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm (
  SigningAlgorithm (
    ..
    , SHA256WITHECDSA
    , SHA256WITHRSA
    , SHA384WITHECDSA
    , SHA384WITHRSA
    , SHA512WITHECDSA
    , SHA512WITHRSA
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SigningAlgorithm = SigningAlgorithm' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern SHA256WITHECDSA :: SigningAlgorithm
pattern SHA256WITHECDSA = SigningAlgorithm' "SHA256WITHECDSA"

pattern SHA256WITHRSA :: SigningAlgorithm
pattern SHA256WITHRSA = SigningAlgorithm' "SHA256WITHRSA"

pattern SHA384WITHECDSA :: SigningAlgorithm
pattern SHA384WITHECDSA = SigningAlgorithm' "SHA384WITHECDSA"

pattern SHA384WITHRSA :: SigningAlgorithm
pattern SHA384WITHRSA = SigningAlgorithm' "SHA384WITHRSA"

pattern SHA512WITHECDSA :: SigningAlgorithm
pattern SHA512WITHECDSA = SigningAlgorithm' "SHA512WITHECDSA"

pattern SHA512WITHRSA :: SigningAlgorithm
pattern SHA512WITHRSA = SigningAlgorithm' "SHA512WITHRSA"

{-# COMPLETE
  SHA256WITHECDSA,
  SHA256WITHRSA,
  SHA384WITHECDSA,
  SHA384WITHRSA,
  SHA512WITHECDSA,
  SHA512WITHRSA,
  SigningAlgorithm' #-}

instance FromText SigningAlgorithm where
    parser = (SigningAlgorithm' . mk) <$> takeText

instance ToText SigningAlgorithm where
    toText (SigningAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $SigningAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SigningAlgorithm where
    toEnum i = case i of
        0 -> SHA256WITHECDSA
        1 -> SHA256WITHRSA
        2 -> SHA384WITHECDSA
        3 -> SHA384WITHRSA
        4 -> SHA512WITHECDSA
        5 -> SHA512WITHRSA
        _ -> (error . showText) $ "Unknown index for SigningAlgorithm: " <> toText i
    fromEnum x = case x of
        SHA256WITHECDSA -> 0
        SHA256WITHRSA -> 1
        SHA384WITHECDSA -> 2
        SHA384WITHRSA -> 3
        SHA512WITHECDSA -> 4
        SHA512WITHRSA -> 5
        SigningAlgorithm' name -> (error . showText) $ "Unknown SigningAlgorithm: " <> original name

-- | Represents the bounds of /known/ $SigningAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SigningAlgorithm where
    minBound = SHA256WITHECDSA
    maxBound = SHA512WITHRSA

instance Hashable     SigningAlgorithm
instance NFData       SigningAlgorithm
instance ToByteString SigningAlgorithm
instance ToQuery      SigningAlgorithm
instance ToHeader     SigningAlgorithm

instance ToJSON SigningAlgorithm where
    toJSON = toJSONText

instance FromJSON SigningAlgorithm where
    parseJSON = parseJSONText "SigningAlgorithm"
