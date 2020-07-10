{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.SigningAlgorithmSpec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.SigningAlgorithmSpec (
  SigningAlgorithmSpec (
    ..
    , EcdsaSha256
    , EcdsaSha384
    , EcdsaSha512
    , RsassaPKCS1V15Sha256
    , RsassaPKCS1V15Sha384
    , RsassaPKCS1V15Sha512
    , RsassaPssSha256
    , RsassaPssSha384
    , RsassaPssSha512
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SigningAlgorithmSpec = SigningAlgorithmSpec' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern EcdsaSha256 :: SigningAlgorithmSpec
pattern EcdsaSha256 = SigningAlgorithmSpec' "ECDSA_SHA_256"

pattern EcdsaSha384 :: SigningAlgorithmSpec
pattern EcdsaSha384 = SigningAlgorithmSpec' "ECDSA_SHA_384"

pattern EcdsaSha512 :: SigningAlgorithmSpec
pattern EcdsaSha512 = SigningAlgorithmSpec' "ECDSA_SHA_512"

pattern RsassaPKCS1V15Sha256 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha256 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_256"

pattern RsassaPKCS1V15Sha384 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha384 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_384"

pattern RsassaPKCS1V15Sha512 :: SigningAlgorithmSpec
pattern RsassaPKCS1V15Sha512 = SigningAlgorithmSpec' "RSASSA_PKCS1_V1_5_SHA_512"

pattern RsassaPssSha256 :: SigningAlgorithmSpec
pattern RsassaPssSha256 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_256"

pattern RsassaPssSha384 :: SigningAlgorithmSpec
pattern RsassaPssSha384 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_384"

pattern RsassaPssSha512 :: SigningAlgorithmSpec
pattern RsassaPssSha512 = SigningAlgorithmSpec' "RSASSA_PSS_SHA_512"

{-# COMPLETE
  EcdsaSha256,
  EcdsaSha384,
  EcdsaSha512,
  RsassaPKCS1V15Sha256,
  RsassaPKCS1V15Sha384,
  RsassaPKCS1V15Sha512,
  RsassaPssSha256,
  RsassaPssSha384,
  RsassaPssSha512,
  SigningAlgorithmSpec' #-}

instance FromText SigningAlgorithmSpec where
    parser = (SigningAlgorithmSpec' . mk) <$> takeText

instance ToText SigningAlgorithmSpec where
    toText (SigningAlgorithmSpec' ci) = original ci

-- | Represents an enum of /known/ $SigningAlgorithmSpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SigningAlgorithmSpec where
    toEnum i = case i of
        0 -> EcdsaSha256
        1 -> EcdsaSha384
        2 -> EcdsaSha512
        3 -> RsassaPKCS1V15Sha256
        4 -> RsassaPKCS1V15Sha384
        5 -> RsassaPKCS1V15Sha512
        6 -> RsassaPssSha256
        7 -> RsassaPssSha384
        8 -> RsassaPssSha512
        _ -> (error . showText) $ "Unknown index for SigningAlgorithmSpec: " <> toText i
    fromEnum x = case x of
        EcdsaSha256 -> 0
        EcdsaSha384 -> 1
        EcdsaSha512 -> 2
        RsassaPKCS1V15Sha256 -> 3
        RsassaPKCS1V15Sha384 -> 4
        RsassaPKCS1V15Sha512 -> 5
        RsassaPssSha256 -> 6
        RsassaPssSha384 -> 7
        RsassaPssSha512 -> 8
        SigningAlgorithmSpec' name -> (error . showText) $ "Unknown SigningAlgorithmSpec: " <> original name

-- | Represents the bounds of /known/ $SigningAlgorithmSpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SigningAlgorithmSpec where
    minBound = EcdsaSha256
    maxBound = RsassaPssSha512

instance Hashable     SigningAlgorithmSpec
instance NFData       SigningAlgorithmSpec
instance ToByteString SigningAlgorithmSpec
instance ToQuery      SigningAlgorithmSpec
instance ToHeader     SigningAlgorithmSpec

instance ToJSON SigningAlgorithmSpec where
    toJSON = toJSONText

instance FromJSON SigningAlgorithmSpec where
    parseJSON = parseJSONText "SigningAlgorithmSpec"
