{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.CustomerMasterKeySpec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.CustomerMasterKeySpec (
  CustomerMasterKeySpec (
    ..
    , CMKSEccNistP256
    , CMKSEccNistP384
    , CMKSEccNistP521
    , CMKSEccSecgP256K1
    , CMKSRsa2048
    , CMKSRsa3072
    , CMKSRsa4096
    , CMKSSymmetricDefault
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CustomerMasterKeySpec = CustomerMasterKeySpec' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern CMKSEccNistP256 :: CustomerMasterKeySpec
pattern CMKSEccNistP256 = CustomerMasterKeySpec' "ECC_NIST_P256"

pattern CMKSEccNistP384 :: CustomerMasterKeySpec
pattern CMKSEccNistP384 = CustomerMasterKeySpec' "ECC_NIST_P384"

pattern CMKSEccNistP521 :: CustomerMasterKeySpec
pattern CMKSEccNistP521 = CustomerMasterKeySpec' "ECC_NIST_P521"

pattern CMKSEccSecgP256K1 :: CustomerMasterKeySpec
pattern CMKSEccSecgP256K1 = CustomerMasterKeySpec' "ECC_SECG_P256K1"

pattern CMKSRsa2048 :: CustomerMasterKeySpec
pattern CMKSRsa2048 = CustomerMasterKeySpec' "RSA_2048"

pattern CMKSRsa3072 :: CustomerMasterKeySpec
pattern CMKSRsa3072 = CustomerMasterKeySpec' "RSA_3072"

pattern CMKSRsa4096 :: CustomerMasterKeySpec
pattern CMKSRsa4096 = CustomerMasterKeySpec' "RSA_4096"

pattern CMKSSymmetricDefault :: CustomerMasterKeySpec
pattern CMKSSymmetricDefault = CustomerMasterKeySpec' "SYMMETRIC_DEFAULT"

{-# COMPLETE
  CMKSEccNistP256,
  CMKSEccNistP384,
  CMKSEccNistP521,
  CMKSEccSecgP256K1,
  CMKSRsa2048,
  CMKSRsa3072,
  CMKSRsa4096,
  CMKSSymmetricDefault,
  CustomerMasterKeySpec' #-}

instance FromText CustomerMasterKeySpec where
    parser = (CustomerMasterKeySpec' . mk) <$> takeText

instance ToText CustomerMasterKeySpec where
    toText (CustomerMasterKeySpec' ci) = original ci

-- | Represents an enum of /known/ $CustomerMasterKeySpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CustomerMasterKeySpec where
    toEnum i = case i of
        0 -> CMKSEccNistP256
        1 -> CMKSEccNistP384
        2 -> CMKSEccNistP521
        3 -> CMKSEccSecgP256K1
        4 -> CMKSRsa2048
        5 -> CMKSRsa3072
        6 -> CMKSRsa4096
        7 -> CMKSSymmetricDefault
        _ -> (error . showText) $ "Unknown index for CustomerMasterKeySpec: " <> toText i
    fromEnum x = case x of
        CMKSEccNistP256 -> 0
        CMKSEccNistP384 -> 1
        CMKSEccNistP521 -> 2
        CMKSEccSecgP256K1 -> 3
        CMKSRsa2048 -> 4
        CMKSRsa3072 -> 5
        CMKSRsa4096 -> 6
        CMKSSymmetricDefault -> 7
        CustomerMasterKeySpec' name -> (error . showText) $ "Unknown CustomerMasterKeySpec: " <> original name

-- | Represents the bounds of /known/ $CustomerMasterKeySpec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CustomerMasterKeySpec where
    minBound = CMKSEccNistP256
    maxBound = CMKSSymmetricDefault

instance Hashable     CustomerMasterKeySpec
instance NFData       CustomerMasterKeySpec
instance ToByteString CustomerMasterKeySpec
instance ToQuery      CustomerMasterKeySpec
instance ToHeader     CustomerMasterKeySpec

instance ToJSON CustomerMasterKeySpec where
    toJSON = toJSONText

instance FromJSON CustomerMasterKeySpec where
    parseJSON = parseJSONText "CustomerMasterKeySpec"
