{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.EncryptionMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.EncryptionMethod (
  EncryptionMethod (
    ..
    , AES128
    , SampleAES
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EncryptionMethod = EncryptionMethod' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AES128 :: EncryptionMethod
pattern AES128 = EncryptionMethod' "AES_128"

pattern SampleAES :: EncryptionMethod
pattern SampleAES = EncryptionMethod' "SAMPLE_AES"

{-# COMPLETE
  AES128,
  SampleAES,
  EncryptionMethod' #-}

instance FromText EncryptionMethod where
    parser = (EncryptionMethod' . mk) <$> takeText

instance ToText EncryptionMethod where
    toText (EncryptionMethod' ci) = original ci

-- | Represents an enum of /known/ $EncryptionMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EncryptionMethod where
    toEnum i = case i of
        0 -> AES128
        1 -> SampleAES
        _ -> (error . showText) $ "Unknown index for EncryptionMethod: " <> toText i
    fromEnum x = case x of
        AES128 -> 0
        SampleAES -> 1
        EncryptionMethod' name -> (error . showText) $ "Unknown EncryptionMethod: " <> original name

-- | Represents the bounds of /known/ $EncryptionMethod.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EncryptionMethod where
    minBound = AES128
    maxBound = SampleAES

instance Hashable     EncryptionMethod
instance NFData       EncryptionMethod
instance ToByteString EncryptionMethod
instance ToQuery      EncryptionMethod
instance ToHeader     EncryptionMethod

instance ToJSON EncryptionMethod where
    toJSON = toJSONText

instance FromJSON EncryptionMethod where
    parseJSON = parseJSONText "EncryptionMethod"
