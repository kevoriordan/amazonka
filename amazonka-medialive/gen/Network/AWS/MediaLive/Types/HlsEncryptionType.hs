{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsEncryptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsEncryptionType (
  HlsEncryptionType (
    ..
    , AES128
    , SampleAES
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Encryption Type
data HlsEncryptionType = HlsEncryptionType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AES128 :: HlsEncryptionType
pattern AES128 = HlsEncryptionType' "AES128"

pattern SampleAES :: HlsEncryptionType
pattern SampleAES = HlsEncryptionType' "SAMPLE_AES"

{-# COMPLETE
  AES128,
  SampleAES,
  HlsEncryptionType' #-}

instance FromText HlsEncryptionType where
    parser = (HlsEncryptionType' . mk) <$> takeText

instance ToText HlsEncryptionType where
    toText (HlsEncryptionType' ci) = original ci

-- | Represents an enum of /known/ $HlsEncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsEncryptionType where
    toEnum i = case i of
        0 -> AES128
        1 -> SampleAES
        _ -> (error . showText) $ "Unknown index for HlsEncryptionType: " <> toText i
    fromEnum x = case x of
        AES128 -> 0
        SampleAES -> 1
        HlsEncryptionType' name -> (error . showText) $ "Unknown HlsEncryptionType: " <> original name

-- | Represents the bounds of /known/ $HlsEncryptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsEncryptionType where
    minBound = AES128
    maxBound = SampleAES

instance Hashable     HlsEncryptionType
instance NFData       HlsEncryptionType
instance ToByteString HlsEncryptionType
instance ToQuery      HlsEncryptionType
instance ToHeader     HlsEncryptionType

instance ToJSON HlsEncryptionType where
    toJSON = toJSONText

instance FromJSON HlsEncryptionType where
    parseJSON = parseJSONText "HlsEncryptionType"
