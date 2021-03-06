{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsKeyProviderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsKeyProviderType (
  HlsKeyProviderType (
    ..
    , HKPTSpeke
    , HKPTStaticKey
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
data HlsKeyProviderType = HlsKeyProviderType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern HKPTSpeke :: HlsKeyProviderType
pattern HKPTSpeke = HlsKeyProviderType' "SPEKE"

pattern HKPTStaticKey :: HlsKeyProviderType
pattern HKPTStaticKey = HlsKeyProviderType' "STATIC_KEY"

{-# COMPLETE
  HKPTSpeke,
  HKPTStaticKey,
  HlsKeyProviderType' #-}

instance FromText HlsKeyProviderType where
    parser = (HlsKeyProviderType' . mk) <$> takeText

instance ToText HlsKeyProviderType where
    toText (HlsKeyProviderType' ci) = original ci

-- | Represents an enum of /known/ $HlsKeyProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsKeyProviderType where
    toEnum i = case i of
        0 -> HKPTSpeke
        1 -> HKPTStaticKey
        _ -> (error . showText) $ "Unknown index for HlsKeyProviderType: " <> toText i
    fromEnum x = case x of
        HKPTSpeke -> 0
        HKPTStaticKey -> 1
        HlsKeyProviderType' name -> (error . showText) $ "Unknown HlsKeyProviderType: " <> original name

-- | Represents the bounds of /known/ $HlsKeyProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsKeyProviderType where
    minBound = HKPTSpeke
    maxBound = HKPTStaticKey

instance Hashable     HlsKeyProviderType
instance NFData       HlsKeyProviderType
instance ToByteString HlsKeyProviderType
instance ToQuery      HlsKeyProviderType
instance ToHeader     HlsKeyProviderType

instance ToJSON HlsKeyProviderType where
    toJSON = toJSONText

instance FromJSON HlsKeyProviderType where
    parseJSON = parseJSONText "HlsKeyProviderType"
