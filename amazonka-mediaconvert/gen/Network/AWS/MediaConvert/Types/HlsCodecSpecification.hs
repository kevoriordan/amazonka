{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsCodecSpecification (
  HlsCodecSpecification (
    ..
    , HCSRfc4281
    , HCSRfc6381
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
data HlsCodecSpecification = HlsCodecSpecification' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern HCSRfc4281 :: HlsCodecSpecification
pattern HCSRfc4281 = HlsCodecSpecification' "RFC_4281"

pattern HCSRfc6381 :: HlsCodecSpecification
pattern HCSRfc6381 = HlsCodecSpecification' "RFC_6381"

{-# COMPLETE
  HCSRfc4281,
  HCSRfc6381,
  HlsCodecSpecification' #-}

instance FromText HlsCodecSpecification where
    parser = (HlsCodecSpecification' . mk) <$> takeText

instance ToText HlsCodecSpecification where
    toText (HlsCodecSpecification' ci) = original ci

-- | Represents an enum of /known/ $HlsCodecSpecification.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsCodecSpecification where
    toEnum i = case i of
        0 -> HCSRfc4281
        1 -> HCSRfc6381
        _ -> (error . showText) $ "Unknown index for HlsCodecSpecification: " <> toText i
    fromEnum x = case x of
        HCSRfc4281 -> 0
        HCSRfc6381 -> 1
        HlsCodecSpecification' name -> (error . showText) $ "Unknown HlsCodecSpecification: " <> original name

-- | Represents the bounds of /known/ $HlsCodecSpecification.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsCodecSpecification where
    minBound = HCSRfc4281
    maxBound = HCSRfc6381

instance Hashable     HlsCodecSpecification
instance NFData       HlsCodecSpecification
instance ToByteString HlsCodecSpecification
instance ToQuery      HlsCodecSpecification
instance ToHeader     HlsCodecSpecification

instance ToJSON HlsCodecSpecification where
    toJSON = toJSONText

instance FromJSON HlsCodecSpecification where
    parseJSON = parseJSONText "HlsCodecSpecification"
