{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsAkamaiHTTPTransferMode (
  HlsAkamaiHTTPTransferMode (
    ..
    , AkamaiChunked
    , AkamaiNonChunked
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Akamai Http Transfer Mode
data HlsAkamaiHTTPTransferMode = HlsAkamaiHTTPTransferMode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern AkamaiChunked :: HlsAkamaiHTTPTransferMode
pattern AkamaiChunked = HlsAkamaiHTTPTransferMode' "CHUNKED"

pattern AkamaiNonChunked :: HlsAkamaiHTTPTransferMode
pattern AkamaiNonChunked = HlsAkamaiHTTPTransferMode' "NON_CHUNKED"

{-# COMPLETE
  AkamaiChunked,
  AkamaiNonChunked,
  HlsAkamaiHTTPTransferMode' #-}

instance FromText HlsAkamaiHTTPTransferMode where
    parser = (HlsAkamaiHTTPTransferMode' . mk) <$> takeText

instance ToText HlsAkamaiHTTPTransferMode where
    toText (HlsAkamaiHTTPTransferMode' ci) = original ci

-- | Represents an enum of /known/ $HlsAkamaiHTTPTransferMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsAkamaiHTTPTransferMode where
    toEnum i = case i of
        0 -> AkamaiChunked
        1 -> AkamaiNonChunked
        _ -> (error . showText) $ "Unknown index for HlsAkamaiHTTPTransferMode: " <> toText i
    fromEnum x = case x of
        AkamaiChunked -> 0
        AkamaiNonChunked -> 1
        HlsAkamaiHTTPTransferMode' name -> (error . showText) $ "Unknown HlsAkamaiHTTPTransferMode: " <> original name

-- | Represents the bounds of /known/ $HlsAkamaiHTTPTransferMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsAkamaiHTTPTransferMode where
    minBound = AkamaiChunked
    maxBound = AkamaiNonChunked

instance Hashable     HlsAkamaiHTTPTransferMode
instance NFData       HlsAkamaiHTTPTransferMode
instance ToByteString HlsAkamaiHTTPTransferMode
instance ToQuery      HlsAkamaiHTTPTransferMode
instance ToHeader     HlsAkamaiHTTPTransferMode

instance ToJSON HlsAkamaiHTTPTransferMode where
    toJSON = toJSONText

instance FromJSON HlsAkamaiHTTPTransferMode where
    parseJSON = parseJSONText "HlsAkamaiHTTPTransferMode"
