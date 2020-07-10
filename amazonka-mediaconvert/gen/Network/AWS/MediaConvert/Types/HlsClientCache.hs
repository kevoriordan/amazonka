{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsClientCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsClientCache (
  HlsClientCache (
    ..
    , HCCDisabled
    , HCCEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
data HlsClientCache = HlsClientCache' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern HCCDisabled :: HlsClientCache
pattern HCCDisabled = HlsClientCache' "DISABLED"

pattern HCCEnabled :: HlsClientCache
pattern HCCEnabled = HlsClientCache' "ENABLED"

{-# COMPLETE
  HCCDisabled,
  HCCEnabled,
  HlsClientCache' #-}

instance FromText HlsClientCache where
    parser = (HlsClientCache' . mk) <$> takeText

instance ToText HlsClientCache where
    toText (HlsClientCache' ci) = original ci

-- | Represents an enum of /known/ $HlsClientCache.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsClientCache where
    toEnum i = case i of
        0 -> HCCDisabled
        1 -> HCCEnabled
        _ -> (error . showText) $ "Unknown index for HlsClientCache: " <> toText i
    fromEnum x = case x of
        HCCDisabled -> 0
        HCCEnabled -> 1
        HlsClientCache' name -> (error . showText) $ "Unknown HlsClientCache: " <> original name

-- | Represents the bounds of /known/ $HlsClientCache.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsClientCache where
    minBound = HCCDisabled
    maxBound = HCCEnabled

instance Hashable     HlsClientCache
instance NFData       HlsClientCache
instance ToByteString HlsClientCache
instance ToQuery      HlsClientCache
instance ToHeader     HlsClientCache

instance ToJSON HlsClientCache where
    toJSON = toJSONText

instance FromJSON HlsClientCache where
    parseJSON = parseJSONText "HlsClientCache"
