{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsRedundantManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsRedundantManifest (
  HlsRedundantManifest (
    ..
    , HRMDisabled
    , HRMEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Redundant Manifest
data HlsRedundantManifest = HlsRedundantManifest' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HRMDisabled :: HlsRedundantManifest
pattern HRMDisabled = HlsRedundantManifest' "DISABLED"

pattern HRMEnabled :: HlsRedundantManifest
pattern HRMEnabled = HlsRedundantManifest' "ENABLED"

{-# COMPLETE
  HRMDisabled,
  HRMEnabled,
  HlsRedundantManifest' #-}

instance FromText HlsRedundantManifest where
    parser = (HlsRedundantManifest' . mk) <$> takeText

instance ToText HlsRedundantManifest where
    toText (HlsRedundantManifest' ci) = original ci

-- | Represents an enum of /known/ $HlsRedundantManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsRedundantManifest where
    toEnum i = case i of
        0 -> HRMDisabled
        1 -> HRMEnabled
        _ -> (error . showText) $ "Unknown index for HlsRedundantManifest: " <> toText i
    fromEnum x = case x of
        HRMDisabled -> 0
        HRMEnabled -> 1
        HlsRedundantManifest' name -> (error . showText) $ "Unknown HlsRedundantManifest: " <> original name

-- | Represents the bounds of /known/ $HlsRedundantManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsRedundantManifest where
    minBound = HRMDisabled
    maxBound = HRMEnabled

instance Hashable     HlsRedundantManifest
instance NFData       HlsRedundantManifest
instance ToByteString HlsRedundantManifest
instance ToQuery      HlsRedundantManifest
instance ToHeader     HlsRedundantManifest

instance ToJSON HlsRedundantManifest where
    toJSON = toJSONText

instance FromJSON HlsRedundantManifest where
    parseJSON = parseJSONText "HlsRedundantManifest"
