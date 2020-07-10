{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsIvInManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsIvInManifest (
  HlsIvInManifest (
    ..
    , Exclude
    , Include
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Iv In Manifest
data HlsIvInManifest = HlsIvInManifest' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Exclude :: HlsIvInManifest
pattern Exclude = HlsIvInManifest' "EXCLUDE"

pattern Include :: HlsIvInManifest
pattern Include = HlsIvInManifest' "INCLUDE"

{-# COMPLETE
  Exclude,
  Include,
  HlsIvInManifest' #-}

instance FromText HlsIvInManifest where
    parser = (HlsIvInManifest' . mk) <$> takeText

instance ToText HlsIvInManifest where
    toText (HlsIvInManifest' ci) = original ci

-- | Represents an enum of /known/ $HlsIvInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsIvInManifest where
    toEnum i = case i of
        0 -> Exclude
        1 -> Include
        _ -> (error . showText) $ "Unknown index for HlsIvInManifest: " <> toText i
    fromEnum x = case x of
        Exclude -> 0
        Include -> 1
        HlsIvInManifest' name -> (error . showText) $ "Unknown HlsIvInManifest: " <> original name

-- | Represents the bounds of /known/ $HlsIvInManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsIvInManifest where
    minBound = Exclude
    maxBound = Include

instance Hashable     HlsIvInManifest
instance NFData       HlsIvInManifest
instance ToByteString HlsIvInManifest
instance ToQuery      HlsIvInManifest
instance ToHeader     HlsIvInManifest

instance ToJSON HlsIvInManifest where
    toJSON = toJSONText

instance FromJSON HlsIvInManifest where
    parseJSON = parseJSONText "HlsIvInManifest"
