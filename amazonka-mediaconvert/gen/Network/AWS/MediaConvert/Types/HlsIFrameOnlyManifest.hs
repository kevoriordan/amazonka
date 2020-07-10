{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest (
  HlsIFrameOnlyManifest (
    ..
    , HIFOMExclude
    , HIFOMInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
data HlsIFrameOnlyManifest = HlsIFrameOnlyManifest' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern HIFOMExclude :: HlsIFrameOnlyManifest
pattern HIFOMExclude = HlsIFrameOnlyManifest' "EXCLUDE"

pattern HIFOMInclude :: HlsIFrameOnlyManifest
pattern HIFOMInclude = HlsIFrameOnlyManifest' "INCLUDE"

{-# COMPLETE
  HIFOMExclude,
  HIFOMInclude,
  HlsIFrameOnlyManifest' #-}

instance FromText HlsIFrameOnlyManifest where
    parser = (HlsIFrameOnlyManifest' . mk) <$> takeText

instance ToText HlsIFrameOnlyManifest where
    toText (HlsIFrameOnlyManifest' ci) = original ci

-- | Represents an enum of /known/ $HlsIFrameOnlyManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsIFrameOnlyManifest where
    toEnum i = case i of
        0 -> HIFOMExclude
        1 -> HIFOMInclude
        _ -> (error . showText) $ "Unknown index for HlsIFrameOnlyManifest: " <> toText i
    fromEnum x = case x of
        HIFOMExclude -> 0
        HIFOMInclude -> 1
        HlsIFrameOnlyManifest' name -> (error . showText) $ "Unknown HlsIFrameOnlyManifest: " <> original name

-- | Represents the bounds of /known/ $HlsIFrameOnlyManifest.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsIFrameOnlyManifest where
    minBound = HIFOMExclude
    maxBound = HIFOMInclude

instance Hashable     HlsIFrameOnlyManifest
instance NFData       HlsIFrameOnlyManifest
instance ToByteString HlsIFrameOnlyManifest
instance ToQuery      HlsIFrameOnlyManifest
instance ToHeader     HlsIFrameOnlyManifest

instance ToJSON HlsIFrameOnlyManifest where
    toJSON = toJSONText

instance FromJSON HlsIFrameOnlyManifest where
    parseJSON = parseJSONText "HlsIFrameOnlyManifest"
