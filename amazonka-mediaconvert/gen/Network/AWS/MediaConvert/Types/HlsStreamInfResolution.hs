{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsStreamInfResolution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsStreamInfResolution (
  HlsStreamInfResolution (
    ..
    , HSIRExclude
    , HSIRInclude
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
data HlsStreamInfResolution = HlsStreamInfResolution' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern HSIRExclude :: HlsStreamInfResolution
pattern HSIRExclude = HlsStreamInfResolution' "EXCLUDE"

pattern HSIRInclude :: HlsStreamInfResolution
pattern HSIRInclude = HlsStreamInfResolution' "INCLUDE"

{-# COMPLETE
  HSIRExclude,
  HSIRInclude,
  HlsStreamInfResolution' #-}

instance FromText HlsStreamInfResolution where
    parser = (HlsStreamInfResolution' . mk) <$> takeText

instance ToText HlsStreamInfResolution where
    toText (HlsStreamInfResolution' ci) = original ci

-- | Represents an enum of /known/ $HlsStreamInfResolution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsStreamInfResolution where
    toEnum i = case i of
        0 -> HSIRExclude
        1 -> HSIRInclude
        _ -> (error . showText) $ "Unknown index for HlsStreamInfResolution: " <> toText i
    fromEnum x = case x of
        HSIRExclude -> 0
        HSIRInclude -> 1
        HlsStreamInfResolution' name -> (error . showText) $ "Unknown HlsStreamInfResolution: " <> original name

-- | Represents the bounds of /known/ $HlsStreamInfResolution.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsStreamInfResolution where
    minBound = HSIRExclude
    maxBound = HSIRInclude

instance Hashable     HlsStreamInfResolution
instance NFData       HlsStreamInfResolution
instance ToByteString HlsStreamInfResolution
instance ToQuery      HlsStreamInfResolution
instance ToHeader     HlsStreamInfResolution

instance ToJSON HlsStreamInfResolution where
    toJSON = toJSONText

instance FromJSON HlsStreamInfResolution where
    parseJSON = parseJSONText "HlsStreamInfResolution"
