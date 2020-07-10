{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAdMarkers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsAdMarkers (
  HlsAdMarkers (
    ..
    , Elemental
    , ElementalSCTE35
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HlsAdMarkers = HlsAdMarkers' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Elemental :: HlsAdMarkers
pattern Elemental = HlsAdMarkers' "ELEMENTAL"

pattern ElementalSCTE35 :: HlsAdMarkers
pattern ElementalSCTE35 = HlsAdMarkers' "ELEMENTAL_SCTE35"

{-# COMPLETE
  Elemental,
  ElementalSCTE35,
  HlsAdMarkers' #-}

instance FromText HlsAdMarkers where
    parser = (HlsAdMarkers' . mk) <$> takeText

instance ToText HlsAdMarkers where
    toText (HlsAdMarkers' ci) = original ci

-- | Represents an enum of /known/ $HlsAdMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsAdMarkers where
    toEnum i = case i of
        0 -> Elemental
        1 -> ElementalSCTE35
        _ -> (error . showText) $ "Unknown index for HlsAdMarkers: " <> toText i
    fromEnum x = case x of
        Elemental -> 0
        ElementalSCTE35 -> 1
        HlsAdMarkers' name -> (error . showText) $ "Unknown HlsAdMarkers: " <> original name

-- | Represents the bounds of /known/ $HlsAdMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsAdMarkers where
    minBound = Elemental
    maxBound = ElementalSCTE35

instance Hashable     HlsAdMarkers
instance NFData       HlsAdMarkers
instance ToByteString HlsAdMarkers
instance ToQuery      HlsAdMarkers
instance ToHeader     HlsAdMarkers

instance ToJSON HlsAdMarkers where
    toJSON = toJSONText

instance FromJSON HlsAdMarkers where
    parseJSON = parseJSONText "HlsAdMarkers"
