{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdMarkers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.AdMarkers (
  AdMarkers (
    ..
    , AMNone
    , AMPassthrough
    , AMSCTE35Enhanced
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AdMarkers = AdMarkers' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AMNone :: AdMarkers
pattern AMNone = AdMarkers' "NONE"

pattern AMPassthrough :: AdMarkers
pattern AMPassthrough = AdMarkers' "PASSTHROUGH"

pattern AMSCTE35Enhanced :: AdMarkers
pattern AMSCTE35Enhanced = AdMarkers' "SCTE35_ENHANCED"

{-# COMPLETE
  AMNone,
  AMPassthrough,
  AMSCTE35Enhanced,
  AdMarkers' #-}

instance FromText AdMarkers where
    parser = (AdMarkers' . mk) <$> takeText

instance ToText AdMarkers where
    toText (AdMarkers' ci) = original ci

-- | Represents an enum of /known/ $AdMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdMarkers where
    toEnum i = case i of
        0 -> AMNone
        1 -> AMPassthrough
        2 -> AMSCTE35Enhanced
        _ -> (error . showText) $ "Unknown index for AdMarkers: " <> toText i
    fromEnum x = case x of
        AMNone -> 0
        AMPassthrough -> 1
        AMSCTE35Enhanced -> 2
        AdMarkers' name -> (error . showText) $ "Unknown AdMarkers: " <> original name

-- | Represents the bounds of /known/ $AdMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdMarkers where
    minBound = AMNone
    maxBound = AMSCTE35Enhanced

instance Hashable     AdMarkers
instance NFData       AdMarkers
instance ToByteString AdMarkers
instance ToQuery      AdMarkers
instance ToHeader     AdMarkers

instance ToJSON AdMarkers where
    toJSON = toJSONText

instance FromJSON AdMarkers where
    parseJSON = parseJSONText "AdMarkers"
