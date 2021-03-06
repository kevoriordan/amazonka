{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Tiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265Tiles (
  H265Tiles (
    ..
    , HTDisabled
    , HTEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
data H265Tiles = H265Tiles' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern HTDisabled :: H265Tiles
pattern HTDisabled = H265Tiles' "DISABLED"

pattern HTEnabled :: H265Tiles
pattern HTEnabled = H265Tiles' "ENABLED"

{-# COMPLETE
  HTDisabled,
  HTEnabled,
  H265Tiles' #-}

instance FromText H265Tiles where
    parser = (H265Tiles' . mk) <$> takeText

instance ToText H265Tiles where
    toText (H265Tiles' ci) = original ci

-- | Represents an enum of /known/ $H265Tiles.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265Tiles where
    toEnum i = case i of
        0 -> HTDisabled
        1 -> HTEnabled
        _ -> (error . showText) $ "Unknown index for H265Tiles: " <> toText i
    fromEnum x = case x of
        HTDisabled -> 0
        HTEnabled -> 1
        H265Tiles' name -> (error . showText) $ "Unknown H265Tiles: " <> original name

-- | Represents the bounds of /known/ $H265Tiles.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265Tiles where
    minBound = HTDisabled
    maxBound = HTEnabled

instance Hashable     H265Tiles
instance NFData       H265Tiles
instance ToByteString H265Tiles
instance ToQuery      H265Tiles
instance ToHeader     H265Tiles

instance ToJSON H265Tiles where
    toJSON = toJSONText

instance FromJSON H265Tiles where
    parseJSON = parseJSONText "H265Tiles"
