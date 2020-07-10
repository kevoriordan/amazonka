{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInFontColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInFontColor (
  BurnInFontColor (
    ..
    , BIFCBlack
    , BIFCBlue
    , BIFCGreen
    , BIFCRed
    , BIFCWhite
    , BIFCYellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Burn In Font Color
data BurnInFontColor = BurnInFontColor' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern BIFCBlack :: BurnInFontColor
pattern BIFCBlack = BurnInFontColor' "BLACK"

pattern BIFCBlue :: BurnInFontColor
pattern BIFCBlue = BurnInFontColor' "BLUE"

pattern BIFCGreen :: BurnInFontColor
pattern BIFCGreen = BurnInFontColor' "GREEN"

pattern BIFCRed :: BurnInFontColor
pattern BIFCRed = BurnInFontColor' "RED"

pattern BIFCWhite :: BurnInFontColor
pattern BIFCWhite = BurnInFontColor' "WHITE"

pattern BIFCYellow :: BurnInFontColor
pattern BIFCYellow = BurnInFontColor' "YELLOW"

{-# COMPLETE
  BIFCBlack,
  BIFCBlue,
  BIFCGreen,
  BIFCRed,
  BIFCWhite,
  BIFCYellow,
  BurnInFontColor' #-}

instance FromText BurnInFontColor where
    parser = (BurnInFontColor' . mk) <$> takeText

instance ToText BurnInFontColor where
    toText (BurnInFontColor' ci) = original ci

-- | Represents an enum of /known/ $BurnInFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInFontColor where
    toEnum i = case i of
        0 -> BIFCBlack
        1 -> BIFCBlue
        2 -> BIFCGreen
        3 -> BIFCRed
        4 -> BIFCWhite
        5 -> BIFCYellow
        _ -> (error . showText) $ "Unknown index for BurnInFontColor: " <> toText i
    fromEnum x = case x of
        BIFCBlack -> 0
        BIFCBlue -> 1
        BIFCGreen -> 2
        BIFCRed -> 3
        BIFCWhite -> 4
        BIFCYellow -> 5
        BurnInFontColor' name -> (error . showText) $ "Unknown BurnInFontColor: " <> original name

-- | Represents the bounds of /known/ $BurnInFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInFontColor where
    minBound = BIFCBlack
    maxBound = BIFCYellow

instance Hashable     BurnInFontColor
instance NFData       BurnInFontColor
instance ToByteString BurnInFontColor
instance ToQuery      BurnInFontColor
instance ToHeader     BurnInFontColor

instance ToJSON BurnInFontColor where
    toJSON = toJSONText

instance FromJSON BurnInFontColor where
    parseJSON = parseJSONText "BurnInFontColor"
