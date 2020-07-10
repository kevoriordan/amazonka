{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInOutlineColor (
  BurnInOutlineColor (
    ..
    , BIOCBlack
    , BIOCBlue
    , BIOCGreen
    , BIOCRed
    , BIOCWhite
    , BIOCYellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Burn In Outline Color
data BurnInOutlineColor = BurnInOutlineColor' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern BIOCBlack :: BurnInOutlineColor
pattern BIOCBlack = BurnInOutlineColor' "BLACK"

pattern BIOCBlue :: BurnInOutlineColor
pattern BIOCBlue = BurnInOutlineColor' "BLUE"

pattern BIOCGreen :: BurnInOutlineColor
pattern BIOCGreen = BurnInOutlineColor' "GREEN"

pattern BIOCRed :: BurnInOutlineColor
pattern BIOCRed = BurnInOutlineColor' "RED"

pattern BIOCWhite :: BurnInOutlineColor
pattern BIOCWhite = BurnInOutlineColor' "WHITE"

pattern BIOCYellow :: BurnInOutlineColor
pattern BIOCYellow = BurnInOutlineColor' "YELLOW"

{-# COMPLETE
  BIOCBlack,
  BIOCBlue,
  BIOCGreen,
  BIOCRed,
  BIOCWhite,
  BIOCYellow,
  BurnInOutlineColor' #-}

instance FromText BurnInOutlineColor where
    parser = (BurnInOutlineColor' . mk) <$> takeText

instance ToText BurnInOutlineColor where
    toText (BurnInOutlineColor' ci) = original ci

-- | Represents an enum of /known/ $BurnInOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInOutlineColor where
    toEnum i = case i of
        0 -> BIOCBlack
        1 -> BIOCBlue
        2 -> BIOCGreen
        3 -> BIOCRed
        4 -> BIOCWhite
        5 -> BIOCYellow
        _ -> (error . showText) $ "Unknown index for BurnInOutlineColor: " <> toText i
    fromEnum x = case x of
        BIOCBlack -> 0
        BIOCBlue -> 1
        BIOCGreen -> 2
        BIOCRed -> 3
        BIOCWhite -> 4
        BIOCYellow -> 5
        BurnInOutlineColor' name -> (error . showText) $ "Unknown BurnInOutlineColor: " <> original name

-- | Represents the bounds of /known/ $BurnInOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInOutlineColor where
    minBound = BIOCBlack
    maxBound = BIOCYellow

instance Hashable     BurnInOutlineColor
instance NFData       BurnInOutlineColor
instance ToByteString BurnInOutlineColor
instance ToQuery      BurnInOutlineColor
instance ToHeader     BurnInOutlineColor

instance ToJSON BurnInOutlineColor where
    toJSON = toJSONText

instance FromJSON BurnInOutlineColor where
    parseJSON = parseJSONText "BurnInOutlineColor"
