{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.BurninSubtitleOutlineColor (
  BurninSubtitleOutlineColor (
    ..
    , BSOCBlack
    , BSOCBlue
    , BSOCGreen
    , BSOCRed
    , BSOCWhite
    , BSOCYellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data BurninSubtitleOutlineColor = BurninSubtitleOutlineColor' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern BSOCBlack :: BurninSubtitleOutlineColor
pattern BSOCBlack = BurninSubtitleOutlineColor' "BLACK"

pattern BSOCBlue :: BurninSubtitleOutlineColor
pattern BSOCBlue = BurninSubtitleOutlineColor' "BLUE"

pattern BSOCGreen :: BurninSubtitleOutlineColor
pattern BSOCGreen = BurninSubtitleOutlineColor' "GREEN"

pattern BSOCRed :: BurninSubtitleOutlineColor
pattern BSOCRed = BurninSubtitleOutlineColor' "RED"

pattern BSOCWhite :: BurninSubtitleOutlineColor
pattern BSOCWhite = BurninSubtitleOutlineColor' "WHITE"

pattern BSOCYellow :: BurninSubtitleOutlineColor
pattern BSOCYellow = BurninSubtitleOutlineColor' "YELLOW"

{-# COMPLETE
  BSOCBlack,
  BSOCBlue,
  BSOCGreen,
  BSOCRed,
  BSOCWhite,
  BSOCYellow,
  BurninSubtitleOutlineColor' #-}

instance FromText BurninSubtitleOutlineColor where
    parser = (BurninSubtitleOutlineColor' . mk) <$> takeText

instance ToText BurninSubtitleOutlineColor where
    toText (BurninSubtitleOutlineColor' ci) = original ci

-- | Represents an enum of /known/ $BurninSubtitleOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurninSubtitleOutlineColor where
    toEnum i = case i of
        0 -> BSOCBlack
        1 -> BSOCBlue
        2 -> BSOCGreen
        3 -> BSOCRed
        4 -> BSOCWhite
        5 -> BSOCYellow
        _ -> (error . showText) $ "Unknown index for BurninSubtitleOutlineColor: " <> toText i
    fromEnum x = case x of
        BSOCBlack -> 0
        BSOCBlue -> 1
        BSOCGreen -> 2
        BSOCRed -> 3
        BSOCWhite -> 4
        BSOCYellow -> 5
        BurninSubtitleOutlineColor' name -> (error . showText) $ "Unknown BurninSubtitleOutlineColor: " <> original name

-- | Represents the bounds of /known/ $BurninSubtitleOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurninSubtitleOutlineColor where
    minBound = BSOCBlack
    maxBound = BSOCYellow

instance Hashable     BurninSubtitleOutlineColor
instance NFData       BurninSubtitleOutlineColor
instance ToByteString BurninSubtitleOutlineColor
instance ToQuery      BurninSubtitleOutlineColor
instance ToHeader     BurninSubtitleOutlineColor

instance ToJSON BurninSubtitleOutlineColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleOutlineColor where
    parseJSON = parseJSONText "BurninSubtitleOutlineColor"
