{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitleFontColor (
  DvbSubtitleFontColor (
    ..
    , DSFCBlack
    , DSFCBlue
    , DSFCGreen
    , DSFCRed
    , DSFCWhite
    , DSFCYellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
data DvbSubtitleFontColor = DvbSubtitleFontColor' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern DSFCBlack :: DvbSubtitleFontColor
pattern DSFCBlack = DvbSubtitleFontColor' "BLACK"

pattern DSFCBlue :: DvbSubtitleFontColor
pattern DSFCBlue = DvbSubtitleFontColor' "BLUE"

pattern DSFCGreen :: DvbSubtitleFontColor
pattern DSFCGreen = DvbSubtitleFontColor' "GREEN"

pattern DSFCRed :: DvbSubtitleFontColor
pattern DSFCRed = DvbSubtitleFontColor' "RED"

pattern DSFCWhite :: DvbSubtitleFontColor
pattern DSFCWhite = DvbSubtitleFontColor' "WHITE"

pattern DSFCYellow :: DvbSubtitleFontColor
pattern DSFCYellow = DvbSubtitleFontColor' "YELLOW"

{-# COMPLETE
  DSFCBlack,
  DSFCBlue,
  DSFCGreen,
  DSFCRed,
  DSFCWhite,
  DSFCYellow,
  DvbSubtitleFontColor' #-}

instance FromText DvbSubtitleFontColor where
    parser = (DvbSubtitleFontColor' . mk) <$> takeText

instance ToText DvbSubtitleFontColor where
    toText (DvbSubtitleFontColor' ci) = original ci

-- | Represents an enum of /known/ $DvbSubtitleFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubtitleFontColor where
    toEnum i = case i of
        0 -> DSFCBlack
        1 -> DSFCBlue
        2 -> DSFCGreen
        3 -> DSFCRed
        4 -> DSFCWhite
        5 -> DSFCYellow
        _ -> (error . showText) $ "Unknown index for DvbSubtitleFontColor: " <> toText i
    fromEnum x = case x of
        DSFCBlack -> 0
        DSFCBlue -> 1
        DSFCGreen -> 2
        DSFCRed -> 3
        DSFCWhite -> 4
        DSFCYellow -> 5
        DvbSubtitleFontColor' name -> (error . showText) $ "Unknown DvbSubtitleFontColor: " <> original name

-- | Represents the bounds of /known/ $DvbSubtitleFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubtitleFontColor where
    minBound = DSFCBlack
    maxBound = DSFCYellow

instance Hashable     DvbSubtitleFontColor
instance NFData       DvbSubtitleFontColor
instance ToByteString DvbSubtitleFontColor
instance ToQuery      DvbSubtitleFontColor
instance ToHeader     DvbSubtitleFontColor

instance ToJSON DvbSubtitleFontColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleFontColor where
    parseJSON = parseJSONText "DvbSubtitleFontColor"
