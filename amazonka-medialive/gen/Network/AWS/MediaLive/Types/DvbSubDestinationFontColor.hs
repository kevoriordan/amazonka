{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationFontColor (
  DvbSubDestinationFontColor (
    ..
    , DSDFCBlack
    , DSDFCBlue
    , DSDFCGreen
    , DSDFCRed
    , DSDFCWhite
    , DSDFCYellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Dvb Sub Destination Font Color
data DvbSubDestinationFontColor = DvbSubDestinationFontColor' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern DSDFCBlack :: DvbSubDestinationFontColor
pattern DSDFCBlack = DvbSubDestinationFontColor' "BLACK"

pattern DSDFCBlue :: DvbSubDestinationFontColor
pattern DSDFCBlue = DvbSubDestinationFontColor' "BLUE"

pattern DSDFCGreen :: DvbSubDestinationFontColor
pattern DSDFCGreen = DvbSubDestinationFontColor' "GREEN"

pattern DSDFCRed :: DvbSubDestinationFontColor
pattern DSDFCRed = DvbSubDestinationFontColor' "RED"

pattern DSDFCWhite :: DvbSubDestinationFontColor
pattern DSDFCWhite = DvbSubDestinationFontColor' "WHITE"

pattern DSDFCYellow :: DvbSubDestinationFontColor
pattern DSDFCYellow = DvbSubDestinationFontColor' "YELLOW"

{-# COMPLETE
  DSDFCBlack,
  DSDFCBlue,
  DSDFCGreen,
  DSDFCRed,
  DSDFCWhite,
  DSDFCYellow,
  DvbSubDestinationFontColor' #-}

instance FromText DvbSubDestinationFontColor where
    parser = (DvbSubDestinationFontColor' . mk) <$> takeText

instance ToText DvbSubDestinationFontColor where
    toText (DvbSubDestinationFontColor' ci) = original ci

-- | Represents an enum of /known/ $DvbSubDestinationFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubDestinationFontColor where
    toEnum i = case i of
        0 -> DSDFCBlack
        1 -> DSDFCBlue
        2 -> DSDFCGreen
        3 -> DSDFCRed
        4 -> DSDFCWhite
        5 -> DSDFCYellow
        _ -> (error . showText) $ "Unknown index for DvbSubDestinationFontColor: " <> toText i
    fromEnum x = case x of
        DSDFCBlack -> 0
        DSDFCBlue -> 1
        DSDFCGreen -> 2
        DSDFCRed -> 3
        DSDFCWhite -> 4
        DSDFCYellow -> 5
        DvbSubDestinationFontColor' name -> (error . showText) $ "Unknown DvbSubDestinationFontColor: " <> original name

-- | Represents the bounds of /known/ $DvbSubDestinationFontColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubDestinationFontColor where
    minBound = DSDFCBlack
    maxBound = DSDFCYellow

instance Hashable     DvbSubDestinationFontColor
instance NFData       DvbSubDestinationFontColor
instance ToByteString DvbSubDestinationFontColor
instance ToQuery      DvbSubDestinationFontColor
instance ToHeader     DvbSubDestinationFontColor

instance ToJSON DvbSubDestinationFontColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationFontColor where
    parseJSON = parseJSONText "DvbSubDestinationFontColor"
