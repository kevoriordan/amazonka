{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor (
  DvbSubDestinationOutlineColor (
    ..
    , Black
    , Blue
    , Green
    , Red
    , White
    , Yellow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Dvb Sub Destination Outline Color
data DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Black :: DvbSubDestinationOutlineColor
pattern Black = DvbSubDestinationOutlineColor' "BLACK"

pattern Blue :: DvbSubDestinationOutlineColor
pattern Blue = DvbSubDestinationOutlineColor' "BLUE"

pattern Green :: DvbSubDestinationOutlineColor
pattern Green = DvbSubDestinationOutlineColor' "GREEN"

pattern Red :: DvbSubDestinationOutlineColor
pattern Red = DvbSubDestinationOutlineColor' "RED"

pattern White :: DvbSubDestinationOutlineColor
pattern White = DvbSubDestinationOutlineColor' "WHITE"

pattern Yellow :: DvbSubDestinationOutlineColor
pattern Yellow = DvbSubDestinationOutlineColor' "YELLOW"

{-# COMPLETE
  Black,
  Blue,
  Green,
  Red,
  White,
  Yellow,
  DvbSubDestinationOutlineColor' #-}

instance FromText DvbSubDestinationOutlineColor where
    parser = (DvbSubDestinationOutlineColor' . mk) <$> takeText

instance ToText DvbSubDestinationOutlineColor where
    toText (DvbSubDestinationOutlineColor' ci) = original ci

-- | Represents an enum of /known/ $DvbSubDestinationOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubDestinationOutlineColor where
    toEnum i = case i of
        0 -> Black
        1 -> Blue
        2 -> Green
        3 -> Red
        4 -> White
        5 -> Yellow
        _ -> (error . showText) $ "Unknown index for DvbSubDestinationOutlineColor: " <> toText i
    fromEnum x = case x of
        Black -> 0
        Blue -> 1
        Green -> 2
        Red -> 3
        White -> 4
        Yellow -> 5
        DvbSubDestinationOutlineColor' name -> (error . showText) $ "Unknown DvbSubDestinationOutlineColor: " <> original name

-- | Represents the bounds of /known/ $DvbSubDestinationOutlineColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubDestinationOutlineColor where
    minBound = Black
    maxBound = Yellow

instance Hashable     DvbSubDestinationOutlineColor
instance NFData       DvbSubDestinationOutlineColor
instance ToByteString DvbSubDestinationOutlineColor
instance ToQuery      DvbSubDestinationOutlineColor
instance ToHeader     DvbSubDestinationOutlineColor

instance ToJSON DvbSubDestinationOutlineColor where
    toJSON = toJSONText

instance FromJSON DvbSubDestinationOutlineColor where
    parseJSON = parseJSONText "DvbSubDestinationOutlineColor"
