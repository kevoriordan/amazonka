{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor (
  DvbSubtitleBackgroundColor (
    ..
    , Black
    , None
    , White
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies the color of the rectangle behind the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern Black :: DvbSubtitleBackgroundColor
pattern Black = DvbSubtitleBackgroundColor' "BLACK"

pattern None :: DvbSubtitleBackgroundColor
pattern None = DvbSubtitleBackgroundColor' "NONE"

pattern White :: DvbSubtitleBackgroundColor
pattern White = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  Black,
  None,
  White,
  DvbSubtitleBackgroundColor' #-}

instance FromText DvbSubtitleBackgroundColor where
    parser = (DvbSubtitleBackgroundColor' . mk) <$> takeText

instance ToText DvbSubtitleBackgroundColor where
    toText (DvbSubtitleBackgroundColor' ci) = original ci

-- | Represents an enum of /known/ $DvbSubtitleBackgroundColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubtitleBackgroundColor where
    toEnum i = case i of
        0 -> Black
        1 -> None
        2 -> White
        _ -> (error . showText) $ "Unknown index for DvbSubtitleBackgroundColor: " <> toText i
    fromEnum x = case x of
        Black -> 0
        None -> 1
        White -> 2
        DvbSubtitleBackgroundColor' name -> (error . showText) $ "Unknown DvbSubtitleBackgroundColor: " <> original name

-- | Represents the bounds of /known/ $DvbSubtitleBackgroundColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubtitleBackgroundColor where
    minBound = Black
    maxBound = White

instance Hashable     DvbSubtitleBackgroundColor
instance NFData       DvbSubtitleBackgroundColor
instance ToByteString DvbSubtitleBackgroundColor
instance ToQuery      DvbSubtitleBackgroundColor
instance ToHeader     DvbSubtitleBackgroundColor

instance ToJSON DvbSubtitleBackgroundColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleBackgroundColor where
    parseJSON = parseJSONText "DvbSubtitleBackgroundColor"
