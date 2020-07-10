{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor (
  BurninSubtitleShadowColor (
    ..
    , BSSCBlack
    , BSSCNone
    , BSSCWhite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data BurninSubtitleShadowColor = BurninSubtitleShadowColor' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern BSSCBlack :: BurninSubtitleShadowColor
pattern BSSCBlack = BurninSubtitleShadowColor' "BLACK"

pattern BSSCNone :: BurninSubtitleShadowColor
pattern BSSCNone = BurninSubtitleShadowColor' "NONE"

pattern BSSCWhite :: BurninSubtitleShadowColor
pattern BSSCWhite = BurninSubtitleShadowColor' "WHITE"

{-# COMPLETE
  BSSCBlack,
  BSSCNone,
  BSSCWhite,
  BurninSubtitleShadowColor' #-}

instance FromText BurninSubtitleShadowColor where
    parser = (BurninSubtitleShadowColor' . mk) <$> takeText

instance ToText BurninSubtitleShadowColor where
    toText (BurninSubtitleShadowColor' ci) = original ci

-- | Represents an enum of /known/ $BurninSubtitleShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurninSubtitleShadowColor where
    toEnum i = case i of
        0 -> BSSCBlack
        1 -> BSSCNone
        2 -> BSSCWhite
        _ -> (error . showText) $ "Unknown index for BurninSubtitleShadowColor: " <> toText i
    fromEnum x = case x of
        BSSCBlack -> 0
        BSSCNone -> 1
        BSSCWhite -> 2
        BurninSubtitleShadowColor' name -> (error . showText) $ "Unknown BurninSubtitleShadowColor: " <> original name

-- | Represents the bounds of /known/ $BurninSubtitleShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurninSubtitleShadowColor where
    minBound = BSSCBlack
    maxBound = BSSCWhite

instance Hashable     BurninSubtitleShadowColor
instance NFData       BurninSubtitleShadowColor
instance ToByteString BurninSubtitleShadowColor
instance ToQuery      BurninSubtitleShadowColor
instance ToHeader     BurninSubtitleShadowColor

instance ToJSON BurninSubtitleShadowColor where
    toJSON = toJSONText

instance FromJSON BurninSubtitleShadowColor where
    parseJSON = parseJSONText "BurninSubtitleShadowColor"
