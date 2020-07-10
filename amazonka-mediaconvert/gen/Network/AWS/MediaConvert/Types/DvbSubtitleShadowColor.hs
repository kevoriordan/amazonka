{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor (
  DvbSubtitleShadowColor (
    ..
    , DSSCBlack
    , DSSCNone
    , DSSCWhite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data DvbSubtitleShadowColor = DvbSubtitleShadowColor' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern DSSCBlack :: DvbSubtitleShadowColor
pattern DSSCBlack = DvbSubtitleShadowColor' "BLACK"

pattern DSSCNone :: DvbSubtitleShadowColor
pattern DSSCNone = DvbSubtitleShadowColor' "NONE"

pattern DSSCWhite :: DvbSubtitleShadowColor
pattern DSSCWhite = DvbSubtitleShadowColor' "WHITE"

{-# COMPLETE
  DSSCBlack,
  DSSCNone,
  DSSCWhite,
  DvbSubtitleShadowColor' #-}

instance FromText DvbSubtitleShadowColor where
    parser = (DvbSubtitleShadowColor' . mk) <$> takeText

instance ToText DvbSubtitleShadowColor where
    toText (DvbSubtitleShadowColor' ci) = original ci

-- | Represents an enum of /known/ $DvbSubtitleShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DvbSubtitleShadowColor where
    toEnum i = case i of
        0 -> DSSCBlack
        1 -> DSSCNone
        2 -> DSSCWhite
        _ -> (error . showText) $ "Unknown index for DvbSubtitleShadowColor: " <> toText i
    fromEnum x = case x of
        DSSCBlack -> 0
        DSSCNone -> 1
        DSSCWhite -> 2
        DvbSubtitleShadowColor' name -> (error . showText) $ "Unknown DvbSubtitleShadowColor: " <> original name

-- | Represents the bounds of /known/ $DvbSubtitleShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DvbSubtitleShadowColor where
    minBound = DSSCBlack
    maxBound = DSSCWhite

instance Hashable     DvbSubtitleShadowColor
instance NFData       DvbSubtitleShadowColor
instance ToByteString DvbSubtitleShadowColor
instance ToQuery      DvbSubtitleShadowColor
instance ToHeader     DvbSubtitleShadowColor

instance ToJSON DvbSubtitleShadowColor where
    toJSON = toJSONText

instance FromJSON DvbSubtitleShadowColor where
    parseJSON = parseJSONText "DvbSubtitleShadowColor"
