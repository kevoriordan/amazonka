{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInShadowColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInShadowColor (
  BurnInShadowColor (
    ..
    , BISCBlack
    , BISCNone
    , BISCWhite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Burn In Shadow Color
data BurnInShadowColor = BurnInShadowColor' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern BISCBlack :: BurnInShadowColor
pattern BISCBlack = BurnInShadowColor' "BLACK"

pattern BISCNone :: BurnInShadowColor
pattern BISCNone = BurnInShadowColor' "NONE"

pattern BISCWhite :: BurnInShadowColor
pattern BISCWhite = BurnInShadowColor' "WHITE"

{-# COMPLETE
  BISCBlack,
  BISCNone,
  BISCWhite,
  BurnInShadowColor' #-}

instance FromText BurnInShadowColor where
    parser = (BurnInShadowColor' . mk) <$> takeText

instance ToText BurnInShadowColor where
    toText (BurnInShadowColor' ci) = original ci

-- | Represents an enum of /known/ $BurnInShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInShadowColor where
    toEnum i = case i of
        0 -> BISCBlack
        1 -> BISCNone
        2 -> BISCWhite
        _ -> (error . showText) $ "Unknown index for BurnInShadowColor: " <> toText i
    fromEnum x = case x of
        BISCBlack -> 0
        BISCNone -> 1
        BISCWhite -> 2
        BurnInShadowColor' name -> (error . showText) $ "Unknown BurnInShadowColor: " <> original name

-- | Represents the bounds of /known/ $BurnInShadowColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInShadowColor where
    minBound = BISCBlack
    maxBound = BISCWhite

instance Hashable     BurnInShadowColor
instance NFData       BurnInShadowColor
instance ToByteString BurnInShadowColor
instance ToQuery      BurnInShadowColor
instance ToHeader     BurnInShadowColor

instance ToJSON BurnInShadowColor where
    toJSON = toJSONText

instance FromJSON BurnInShadowColor where
    parseJSON = parseJSONText "BurnInShadowColor"
