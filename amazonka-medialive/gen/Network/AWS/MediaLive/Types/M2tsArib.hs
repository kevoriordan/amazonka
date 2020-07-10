{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsArib
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsArib (
  M2tsArib (
    ..
    , MADisabled
    , MAEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | M2ts Arib
data M2tsArib = M2tsArib' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern MADisabled :: M2tsArib
pattern MADisabled = M2tsArib' "DISABLED"

pattern MAEnabled :: M2tsArib
pattern MAEnabled = M2tsArib' "ENABLED"

{-# COMPLETE
  MADisabled,
  MAEnabled,
  M2tsArib' #-}

instance FromText M2tsArib where
    parser = (M2tsArib' . mk) <$> takeText

instance ToText M2tsArib where
    toText (M2tsArib' ci) = original ci

-- | Represents an enum of /known/ $M2tsArib.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsArib where
    toEnum i = case i of
        0 -> MADisabled
        1 -> MAEnabled
        _ -> (error . showText) $ "Unknown index for M2tsArib: " <> toText i
    fromEnum x = case x of
        MADisabled -> 0
        MAEnabled -> 1
        M2tsArib' name -> (error . showText) $ "Unknown M2tsArib: " <> original name

-- | Represents the bounds of /known/ $M2tsArib.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsArib where
    minBound = MADisabled
    maxBound = MAEnabled

instance Hashable     M2tsArib
instance NFData       M2tsArib
instance ToByteString M2tsArib
instance ToQuery      M2tsArib
instance ToHeader     M2tsArib

instance ToJSON M2tsArib where
    toJSON = toJSONText

instance FromJSON M2tsArib where
    parseJSON = parseJSONText "M2tsArib"
