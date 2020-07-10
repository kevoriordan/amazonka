{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInAlignment (
  BurnInAlignment (
    ..
    , BIACentered
    , BIALeft'
    , BIASmart
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Burn In Alignment
data BurnInAlignment = BurnInAlignment' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern BIACentered :: BurnInAlignment
pattern BIACentered = BurnInAlignment' "CENTERED"

pattern BIALeft' :: BurnInAlignment
pattern BIALeft' = BurnInAlignment' "LEFT"

pattern BIASmart :: BurnInAlignment
pattern BIASmart = BurnInAlignment' "SMART"

{-# COMPLETE
  BIACentered,
  BIALeft',
  BIASmart,
  BurnInAlignment' #-}

instance FromText BurnInAlignment where
    parser = (BurnInAlignment' . mk) <$> takeText

instance ToText BurnInAlignment where
    toText (BurnInAlignment' ci) = original ci

-- | Represents an enum of /known/ $BurnInAlignment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInAlignment where
    toEnum i = case i of
        0 -> BIACentered
        1 -> BIALeft'
        2 -> BIASmart
        _ -> (error . showText) $ "Unknown index for BurnInAlignment: " <> toText i
    fromEnum x = case x of
        BIACentered -> 0
        BIALeft' -> 1
        BIASmart -> 2
        BurnInAlignment' name -> (error . showText) $ "Unknown BurnInAlignment: " <> original name

-- | Represents the bounds of /known/ $BurnInAlignment.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInAlignment where
    minBound = BIACentered
    maxBound = BIASmart

instance Hashable     BurnInAlignment
instance NFData       BurnInAlignment
instance ToByteString BurnInAlignment
instance ToQuery      BurnInAlignment
instance ToHeader     BurnInAlignment

instance ToJSON BurnInAlignment where
    toJSON = toJSONText

instance FromJSON BurnInAlignment where
    parseJSON = parseJSONText "BurnInAlignment"
