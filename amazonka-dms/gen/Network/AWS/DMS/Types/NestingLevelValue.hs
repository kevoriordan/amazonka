{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.NestingLevelValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.NestingLevelValue (
  NestingLevelValue (
    ..
    , None
    , One
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NestingLevelValue = NestingLevelValue' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern None :: NestingLevelValue
pattern None = NestingLevelValue' "none"

pattern One :: NestingLevelValue
pattern One = NestingLevelValue' "one"

{-# COMPLETE
  None,
  One,
  NestingLevelValue' #-}

instance FromText NestingLevelValue where
    parser = (NestingLevelValue' . mk) <$> takeText

instance ToText NestingLevelValue where
    toText (NestingLevelValue' ci) = original ci

-- | Represents an enum of /known/ $NestingLevelValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NestingLevelValue where
    toEnum i = case i of
        0 -> None
        1 -> One
        _ -> (error . showText) $ "Unknown index for NestingLevelValue: " <> toText i
    fromEnum x = case x of
        None -> 0
        One -> 1
        NestingLevelValue' name -> (error . showText) $ "Unknown NestingLevelValue: " <> original name

-- | Represents the bounds of /known/ $NestingLevelValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NestingLevelValue where
    minBound = None
    maxBound = One

instance Hashable     NestingLevelValue
instance NFData       NestingLevelValue
instance ToByteString NestingLevelValue
instance ToQuery      NestingLevelValue
instance ToHeader     NestingLevelValue

instance ToJSON NestingLevelValue where
    toJSON = toJSONText

instance FromJSON NestingLevelValue where
    parseJSON = parseJSONText "NestingLevelValue"
