{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RangeMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.RangeMode (
  RangeMode (
    ..
    , Exclusive
    , First
    , Inclusive
    , Last
    , LastBeforeMissingValues
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RangeMode = RangeMode' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Exclusive :: RangeMode
pattern Exclusive = RangeMode' "EXCLUSIVE"

pattern First :: RangeMode
pattern First = RangeMode' "FIRST"

pattern Inclusive :: RangeMode
pattern Inclusive = RangeMode' "INCLUSIVE"

pattern Last :: RangeMode
pattern Last = RangeMode' "LAST"

pattern LastBeforeMissingValues :: RangeMode
pattern LastBeforeMissingValues = RangeMode' "LAST_BEFORE_MISSING_VALUES"

{-# COMPLETE
  Exclusive,
  First,
  Inclusive,
  Last,
  LastBeforeMissingValues,
  RangeMode' #-}

instance FromText RangeMode where
    parser = (RangeMode' . mk) <$> takeText

instance ToText RangeMode where
    toText (RangeMode' ci) = original ci

-- | Represents an enum of /known/ $RangeMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RangeMode where
    toEnum i = case i of
        0 -> Exclusive
        1 -> First
        2 -> Inclusive
        3 -> Last
        4 -> LastBeforeMissingValues
        _ -> (error . showText) $ "Unknown index for RangeMode: " <> toText i
    fromEnum x = case x of
        Exclusive -> 0
        First -> 1
        Inclusive -> 2
        Last -> 3
        LastBeforeMissingValues -> 4
        RangeMode' name -> (error . showText) $ "Unknown RangeMode: " <> original name

-- | Represents the bounds of /known/ $RangeMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RangeMode where
    minBound = Exclusive
    maxBound = LastBeforeMissingValues

instance Hashable     RangeMode
instance NFData       RangeMode
instance ToByteString RangeMode
instance ToQuery      RangeMode
instance ToHeader     RangeMode

instance ToJSON RangeMode where
    toJSON = toJSONText
