{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ColumnNullable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.ColumnNullable (
  ColumnNullable (
    ..
    , NotNull
    , Nullable
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ColumnNullable = ColumnNullable' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern NotNull :: ColumnNullable
pattern NotNull = ColumnNullable' "NOT_NULL"

pattern Nullable :: ColumnNullable
pattern Nullable = ColumnNullable' "NULLABLE"

pattern Unknown :: ColumnNullable
pattern Unknown = ColumnNullable' "UNKNOWN"

{-# COMPLETE
  NotNull,
  Nullable,
  Unknown,
  ColumnNullable' #-}

instance FromText ColumnNullable where
    parser = (ColumnNullable' . mk) <$> takeText

instance ToText ColumnNullable where
    toText (ColumnNullable' ci) = original ci

-- | Represents an enum of /known/ $ColumnNullable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ColumnNullable where
    toEnum i = case i of
        0 -> NotNull
        1 -> Nullable
        2 -> Unknown
        _ -> (error . showText) $ "Unknown index for ColumnNullable: " <> toText i
    fromEnum x = case x of
        NotNull -> 0
        Nullable -> 1
        Unknown -> 2
        ColumnNullable' name -> (error . showText) $ "Unknown ColumnNullable: " <> original name

-- | Represents the bounds of /known/ $ColumnNullable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ColumnNullable where
    minBound = NotNull
    maxBound = Unknown

instance Hashable     ColumnNullable
instance NFData       ColumnNullable
instance ToByteString ColumnNullable
instance ToQuery      ColumnNullable
instance ToHeader     ColumnNullable

instance FromJSON ColumnNullable where
    parseJSON = parseJSONText "ColumnNullable"
