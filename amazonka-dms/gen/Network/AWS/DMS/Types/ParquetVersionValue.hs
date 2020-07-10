{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ParquetVersionValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ParquetVersionValue (
  ParquetVersionValue (
    ..
    , Parquet10
    , Parquet20
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ParquetVersionValue = ParquetVersionValue' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Parquet10 :: ParquetVersionValue
pattern Parquet10 = ParquetVersionValue' "parquet-1-0"

pattern Parquet20 :: ParquetVersionValue
pattern Parquet20 = ParquetVersionValue' "parquet-2-0"

{-# COMPLETE
  Parquet10,
  Parquet20,
  ParquetVersionValue' #-}

instance FromText ParquetVersionValue where
    parser = (ParquetVersionValue' . mk) <$> takeText

instance ToText ParquetVersionValue where
    toText (ParquetVersionValue' ci) = original ci

-- | Represents an enum of /known/ $ParquetVersionValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ParquetVersionValue where
    toEnum i = case i of
        0 -> Parquet10
        1 -> Parquet20
        _ -> (error . showText) $ "Unknown index for ParquetVersionValue: " <> toText i
    fromEnum x = case x of
        Parquet10 -> 0
        Parquet20 -> 1
        ParquetVersionValue' name -> (error . showText) $ "Unknown ParquetVersionValue: " <> original name

-- | Represents the bounds of /known/ $ParquetVersionValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ParquetVersionValue where
    minBound = Parquet10
    maxBound = Parquet20

instance Hashable     ParquetVersionValue
instance NFData       ParquetVersionValue
instance ToByteString ParquetVersionValue
instance ToQuery      ParquetVersionValue
instance ToHeader     ParquetVersionValue

instance ToJSON ParquetVersionValue where
    toJSON = toJSONText

instance FromJSON ParquetVersionValue where
    parseJSON = parseJSONText "ParquetVersionValue"
