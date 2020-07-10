{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.IndexFieldType (
  IndexFieldType (
    ..
    , Date
    , DateArray
    , Double
    , DoubleArray
    , Int
    , IntArray
    , Latlon
    , Literal
    , LiteralArray
    , Text
    , TextArray
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of field. The valid options for a field depend on the field type. For more information about the supported field types, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
--
--
data IndexFieldType = IndexFieldType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Date :: IndexFieldType
pattern Date = IndexFieldType' "date"

pattern DateArray :: IndexFieldType
pattern DateArray = IndexFieldType' "date-array"

pattern Double :: IndexFieldType
pattern Double = IndexFieldType' "double"

pattern DoubleArray :: IndexFieldType
pattern DoubleArray = IndexFieldType' "double-array"

pattern Int :: IndexFieldType
pattern Int = IndexFieldType' "int"

pattern IntArray :: IndexFieldType
pattern IntArray = IndexFieldType' "int-array"

pattern Latlon :: IndexFieldType
pattern Latlon = IndexFieldType' "latlon"

pattern Literal :: IndexFieldType
pattern Literal = IndexFieldType' "literal"

pattern LiteralArray :: IndexFieldType
pattern LiteralArray = IndexFieldType' "literal-array"

pattern Text :: IndexFieldType
pattern Text = IndexFieldType' "text"

pattern TextArray :: IndexFieldType
pattern TextArray = IndexFieldType' "text-array"

{-# COMPLETE
  Date,
  DateArray,
  Double,
  DoubleArray,
  Int,
  IntArray,
  Latlon,
  Literal,
  LiteralArray,
  Text,
  TextArray,
  IndexFieldType' #-}

instance FromText IndexFieldType where
    parser = (IndexFieldType' . mk) <$> takeText

instance ToText IndexFieldType where
    toText (IndexFieldType' ci) = original ci

-- | Represents an enum of /known/ $IndexFieldType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IndexFieldType where
    toEnum i = case i of
        0 -> Date
        1 -> DateArray
        2 -> Double
        3 -> DoubleArray
        4 -> Int
        5 -> IntArray
        6 -> Latlon
        7 -> Literal
        8 -> LiteralArray
        9 -> Text
        10 -> TextArray
        _ -> (error . showText) $ "Unknown index for IndexFieldType: " <> toText i
    fromEnum x = case x of
        Date -> 0
        DateArray -> 1
        Double -> 2
        DoubleArray -> 3
        Int -> 4
        IntArray -> 5
        Latlon -> 6
        Literal -> 7
        LiteralArray -> 8
        Text -> 9
        TextArray -> 10
        IndexFieldType' name -> (error . showText) $ "Unknown IndexFieldType: " <> original name

-- | Represents the bounds of /known/ $IndexFieldType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IndexFieldType where
    minBound = Date
    maxBound = TextArray

instance Hashable     IndexFieldType
instance NFData       IndexFieldType
instance ToByteString IndexFieldType
instance ToQuery      IndexFieldType
instance ToHeader     IndexFieldType

instance FromXML IndexFieldType where
    parseXML = parseXMLText "IndexFieldType"
