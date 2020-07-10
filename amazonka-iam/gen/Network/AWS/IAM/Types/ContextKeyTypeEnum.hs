{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ContextKeyTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ContextKeyTypeEnum (
  ContextKeyTypeEnum (
    ..
    , Binary
    , BinaryList
    , Boolean
    , BooleanList
    , Date
    , DateList
    , IP
    , IPList
    , Numeric
    , NumericList
    , String
    , StringList
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContextKeyTypeEnum = ContextKeyTypeEnum' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Binary :: ContextKeyTypeEnum
pattern Binary = ContextKeyTypeEnum' "binary"

pattern BinaryList :: ContextKeyTypeEnum
pattern BinaryList = ContextKeyTypeEnum' "binaryList"

pattern Boolean :: ContextKeyTypeEnum
pattern Boolean = ContextKeyTypeEnum' "boolean"

pattern BooleanList :: ContextKeyTypeEnum
pattern BooleanList = ContextKeyTypeEnum' "booleanList"

pattern Date :: ContextKeyTypeEnum
pattern Date = ContextKeyTypeEnum' "date"

pattern DateList :: ContextKeyTypeEnum
pattern DateList = ContextKeyTypeEnum' "dateList"

pattern IP :: ContextKeyTypeEnum
pattern IP = ContextKeyTypeEnum' "ip"

pattern IPList :: ContextKeyTypeEnum
pattern IPList = ContextKeyTypeEnum' "ipList"

pattern Numeric :: ContextKeyTypeEnum
pattern Numeric = ContextKeyTypeEnum' "numeric"

pattern NumericList :: ContextKeyTypeEnum
pattern NumericList = ContextKeyTypeEnum' "numericList"

pattern String :: ContextKeyTypeEnum
pattern String = ContextKeyTypeEnum' "string"

pattern StringList :: ContextKeyTypeEnum
pattern StringList = ContextKeyTypeEnum' "stringList"

{-# COMPLETE
  Binary,
  BinaryList,
  Boolean,
  BooleanList,
  Date,
  DateList,
  IP,
  IPList,
  Numeric,
  NumericList,
  String,
  StringList,
  ContextKeyTypeEnum' #-}

instance FromText ContextKeyTypeEnum where
    parser = (ContextKeyTypeEnum' . mk) <$> takeText

instance ToText ContextKeyTypeEnum where
    toText (ContextKeyTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ContextKeyTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContextKeyTypeEnum where
    toEnum i = case i of
        0 -> Binary
        1 -> BinaryList
        2 -> Boolean
        3 -> BooleanList
        4 -> Date
        5 -> DateList
        6 -> IP
        7 -> IPList
        8 -> Numeric
        9 -> NumericList
        10 -> String
        11 -> StringList
        _ -> (error . showText) $ "Unknown index for ContextKeyTypeEnum: " <> toText i
    fromEnum x = case x of
        Binary -> 0
        BinaryList -> 1
        Boolean -> 2
        BooleanList -> 3
        Date -> 4
        DateList -> 5
        IP -> 6
        IPList -> 7
        Numeric -> 8
        NumericList -> 9
        String -> 10
        StringList -> 11
        ContextKeyTypeEnum' name -> (error . showText) $ "Unknown ContextKeyTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ContextKeyTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContextKeyTypeEnum where
    minBound = Binary
    maxBound = StringList

instance Hashable     ContextKeyTypeEnum
instance NFData       ContextKeyTypeEnum
instance ToByteString ContextKeyTypeEnum
instance ToQuery      ContextKeyTypeEnum
instance ToHeader     ContextKeyTypeEnum
