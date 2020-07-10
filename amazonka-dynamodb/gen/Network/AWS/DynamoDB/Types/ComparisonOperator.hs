{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ComparisonOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ComparisonOperator (
  ComparisonOperator (
    ..
    , BeginsWith
    , Between
    , Contains
    , EQ'
    , GE
    , GT'
    , IN
    , LE
    , LT'
    , NE
    , NotContains
    , NotNull
    , Null
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComparisonOperator = ComparisonOperator' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern BeginsWith :: ComparisonOperator
pattern BeginsWith = ComparisonOperator' "BEGINS_WITH"

pattern Between :: ComparisonOperator
pattern Between = ComparisonOperator' "BETWEEN"

pattern Contains :: ComparisonOperator
pattern Contains = ComparisonOperator' "CONTAINS"

pattern EQ' :: ComparisonOperator
pattern EQ' = ComparisonOperator' "EQ"

pattern GE :: ComparisonOperator
pattern GE = ComparisonOperator' "GE"

pattern GT' :: ComparisonOperator
pattern GT' = ComparisonOperator' "GT"

pattern IN :: ComparisonOperator
pattern IN = ComparisonOperator' "IN"

pattern LE :: ComparisonOperator
pattern LE = ComparisonOperator' "LE"

pattern LT' :: ComparisonOperator
pattern LT' = ComparisonOperator' "LT"

pattern NE :: ComparisonOperator
pattern NE = ComparisonOperator' "NE"

pattern NotContains :: ComparisonOperator
pattern NotContains = ComparisonOperator' "NOT_CONTAINS"

pattern NotNull :: ComparisonOperator
pattern NotNull = ComparisonOperator' "NOT_NULL"

pattern Null :: ComparisonOperator
pattern Null = ComparisonOperator' "NULL"

{-# COMPLETE
  BeginsWith,
  Between,
  Contains,
  EQ',
  GE,
  GT',
  IN,
  LE,
  LT',
  NE,
  NotContains,
  NotNull,
  Null,
  ComparisonOperator' #-}

instance FromText ComparisonOperator where
    parser = (ComparisonOperator' . mk) <$> takeText

instance ToText ComparisonOperator where
    toText (ComparisonOperator' ci) = original ci

-- | Represents an enum of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComparisonOperator where
    toEnum i = case i of
        0 -> BeginsWith
        1 -> Between
        2 -> Contains
        3 -> EQ'
        4 -> GE
        5 -> GT'
        6 -> IN
        7 -> LE
        8 -> LT'
        9 -> NE
        10 -> NotContains
        11 -> NotNull
        12 -> Null
        _ -> (error . showText) $ "Unknown index for ComparisonOperator: " <> toText i
    fromEnum x = case x of
        BeginsWith -> 0
        Between -> 1
        Contains -> 2
        EQ' -> 3
        GE -> 4
        GT' -> 5
        IN -> 6
        LE -> 7
        LT' -> 8
        NE -> 9
        NotContains -> 10
        NotNull -> 11
        Null -> 12
        ComparisonOperator' name -> (error . showText) $ "Unknown ComparisonOperator: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperator where
    minBound = BeginsWith
    maxBound = Null

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText
