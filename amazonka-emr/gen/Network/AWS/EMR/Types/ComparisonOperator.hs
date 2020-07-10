{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComparisonOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ComparisonOperator (
  ComparisonOperator (
    ..
    , GreaterThan
    , GreaterThanOrEqual
    , LessThan
    , LessThanOrEqual
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComparisonOperator = ComparisonOperator' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern GreaterThan :: ComparisonOperator
pattern GreaterThan = ComparisonOperator' "GREATER_THAN"

pattern GreaterThanOrEqual :: ComparisonOperator
pattern GreaterThanOrEqual = ComparisonOperator' "GREATER_THAN_OR_EQUAL"

pattern LessThan :: ComparisonOperator
pattern LessThan = ComparisonOperator' "LESS_THAN"

pattern LessThanOrEqual :: ComparisonOperator
pattern LessThanOrEqual = ComparisonOperator' "LESS_THAN_OR_EQUAL"

{-# COMPLETE
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
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
        0 -> GreaterThan
        1 -> GreaterThanOrEqual
        2 -> LessThan
        3 -> LessThanOrEqual
        _ -> (error . showText) $ "Unknown index for ComparisonOperator: " <> toText i
    fromEnum x = case x of
        GreaterThan -> 0
        GreaterThanOrEqual -> 1
        LessThan -> 2
        LessThanOrEqual -> 3
        ComparisonOperator' name -> (error . showText) $ "Unknown ComparisonOperator: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperator where
    minBound = GreaterThan
    maxBound = LessThanOrEqual

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"
