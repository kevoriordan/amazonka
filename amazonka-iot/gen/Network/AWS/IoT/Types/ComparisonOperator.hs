{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ComparisonOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ComparisonOperator (
  ComparisonOperator (
    ..
    , GreaterThan
    , GreaterThanEquals
    , InCidrSet
    , InPortSet
    , LessThan
    , LessThanEquals
    , NotInCidrSet
    , NotInPortSet
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComparisonOperator = ComparisonOperator' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern GreaterThan :: ComparisonOperator
pattern GreaterThan = ComparisonOperator' "greater-than"

pattern GreaterThanEquals :: ComparisonOperator
pattern GreaterThanEquals = ComparisonOperator' "greater-than-equals"

pattern InCidrSet :: ComparisonOperator
pattern InCidrSet = ComparisonOperator' "in-cidr-set"

pattern InPortSet :: ComparisonOperator
pattern InPortSet = ComparisonOperator' "in-port-set"

pattern LessThan :: ComparisonOperator
pattern LessThan = ComparisonOperator' "less-than"

pattern LessThanEquals :: ComparisonOperator
pattern LessThanEquals = ComparisonOperator' "less-than-equals"

pattern NotInCidrSet :: ComparisonOperator
pattern NotInCidrSet = ComparisonOperator' "not-in-cidr-set"

pattern NotInPortSet :: ComparisonOperator
pattern NotInPortSet = ComparisonOperator' "not-in-port-set"

{-# COMPLETE
  GreaterThan,
  GreaterThanEquals,
  InCidrSet,
  InPortSet,
  LessThan,
  LessThanEquals,
  NotInCidrSet,
  NotInPortSet,
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
        1 -> GreaterThanEquals
        2 -> InCidrSet
        3 -> InPortSet
        4 -> LessThan
        5 -> LessThanEquals
        6 -> NotInCidrSet
        7 -> NotInPortSet
        _ -> (error . showText) $ "Unknown index for ComparisonOperator: " <> toText i
    fromEnum x = case x of
        GreaterThan -> 0
        GreaterThanEquals -> 1
        InCidrSet -> 2
        InPortSet -> 3
        LessThan -> 4
        LessThanEquals -> 5
        NotInCidrSet -> 6
        NotInPortSet -> 7
        ComparisonOperator' name -> (error . showText) $ "Unknown ComparisonOperator: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperator where
    minBound = GreaterThan
    maxBound = NotInPortSet

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"
