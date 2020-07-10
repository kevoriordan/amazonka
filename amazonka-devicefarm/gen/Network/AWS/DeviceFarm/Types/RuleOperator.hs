{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.RuleOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.RuleOperator (
  RuleOperator (
    ..
    , Contains
    , Equals
    , GreaterThan
    , GreaterThanOrEquals
    , IN
    , LessThan
    , LessThanOrEquals
    , NotIn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RuleOperator = RuleOperator' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Contains :: RuleOperator
pattern Contains = RuleOperator' "CONTAINS"

pattern Equals :: RuleOperator
pattern Equals = RuleOperator' "EQUALS"

pattern GreaterThan :: RuleOperator
pattern GreaterThan = RuleOperator' "GREATER_THAN"

pattern GreaterThanOrEquals :: RuleOperator
pattern GreaterThanOrEquals = RuleOperator' "GREATER_THAN_OR_EQUALS"

pattern IN :: RuleOperator
pattern IN = RuleOperator' "IN"

pattern LessThan :: RuleOperator
pattern LessThan = RuleOperator' "LESS_THAN"

pattern LessThanOrEquals :: RuleOperator
pattern LessThanOrEquals = RuleOperator' "LESS_THAN_OR_EQUALS"

pattern NotIn :: RuleOperator
pattern NotIn = RuleOperator' "NOT_IN"

{-# COMPLETE
  Contains,
  Equals,
  GreaterThan,
  GreaterThanOrEquals,
  IN,
  LessThan,
  LessThanOrEquals,
  NotIn,
  RuleOperator' #-}

instance FromText RuleOperator where
    parser = (RuleOperator' . mk) <$> takeText

instance ToText RuleOperator where
    toText (RuleOperator' ci) = original ci

-- | Represents an enum of /known/ $RuleOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RuleOperator where
    toEnum i = case i of
        0 -> Contains
        1 -> Equals
        2 -> GreaterThan
        3 -> GreaterThanOrEquals
        4 -> IN
        5 -> LessThan
        6 -> LessThanOrEquals
        7 -> NotIn
        _ -> (error . showText) $ "Unknown index for RuleOperator: " <> toText i
    fromEnum x = case x of
        Contains -> 0
        Equals -> 1
        GreaterThan -> 2
        GreaterThanOrEquals -> 3
        IN -> 4
        LessThan -> 5
        LessThanOrEquals -> 6
        NotIn -> 7
        RuleOperator' name -> (error . showText) $ "Unknown RuleOperator: " <> original name

-- | Represents the bounds of /known/ $RuleOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RuleOperator where
    minBound = Contains
    maxBound = NotIn

instance Hashable     RuleOperator
instance NFData       RuleOperator
instance ToByteString RuleOperator
instance ToQuery      RuleOperator
instance ToHeader     RuleOperator

instance ToJSON RuleOperator where
    toJSON = toJSONText

instance FromJSON RuleOperator where
    parseJSON = parseJSONText "RuleOperator"
