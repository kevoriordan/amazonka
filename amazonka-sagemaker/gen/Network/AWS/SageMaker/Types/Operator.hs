{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Operator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.Operator (
  Operator (
    ..
    , Contains
    , Equals
    , Exists
    , GreaterThan
    , GreaterThanOrEqualTo
    , IN
    , LessThan
    , LessThanOrEqualTo
    , NotEquals
    , NotExists
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Operator = Operator' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Contains :: Operator
pattern Contains = Operator' "Contains"

pattern Equals :: Operator
pattern Equals = Operator' "Equals"

pattern Exists :: Operator
pattern Exists = Operator' "Exists"

pattern GreaterThan :: Operator
pattern GreaterThan = Operator' "GreaterThan"

pattern GreaterThanOrEqualTo :: Operator
pattern GreaterThanOrEqualTo = Operator' "GreaterThanOrEqualTo"

pattern IN :: Operator
pattern IN = Operator' "In"

pattern LessThan :: Operator
pattern LessThan = Operator' "LessThan"

pattern LessThanOrEqualTo :: Operator
pattern LessThanOrEqualTo = Operator' "LessThanOrEqualTo"

pattern NotEquals :: Operator
pattern NotEquals = Operator' "NotEquals"

pattern NotExists :: Operator
pattern NotExists = Operator' "NotExists"

{-# COMPLETE
  Contains,
  Equals,
  Exists,
  GreaterThan,
  GreaterThanOrEqualTo,
  IN,
  LessThan,
  LessThanOrEqualTo,
  NotEquals,
  NotExists,
  Operator' #-}

instance FromText Operator where
    parser = (Operator' . mk) <$> takeText

instance ToText Operator where
    toText (Operator' ci) = original ci

-- | Represents an enum of /known/ $Operator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Operator where
    toEnum i = case i of
        0 -> Contains
        1 -> Equals
        2 -> Exists
        3 -> GreaterThan
        4 -> GreaterThanOrEqualTo
        5 -> IN
        6 -> LessThan
        7 -> LessThanOrEqualTo
        8 -> NotEquals
        9 -> NotExists
        _ -> (error . showText) $ "Unknown index for Operator: " <> toText i
    fromEnum x = case x of
        Contains -> 0
        Equals -> 1
        Exists -> 2
        GreaterThan -> 3
        GreaterThanOrEqualTo -> 4
        IN -> 5
        LessThan -> 6
        LessThanOrEqualTo -> 7
        NotEquals -> 8
        NotExists -> 9
        Operator' name -> (error . showText) $ "Unknown Operator: " <> original name

-- | Represents the bounds of /known/ $Operator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Operator where
    minBound = Contains
    maxBound = NotExists

instance Hashable     Operator
instance NFData       Operator
instance ToByteString Operator
instance ToQuery      Operator
instance ToHeader     Operator

instance ToJSON Operator where
    toJSON = toJSONText
