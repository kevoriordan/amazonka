{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Comparator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.Comparator (
  Comparator (
    ..
    , DoesNotExist
    , EqualTo
    , Exists
    , GreaterThan
    , GreaterThanOrEqualTo
    , IN
    , LessThan
    , LessThanOrEqualTo
    , NotEqualTo
    , NotIn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Comparator = Comparator' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern DoesNotExist :: Comparator
pattern DoesNotExist = Comparator' "DoesNotExist"

pattern EqualTo :: Comparator
pattern EqualTo = Comparator' "EqualTo"

pattern Exists :: Comparator
pattern Exists = Comparator' "Exists"

pattern GreaterThan :: Comparator
pattern GreaterThan = Comparator' "GreaterThan"

pattern GreaterThanOrEqualTo :: Comparator
pattern GreaterThanOrEqualTo = Comparator' "GreaterThanOrEqualTo"

pattern IN :: Comparator
pattern IN = Comparator' "In"

pattern LessThan :: Comparator
pattern LessThan = Comparator' "LessThan"

pattern LessThanOrEqualTo :: Comparator
pattern LessThanOrEqualTo = Comparator' "LessThanOrEqualTo"

pattern NotEqualTo :: Comparator
pattern NotEqualTo = Comparator' "NotEqualTo"

pattern NotIn :: Comparator
pattern NotIn = Comparator' "NotIn"

{-# COMPLETE
  DoesNotExist,
  EqualTo,
  Exists,
  GreaterThan,
  GreaterThanOrEqualTo,
  IN,
  LessThan,
  LessThanOrEqualTo,
  NotEqualTo,
  NotIn,
  Comparator' #-}

instance FromText Comparator where
    parser = (Comparator' . mk) <$> takeText

instance ToText Comparator where
    toText (Comparator' ci) = original ci

-- | Represents an enum of /known/ $Comparator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Comparator where
    toEnum i = case i of
        0 -> DoesNotExist
        1 -> EqualTo
        2 -> Exists
        3 -> GreaterThan
        4 -> GreaterThanOrEqualTo
        5 -> IN
        6 -> LessThan
        7 -> LessThanOrEqualTo
        8 -> NotEqualTo
        9 -> NotIn
        _ -> (error . showText) $ "Unknown index for Comparator: " <> toText i
    fromEnum x = case x of
        DoesNotExist -> 0
        EqualTo -> 1
        Exists -> 2
        GreaterThan -> 3
        GreaterThanOrEqualTo -> 4
        IN -> 5
        LessThan -> 6
        LessThanOrEqualTo -> 7
        NotEqualTo -> 8
        NotIn -> 9
        Comparator' name -> (error . showText) $ "Unknown Comparator: " <> original name

-- | Represents the bounds of /known/ $Comparator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Comparator where
    minBound = DoesNotExist
    maxBound = NotIn

instance Hashable     Comparator
instance NFData       Comparator
instance ToByteString Comparator
instance ToQuery      Comparator
instance ToHeader     Comparator

instance ToJSON Comparator where
    toJSON = toJSONText

instance FromJSON Comparator where
    parseJSON = parseJSONText "Comparator"
