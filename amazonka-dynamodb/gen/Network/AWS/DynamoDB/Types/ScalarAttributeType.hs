{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ScalarAttributeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ScalarAttributeType (
  ScalarAttributeType (
    ..
    , B
    , N
    , S
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalarAttributeType = ScalarAttributeType' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern B :: ScalarAttributeType
pattern B = ScalarAttributeType' "B"

pattern N :: ScalarAttributeType
pattern N = ScalarAttributeType' "N"

pattern S :: ScalarAttributeType
pattern S = ScalarAttributeType' "S"

{-# COMPLETE
  B,
  N,
  S,
  ScalarAttributeType' #-}

instance FromText ScalarAttributeType where
    parser = (ScalarAttributeType' . mk) <$> takeText

instance ToText ScalarAttributeType where
    toText (ScalarAttributeType' ci) = original ci

-- | Represents an enum of /known/ $ScalarAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalarAttributeType where
    toEnum i = case i of
        0 -> B
        1 -> N
        2 -> S
        _ -> (error . showText) $ "Unknown index for ScalarAttributeType: " <> toText i
    fromEnum x = case x of
        B -> 0
        N -> 1
        S -> 2
        ScalarAttributeType' name -> (error . showText) $ "Unknown ScalarAttributeType: " <> original name

-- | Represents the bounds of /known/ $ScalarAttributeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalarAttributeType where
    minBound = B
    maxBound = S

instance Hashable     ScalarAttributeType
instance NFData       ScalarAttributeType
instance ToByteString ScalarAttributeType
instance ToQuery      ScalarAttributeType
instance ToHeader     ScalarAttributeType

instance ToJSON ScalarAttributeType where
    toJSON = toJSONText

instance FromJSON ScalarAttributeType where
    parseJSON = parseJSONText "ScalarAttributeType"
