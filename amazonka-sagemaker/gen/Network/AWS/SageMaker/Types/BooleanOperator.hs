{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.BooleanOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.BooleanOperator (
  BooleanOperator (
    ..
    , And
    , OR
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BooleanOperator = BooleanOperator' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern And :: BooleanOperator
pattern And = BooleanOperator' "And"

pattern OR :: BooleanOperator
pattern OR = BooleanOperator' "Or"

{-# COMPLETE
  And,
  OR,
  BooleanOperator' #-}

instance FromText BooleanOperator where
    parser = (BooleanOperator' . mk) <$> takeText

instance ToText BooleanOperator where
    toText (BooleanOperator' ci) = original ci

-- | Represents an enum of /known/ $BooleanOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BooleanOperator where
    toEnum i = case i of
        0 -> And
        1 -> OR
        _ -> (error . showText) $ "Unknown index for BooleanOperator: " <> toText i
    fromEnum x = case x of
        And -> 0
        OR -> 1
        BooleanOperator' name -> (error . showText) $ "Unknown BooleanOperator: " <> original name

-- | Represents the bounds of /known/ $BooleanOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BooleanOperator where
    minBound = And
    maxBound = OR

instance Hashable     BooleanOperator
instance NFData       BooleanOperator
instance ToByteString BooleanOperator
instance ToQuery      BooleanOperator
instance ToHeader     BooleanOperator

instance ToJSON BooleanOperator where
    toJSON = toJSONText
