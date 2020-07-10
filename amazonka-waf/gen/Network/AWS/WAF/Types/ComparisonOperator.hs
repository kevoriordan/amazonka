{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ComparisonOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.ComparisonOperator (
  ComparisonOperator (
    ..
    , EQ'
    , GE
    , GT'
    , LE
    , LT'
    , NE
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComparisonOperator = ComparisonOperator' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern EQ' :: ComparisonOperator
pattern EQ' = ComparisonOperator' "EQ"

pattern GE :: ComparisonOperator
pattern GE = ComparisonOperator' "GE"

pattern GT' :: ComparisonOperator
pattern GT' = ComparisonOperator' "GT"

pattern LE :: ComparisonOperator
pattern LE = ComparisonOperator' "LE"

pattern LT' :: ComparisonOperator
pattern LT' = ComparisonOperator' "LT"

pattern NE :: ComparisonOperator
pattern NE = ComparisonOperator' "NE"

{-# COMPLETE
  EQ',
  GE,
  GT',
  LE,
  LT',
  NE,
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
        0 -> EQ'
        1 -> GE
        2 -> GT'
        3 -> LE
        4 -> LT'
        5 -> NE
        _ -> (error . showText) $ "Unknown index for ComparisonOperator: " <> toText i
    fromEnum x = case x of
        EQ' -> 0
        GE -> 1
        GT' -> 2
        LE -> 3
        LT' -> 4
        NE -> 5
        ComparisonOperator' name -> (error . showText) $ "Unknown ComparisonOperator: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperator where
    minBound = EQ'
    maxBound = NE

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"
