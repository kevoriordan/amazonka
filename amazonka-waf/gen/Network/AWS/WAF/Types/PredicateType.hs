{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.PredicateType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.PredicateType (
  PredicateType (
    ..
    , ByteMatch
    , GeoMatch
    , IPMatch
    , RegexMatch
    , SizeConstraint
    , SqlInjectionMatch
    , XSSMatch
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PredicateType = PredicateType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern ByteMatch :: PredicateType
pattern ByteMatch = PredicateType' "ByteMatch"

pattern GeoMatch :: PredicateType
pattern GeoMatch = PredicateType' "GeoMatch"

pattern IPMatch :: PredicateType
pattern IPMatch = PredicateType' "IPMatch"

pattern RegexMatch :: PredicateType
pattern RegexMatch = PredicateType' "RegexMatch"

pattern SizeConstraint :: PredicateType
pattern SizeConstraint = PredicateType' "SizeConstraint"

pattern SqlInjectionMatch :: PredicateType
pattern SqlInjectionMatch = PredicateType' "SqlInjectionMatch"

pattern XSSMatch :: PredicateType
pattern XSSMatch = PredicateType' "XssMatch"

{-# COMPLETE
  ByteMatch,
  GeoMatch,
  IPMatch,
  RegexMatch,
  SizeConstraint,
  SqlInjectionMatch,
  XSSMatch,
  PredicateType' #-}

instance FromText PredicateType where
    parser = (PredicateType' . mk) <$> takeText

instance ToText PredicateType where
    toText (PredicateType' ci) = original ci

-- | Represents an enum of /known/ $PredicateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PredicateType where
    toEnum i = case i of
        0 -> ByteMatch
        1 -> GeoMatch
        2 -> IPMatch
        3 -> RegexMatch
        4 -> SizeConstraint
        5 -> SqlInjectionMatch
        6 -> XSSMatch
        _ -> (error . showText) $ "Unknown index for PredicateType: " <> toText i
    fromEnum x = case x of
        ByteMatch -> 0
        GeoMatch -> 1
        IPMatch -> 2
        RegexMatch -> 3
        SizeConstraint -> 4
        SqlInjectionMatch -> 5
        XSSMatch -> 6
        PredicateType' name -> (error . showText) $ "Unknown PredicateType: " <> original name

-- | Represents the bounds of /known/ $PredicateType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PredicateType where
    minBound = ByteMatch
    maxBound = XSSMatch

instance Hashable     PredicateType
instance NFData       PredicateType
instance ToByteString PredicateType
instance ToQuery      PredicateType
instance ToHeader     PredicateType

instance ToJSON PredicateType where
    toJSON = toJSONText

instance FromJSON PredicateType where
    parseJSON = parseJSONText "PredicateType"
