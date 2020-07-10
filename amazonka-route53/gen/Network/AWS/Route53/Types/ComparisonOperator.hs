{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ComparisonOperator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.ComparisonOperator (
  ComparisonOperator (
    ..
    , GreaterThanOrEqualToThreshold
    , GreaterThanThreshold
    , LessThanOrEqualToThreshold
    , LessThanThreshold
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ComparisonOperator = ComparisonOperator' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern GreaterThanOrEqualToThreshold :: ComparisonOperator
pattern GreaterThanOrEqualToThreshold = ComparisonOperator' "GreaterThanOrEqualToThreshold"

pattern GreaterThanThreshold :: ComparisonOperator
pattern GreaterThanThreshold = ComparisonOperator' "GreaterThanThreshold"

pattern LessThanOrEqualToThreshold :: ComparisonOperator
pattern LessThanOrEqualToThreshold = ComparisonOperator' "LessThanOrEqualToThreshold"

pattern LessThanThreshold :: ComparisonOperator
pattern LessThanThreshold = ComparisonOperator' "LessThanThreshold"

{-# COMPLETE
  GreaterThanOrEqualToThreshold,
  GreaterThanThreshold,
  LessThanOrEqualToThreshold,
  LessThanThreshold,
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
        0 -> GreaterThanOrEqualToThreshold
        1 -> GreaterThanThreshold
        2 -> LessThanOrEqualToThreshold
        3 -> LessThanThreshold
        _ -> (error . showText) $ "Unknown index for ComparisonOperator: " <> toText i
    fromEnum x = case x of
        GreaterThanOrEqualToThreshold -> 0
        GreaterThanThreshold -> 1
        LessThanOrEqualToThreshold -> 2
        LessThanThreshold -> 3
        ComparisonOperator' name -> (error . showText) $ "Unknown ComparisonOperator: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperator where
    minBound = GreaterThanOrEqualToThreshold
    maxBound = LessThanThreshold

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance FromXML ComparisonOperator where
    parseXML = parseXMLText "ComparisonOperator"
