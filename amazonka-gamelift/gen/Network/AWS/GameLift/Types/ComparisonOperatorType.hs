{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ComparisonOperatorType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.ComparisonOperatorType (
  ComparisonOperatorType (
    ..
    , GreaterThanOrEqualToThreshold
    , GreaterThanThreshold
    , LessThanOrEqualToThreshold
    , LessThanThreshold
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComparisonOperatorType = ComparisonOperatorType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern GreaterThanOrEqualToThreshold :: ComparisonOperatorType
pattern GreaterThanOrEqualToThreshold = ComparisonOperatorType' "GreaterThanOrEqualToThreshold"

pattern GreaterThanThreshold :: ComparisonOperatorType
pattern GreaterThanThreshold = ComparisonOperatorType' "GreaterThanThreshold"

pattern LessThanOrEqualToThreshold :: ComparisonOperatorType
pattern LessThanOrEqualToThreshold = ComparisonOperatorType' "LessThanOrEqualToThreshold"

pattern LessThanThreshold :: ComparisonOperatorType
pattern LessThanThreshold = ComparisonOperatorType' "LessThanThreshold"

{-# COMPLETE
  GreaterThanOrEqualToThreshold,
  GreaterThanThreshold,
  LessThanOrEqualToThreshold,
  LessThanThreshold,
  ComparisonOperatorType' #-}

instance FromText ComparisonOperatorType where
    parser = (ComparisonOperatorType' . mk) <$> takeText

instance ToText ComparisonOperatorType where
    toText (ComparisonOperatorType' ci) = original ci

-- | Represents an enum of /known/ $ComparisonOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComparisonOperatorType where
    toEnum i = case i of
        0 -> GreaterThanOrEqualToThreshold
        1 -> GreaterThanThreshold
        2 -> LessThanOrEqualToThreshold
        3 -> LessThanThreshold
        _ -> (error . showText) $ "Unknown index for ComparisonOperatorType: " <> toText i
    fromEnum x = case x of
        GreaterThanOrEqualToThreshold -> 0
        GreaterThanThreshold -> 1
        LessThanOrEqualToThreshold -> 2
        LessThanThreshold -> 3
        ComparisonOperatorType' name -> (error . showText) $ "Unknown ComparisonOperatorType: " <> original name

-- | Represents the bounds of /known/ $ComparisonOperatorType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComparisonOperatorType where
    minBound = GreaterThanOrEqualToThreshold
    maxBound = LessThanThreshold

instance Hashable     ComparisonOperatorType
instance NFData       ComparisonOperatorType
instance ToByteString ComparisonOperatorType
instance ToQuery      ComparisonOperatorType
instance ToHeader     ComparisonOperatorType

instance ToJSON ComparisonOperatorType where
    toJSON = toJSONText

instance FromJSON ComparisonOperatorType where
    parseJSON = parseJSONText "ComparisonOperatorType"
