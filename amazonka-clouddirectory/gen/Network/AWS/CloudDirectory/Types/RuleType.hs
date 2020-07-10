{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RuleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.RuleType (
  RuleType (
    ..
    , BinaryLength
    , NumberComparison
    , StringFromSet
    , StringLength
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RuleType = RuleType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern BinaryLength :: RuleType
pattern BinaryLength = RuleType' "BINARY_LENGTH"

pattern NumberComparison :: RuleType
pattern NumberComparison = RuleType' "NUMBER_COMPARISON"

pattern StringFromSet :: RuleType
pattern StringFromSet = RuleType' "STRING_FROM_SET"

pattern StringLength :: RuleType
pattern StringLength = RuleType' "STRING_LENGTH"

{-# COMPLETE
  BinaryLength,
  NumberComparison,
  StringFromSet,
  StringLength,
  RuleType' #-}

instance FromText RuleType where
    parser = (RuleType' . mk) <$> takeText

instance ToText RuleType where
    toText (RuleType' ci) = original ci

-- | Represents an enum of /known/ $RuleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RuleType where
    toEnum i = case i of
        0 -> BinaryLength
        1 -> NumberComparison
        2 -> StringFromSet
        3 -> StringLength
        _ -> (error . showText) $ "Unknown index for RuleType: " <> toText i
    fromEnum x = case x of
        BinaryLength -> 0
        NumberComparison -> 1
        StringFromSet -> 2
        StringLength -> 3
        RuleType' name -> (error . showText) $ "Unknown RuleType: " <> original name

-- | Represents the bounds of /known/ $RuleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RuleType where
    minBound = BinaryLength
    maxBound = StringLength

instance Hashable     RuleType
instance NFData       RuleType
instance ToByteString RuleType
instance ToQuery      RuleType
instance ToHeader     RuleType

instance ToJSON RuleType where
    toJSON = toJSONText

instance FromJSON RuleType where
    parseJSON = parseJSONText "RuleType"
