{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.FilterCondition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.FilterCondition (
  FilterCondition (
    ..
    , Between
    , EQ'
    , IN
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FilterCondition = FilterCondition' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Between :: FilterCondition
pattern Between = FilterCondition' "BETWEEN"

pattern EQ' :: FilterCondition
pattern EQ' = FilterCondition' "EQ"

pattern IN :: FilterCondition
pattern IN = FilterCondition' "IN"

{-# COMPLETE
  Between,
  EQ',
  IN,
  FilterCondition' #-}

instance FromText FilterCondition where
    parser = (FilterCondition' . mk) <$> takeText

instance ToText FilterCondition where
    toText (FilterCondition' ci) = original ci

-- | Represents an enum of /known/ $FilterCondition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FilterCondition where
    toEnum i = case i of
        0 -> Between
        1 -> EQ'
        2 -> IN
        _ -> (error . showText) $ "Unknown index for FilterCondition: " <> toText i
    fromEnum x = case x of
        Between -> 0
        EQ' -> 1
        IN -> 2
        FilterCondition' name -> (error . showText) $ "Unknown FilterCondition: " <> original name

-- | Represents the bounds of /known/ $FilterCondition.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FilterCondition where
    minBound = Between
    maxBound = IN

instance Hashable     FilterCondition
instance NFData       FilterCondition
instance ToByteString FilterCondition
instance ToQuery      FilterCondition
instance ToHeader     FilterCondition

instance ToJSON FilterCondition where
    toJSON = toJSONText
