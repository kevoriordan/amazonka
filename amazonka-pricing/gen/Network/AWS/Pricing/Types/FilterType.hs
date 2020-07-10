{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.FilterType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pricing.Types.FilterType (
  FilterType (
    ..
    , TermMatch
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FilterType = FilterType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern TermMatch :: FilterType
pattern TermMatch = FilterType' "TERM_MATCH"

{-# COMPLETE
  TermMatch,
  FilterType' #-}

instance FromText FilterType where
    parser = (FilterType' . mk) <$> takeText

instance ToText FilterType where
    toText (FilterType' ci) = original ci

-- | Represents an enum of /known/ $FilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FilterType where
    toEnum i = case i of
        0 -> TermMatch
        _ -> (error . showText) $ "Unknown index for FilterType: " <> toText i
    fromEnum x = case x of
        TermMatch -> 0
        FilterType' name -> (error . showText) $ "Unknown FilterType: " <> original name

-- | Represents the bounds of /known/ $FilterType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FilterType where
    minBound = TermMatch
    maxBound = TermMatch

instance Hashable     FilterType
instance NFData       FilterType
instance ToByteString FilterType
instance ToQuery      FilterType
instance ToHeader     FilterType

instance ToJSON FilterType where
    toJSON = toJSONText
