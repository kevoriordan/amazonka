{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchSortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SearchSortOrder (
  SearchSortOrder (
    ..
    , SSOAscending
    , SSODescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SearchSortOrder = SearchSortOrder' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern SSOAscending :: SearchSortOrder
pattern SSOAscending = SearchSortOrder' "Ascending"

pattern SSODescending :: SearchSortOrder
pattern SSODescending = SearchSortOrder' "Descending"

{-# COMPLETE
  SSOAscending,
  SSODescending,
  SearchSortOrder' #-}

instance FromText SearchSortOrder where
    parser = (SearchSortOrder' . mk) <$> takeText

instance ToText SearchSortOrder where
    toText (SearchSortOrder' ci) = original ci

-- | Represents an enum of /known/ $SearchSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SearchSortOrder where
    toEnum i = case i of
        0 -> SSOAscending
        1 -> SSODescending
        _ -> (error . showText) $ "Unknown index for SearchSortOrder: " <> toText i
    fromEnum x = case x of
        SSOAscending -> 0
        SSODescending -> 1
        SearchSortOrder' name -> (error . showText) $ "Unknown SearchSortOrder: " <> original name

-- | Represents the bounds of /known/ $SearchSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SearchSortOrder where
    minBound = SSOAscending
    maxBound = SSODescending

instance Hashable     SearchSortOrder
instance NFData       SearchSortOrder
instance ToByteString SearchSortOrder
instance ToQuery      SearchSortOrder
instance ToHeader     SearchSortOrder

instance ToJSON SearchSortOrder where
    toJSON = toJSONText
