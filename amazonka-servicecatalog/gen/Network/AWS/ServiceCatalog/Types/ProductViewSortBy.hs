{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewSortBy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProductViewSortBy (
  ProductViewSortBy (
    ..
    , CreationDate
    , Title
    , VersionCount
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProductViewSortBy = ProductViewSortBy' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern CreationDate :: ProductViewSortBy
pattern CreationDate = ProductViewSortBy' "CreationDate"

pattern Title :: ProductViewSortBy
pattern Title = ProductViewSortBy' "Title"

pattern VersionCount :: ProductViewSortBy
pattern VersionCount = ProductViewSortBy' "VersionCount"

{-# COMPLETE
  CreationDate,
  Title,
  VersionCount,
  ProductViewSortBy' #-}

instance FromText ProductViewSortBy where
    parser = (ProductViewSortBy' . mk) <$> takeText

instance ToText ProductViewSortBy where
    toText (ProductViewSortBy' ci) = original ci

-- | Represents an enum of /known/ $ProductViewSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProductViewSortBy where
    toEnum i = case i of
        0 -> CreationDate
        1 -> Title
        2 -> VersionCount
        _ -> (error . showText) $ "Unknown index for ProductViewSortBy: " <> toText i
    fromEnum x = case x of
        CreationDate -> 0
        Title -> 1
        VersionCount -> 2
        ProductViewSortBy' name -> (error . showText) $ "Unknown ProductViewSortBy: " <> original name

-- | Represents the bounds of /known/ $ProductViewSortBy.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProductViewSortBy where
    minBound = CreationDate
    maxBound = VersionCount

instance Hashable     ProductViewSortBy
instance NFData       ProductViewSortBy
instance ToByteString ProductViewSortBy
instance ToQuery      ProductViewSortBy
instance ToHeader     ProductViewSortBy

instance ToJSON ProductViewSortBy where
    toJSON = toJSONText
