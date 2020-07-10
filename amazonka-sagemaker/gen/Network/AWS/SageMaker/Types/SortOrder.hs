{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SortOrder (
  SortOrder (
    ..
    , SOAscending
    , SODescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortOrder = SortOrder' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern SOAscending :: SortOrder
pattern SOAscending = SortOrder' "Ascending"

pattern SODescending :: SortOrder
pattern SODescending = SortOrder' "Descending"

{-# COMPLETE
  SOAscending,
  SODescending,
  SortOrder' #-}

instance FromText SortOrder where
    parser = (SortOrder' . mk) <$> takeText

instance ToText SortOrder where
    toText (SortOrder' ci) = original ci

-- | Represents an enum of /known/ $SortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortOrder where
    toEnum i = case i of
        0 -> SOAscending
        1 -> SODescending
        _ -> (error . showText) $ "Unknown index for SortOrder: " <> toText i
    fromEnum x = case x of
        SOAscending -> 0
        SODescending -> 1
        SortOrder' name -> (error . showText) $ "Unknown SortOrder: " <> original name

-- | Represents the bounds of /known/ $SortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortOrder where
    minBound = SOAscending
    maxBound = SODescending

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText
