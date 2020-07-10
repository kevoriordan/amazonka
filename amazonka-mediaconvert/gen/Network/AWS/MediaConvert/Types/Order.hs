{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Order
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Order (
  Order (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
data Order = Order' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern Ascending :: Order
pattern Ascending = Order' "ASCENDING"

pattern Descending :: Order
pattern Descending = Order' "DESCENDING"

{-# COMPLETE
  Ascending,
  Descending,
  Order' #-}

instance FromText Order where
    parser = (Order' . mk) <$> takeText

instance ToText Order where
    toText (Order' ci) = original ci

-- | Represents an enum of /known/ $Order.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Order where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for Order: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        Order' name -> (error . showText) $ "Unknown Order: " <> original name

-- | Represents the bounds of /known/ $Order.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Order where
    minBound = Ascending
    maxBound = Descending

instance Hashable     Order
instance NFData       Order
instance ToByteString Order
instance ToQuery      Order
instance ToHeader     Order

instance ToJSON Order where
    toJSON = toJSONText
