{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.OrderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.OrderType (
  OrderType (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrderType = OrderType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Ascending :: OrderType
pattern Ascending = OrderType' "ASCENDING"

pattern Descending :: OrderType
pattern Descending = OrderType' "DESCENDING"

{-# COMPLETE
  Ascending,
  Descending,
  OrderType' #-}

instance FromText OrderType where
    parser = (OrderType' . mk) <$> takeText

instance ToText OrderType where
    toText (OrderType' ci) = original ci

-- | Represents an enum of /known/ $OrderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrderType where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for OrderType: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        OrderType' name -> (error . showText) $ "Unknown OrderType: " <> original name

-- | Represents the bounds of /known/ $OrderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrderType where
    minBound = Ascending
    maxBound = Descending

instance Hashable     OrderType
instance NFData       OrderType
instance ToByteString OrderType
instance ToQuery      OrderType
instance ToHeader     OrderType

instance ToJSON OrderType where
    toJSON = toJSONText
