{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OrderKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.OrderKey (
  OrderKey (
    ..
    , OKAscending
    , OKDescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrderKey = OrderKey' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern OKAscending :: OrderKey
pattern OKAscending = OrderKey' "Ascending"

pattern OKDescending :: OrderKey
pattern OKDescending = OrderKey' "Descending"

{-# COMPLETE
  OKAscending,
  OKDescending,
  OrderKey' #-}

instance FromText OrderKey where
    parser = (OrderKey' . mk) <$> takeText

instance ToText OrderKey where
    toText (OrderKey' ci) = original ci

-- | Represents an enum of /known/ $OrderKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrderKey where
    toEnum i = case i of
        0 -> OKAscending
        1 -> OKDescending
        _ -> (error . showText) $ "Unknown index for OrderKey: " <> toText i
    fromEnum x = case x of
        OKAscending -> 0
        OKDescending -> 1
        OrderKey' name -> (error . showText) $ "Unknown OrderKey: " <> original name

-- | Represents the bounds of /known/ $OrderKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrderKey where
    minBound = OKAscending
    maxBound = OKDescending

instance Hashable     OrderKey
instance NFData       OrderKey
instance ToByteString OrderKey
instance ToQuery      OrderKey
instance ToHeader     OrderKey

instance ToJSON OrderKey where
    toJSON = toJSONText
