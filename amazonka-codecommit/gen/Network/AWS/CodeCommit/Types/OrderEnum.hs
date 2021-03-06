{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.OrderEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.OrderEnum (
  OrderEnum (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrderEnum = OrderEnum' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Ascending :: OrderEnum
pattern Ascending = OrderEnum' "ascending"

pattern Descending :: OrderEnum
pattern Descending = OrderEnum' "descending"

{-# COMPLETE
  Ascending,
  Descending,
  OrderEnum' #-}

instance FromText OrderEnum where
    parser = (OrderEnum' . mk) <$> takeText

instance ToText OrderEnum where
    toText (OrderEnum' ci) = original ci

-- | Represents an enum of /known/ $OrderEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrderEnum where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for OrderEnum: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        OrderEnum' name -> (error . showText) $ "Unknown OrderEnum: " <> original name

-- | Represents the bounds of /known/ $OrderEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrderEnum where
    minBound = Ascending
    maxBound = Descending

instance Hashable     OrderEnum
instance NFData       OrderEnum
instance ToByteString OrderEnum
instance ToQuery      OrderEnum
instance ToHeader     OrderEnum

instance ToJSON OrderEnum where
    toJSON = toJSONText
