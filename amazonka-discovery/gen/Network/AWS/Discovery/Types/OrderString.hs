{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.OrderString
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.OrderString (
  OrderString (
    ..
    , Asc
    , Desc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OrderString = OrderString' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Asc :: OrderString
pattern Asc = OrderString' "ASC"

pattern Desc :: OrderString
pattern Desc = OrderString' "DESC"

{-# COMPLETE
  Asc,
  Desc,
  OrderString' #-}

instance FromText OrderString where
    parser = (OrderString' . mk) <$> takeText

instance ToText OrderString where
    toText (OrderString' ci) = original ci

-- | Represents an enum of /known/ $OrderString.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OrderString where
    toEnum i = case i of
        0 -> Asc
        1 -> Desc
        _ -> (error . showText) $ "Unknown index for OrderString: " <> toText i
    fromEnum x = case x of
        Asc -> 0
        Desc -> 1
        OrderString' name -> (error . showText) $ "Unknown OrderString: " <> original name

-- | Represents the bounds of /known/ $OrderString.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OrderString where
    minBound = Asc
    maxBound = Desc

instance Hashable     OrderString
instance NFData       OrderString
instance ToByteString OrderString
instance ToQuery      OrderString
instance ToHeader     OrderString

instance ToJSON OrderString where
    toJSON = toJSONText
