{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.SortOrder (
  SortOrder (
    ..
    , Asc
    , Desc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortOrder = SortOrder' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Asc :: SortOrder
pattern Asc = SortOrder' "ASC"

pattern Desc :: SortOrder
pattern Desc = SortOrder' "DESC"

{-# COMPLETE
  Asc,
  Desc,
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
        0 -> Asc
        1 -> Desc
        _ -> (error . showText) $ "Unknown index for SortOrder: " <> toText i
    fromEnum x = case x of
        Asc -> 0
        Desc -> 1
        SortOrder' name -> (error . showText) $ "Unknown SortOrder: " <> original name

-- | Represents the bounds of /known/ $SortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortOrder where
    minBound = Asc
    maxBound = Desc

instance Hashable     SortOrder
instance NFData       SortOrder
instance ToByteString SortOrder
instance ToQuery      SortOrder
instance ToHeader     SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText
