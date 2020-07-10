{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SortOrderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.SortOrderType (
  SortOrderType (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortOrderType = SortOrderType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Ascending :: SortOrderType
pattern Ascending = SortOrderType' "ASCENDING"

pattern Descending :: SortOrderType
pattern Descending = SortOrderType' "DESCENDING"

{-# COMPLETE
  Ascending,
  Descending,
  SortOrderType' #-}

instance FromText SortOrderType where
    parser = (SortOrderType' . mk) <$> takeText

instance ToText SortOrderType where
    toText (SortOrderType' ci) = original ci

-- | Represents an enum of /known/ $SortOrderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortOrderType where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for SortOrderType: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        SortOrderType' name -> (error . showText) $ "Unknown SortOrderType: " <> original name

-- | Represents the bounds of /known/ $SortOrderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortOrderType where
    minBound = Ascending
    maxBound = Descending

instance Hashable     SortOrderType
instance NFData       SortOrderType
instance ToByteString SortOrderType
instance ToQuery      SortOrderType
instance ToHeader     SortOrderType

instance ToJSON SortOrderType where
    toJSON = toJSONText
