{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SortDirectionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.SortDirectionType (
  SortDirectionType (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortDirectionType = SortDirectionType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Ascending :: SortDirectionType
pattern Ascending = SortDirectionType' "ASCENDING"

pattern Descending :: SortDirectionType
pattern Descending = SortDirectionType' "DESCENDING"

{-# COMPLETE
  Ascending,
  Descending,
  SortDirectionType' #-}

instance FromText SortDirectionType where
    parser = (SortDirectionType' . mk) <$> takeText

instance ToText SortDirectionType where
    toText (SortDirectionType' ci) = original ci

-- | Represents an enum of /known/ $SortDirectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortDirectionType where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for SortDirectionType: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        SortDirectionType' name -> (error . showText) $ "Unknown SortDirectionType: " <> original name

-- | Represents the bounds of /known/ $SortDirectionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortDirectionType where
    minBound = Ascending
    maxBound = Descending

instance Hashable     SortDirectionType
instance NFData       SortDirectionType
instance ToByteString SortDirectionType
instance ToQuery      SortDirectionType
instance ToHeader     SortDirectionType

instance ToJSON SortDirectionType where
    toJSON = toJSONText
