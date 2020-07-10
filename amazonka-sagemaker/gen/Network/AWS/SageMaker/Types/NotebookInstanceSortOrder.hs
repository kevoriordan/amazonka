{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceSortOrder (
  NotebookInstanceSortOrder (
    ..
    , Ascending
    , Descending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceSortOrder = NotebookInstanceSortOrder' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Ascending :: NotebookInstanceSortOrder
pattern Ascending = NotebookInstanceSortOrder' "Ascending"

pattern Descending :: NotebookInstanceSortOrder
pattern Descending = NotebookInstanceSortOrder' "Descending"

{-# COMPLETE
  Ascending,
  Descending,
  NotebookInstanceSortOrder' #-}

instance FromText NotebookInstanceSortOrder where
    parser = (NotebookInstanceSortOrder' . mk) <$> takeText

instance ToText NotebookInstanceSortOrder where
    toText (NotebookInstanceSortOrder' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceSortOrder where
    toEnum i = case i of
        0 -> Ascending
        1 -> Descending
        _ -> (error . showText) $ "Unknown index for NotebookInstanceSortOrder: " <> toText i
    fromEnum x = case x of
        Ascending -> 0
        Descending -> 1
        NotebookInstanceSortOrder' name -> (error . showText) $ "Unknown NotebookInstanceSortOrder: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceSortOrder where
    minBound = Ascending
    maxBound = Descending

instance Hashable     NotebookInstanceSortOrder
instance NFData       NotebookInstanceSortOrder
instance ToByteString NotebookInstanceSortOrder
instance ToQuery      NotebookInstanceSortOrder
instance ToHeader     NotebookInstanceSortOrder

instance ToJSON NotebookInstanceSortOrder where
    toJSON = toJSONText
