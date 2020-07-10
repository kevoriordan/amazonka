{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder (
  NotebookInstanceLifecycleConfigSortOrder (
    ..
    , NILCSOAscending
    , NILCSODescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotebookInstanceLifecycleConfigSortOrder = NotebookInstanceLifecycleConfigSortOrder' (CI
                                                                                             Text)
                                                  deriving (Eq, Ord, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

pattern NILCSOAscending :: NotebookInstanceLifecycleConfigSortOrder
pattern NILCSOAscending = NotebookInstanceLifecycleConfigSortOrder' "Ascending"

pattern NILCSODescending :: NotebookInstanceLifecycleConfigSortOrder
pattern NILCSODescending = NotebookInstanceLifecycleConfigSortOrder' "Descending"

{-# COMPLETE
  NILCSOAscending,
  NILCSODescending,
  NotebookInstanceLifecycleConfigSortOrder' #-}

instance FromText NotebookInstanceLifecycleConfigSortOrder where
    parser = (NotebookInstanceLifecycleConfigSortOrder' . mk) <$> takeText

instance ToText NotebookInstanceLifecycleConfigSortOrder where
    toText (NotebookInstanceLifecycleConfigSortOrder' ci) = original ci

-- | Represents an enum of /known/ $NotebookInstanceLifecycleConfigSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotebookInstanceLifecycleConfigSortOrder where
    toEnum i = case i of
        0 -> NILCSOAscending
        1 -> NILCSODescending
        _ -> (error . showText) $ "Unknown index for NotebookInstanceLifecycleConfigSortOrder: " <> toText i
    fromEnum x = case x of
        NILCSOAscending -> 0
        NILCSODescending -> 1
        NotebookInstanceLifecycleConfigSortOrder' name -> (error . showText) $ "Unknown NotebookInstanceLifecycleConfigSortOrder: " <> original name

-- | Represents the bounds of /known/ $NotebookInstanceLifecycleConfigSortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotebookInstanceLifecycleConfigSortOrder where
    minBound = NILCSOAscending
    maxBound = NILCSODescending

instance Hashable     NotebookInstanceLifecycleConfigSortOrder
instance NFData       NotebookInstanceLifecycleConfigSortOrder
instance ToByteString NotebookInstanceLifecycleConfigSortOrder
instance ToQuery      NotebookInstanceLifecycleConfigSortOrder
instance ToHeader     NotebookInstanceLifecycleConfigSortOrder

instance ToJSON NotebookInstanceLifecycleConfigSortOrder where
    toJSON = toJSONText
