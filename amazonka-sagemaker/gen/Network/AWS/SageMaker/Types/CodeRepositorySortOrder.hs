{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySortOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CodeRepositorySortOrder (
  CodeRepositorySortOrder (
    ..
    , CRSOAscending
    , CRSODescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CodeRepositorySortOrder = CodeRepositorySortOrder' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern CRSOAscending :: CodeRepositorySortOrder
pattern CRSOAscending = CodeRepositorySortOrder' "Ascending"

pattern CRSODescending :: CodeRepositorySortOrder
pattern CRSODescending = CodeRepositorySortOrder' "Descending"

{-# COMPLETE
  CRSOAscending,
  CRSODescending,
  CodeRepositorySortOrder' #-}

instance FromText CodeRepositorySortOrder where
    parser = (CodeRepositorySortOrder' . mk) <$> takeText

instance ToText CodeRepositorySortOrder where
    toText (CodeRepositorySortOrder' ci) = original ci

-- | Represents an enum of /known/ $CodeRepositorySortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CodeRepositorySortOrder where
    toEnum i = case i of
        0 -> CRSOAscending
        1 -> CRSODescending
        _ -> (error . showText) $ "Unknown index for CodeRepositorySortOrder: " <> toText i
    fromEnum x = case x of
        CRSOAscending -> 0
        CRSODescending -> 1
        CodeRepositorySortOrder' name -> (error . showText) $ "Unknown CodeRepositorySortOrder: " <> original name

-- | Represents the bounds of /known/ $CodeRepositorySortOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CodeRepositorySortOrder where
    minBound = CRSOAscending
    maxBound = CRSODescending

instance Hashable     CodeRepositorySortOrder
instance NFData       CodeRepositorySortOrder
instance ToByteString CodeRepositorySortOrder
instance ToQuery      CodeRepositorySortOrder
instance ToHeader     CodeRepositorySortOrder

instance ToJSON CodeRepositorySortOrder where
    toJSON = toJSONText
