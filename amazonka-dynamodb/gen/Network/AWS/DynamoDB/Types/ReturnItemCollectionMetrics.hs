{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReturnItemCollectionMetrics (
  ReturnItemCollectionMetrics (
    ..
    , RICMNone
    , RICMSize
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReturnItemCollectionMetrics = ReturnItemCollectionMetrics' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern RICMNone :: ReturnItemCollectionMetrics
pattern RICMNone = ReturnItemCollectionMetrics' "NONE"

pattern RICMSize :: ReturnItemCollectionMetrics
pattern RICMSize = ReturnItemCollectionMetrics' "SIZE"

{-# COMPLETE
  RICMNone,
  RICMSize,
  ReturnItemCollectionMetrics' #-}

instance FromText ReturnItemCollectionMetrics where
    parser = (ReturnItemCollectionMetrics' . mk) <$> takeText

instance ToText ReturnItemCollectionMetrics where
    toText (ReturnItemCollectionMetrics' ci) = original ci

-- | Represents an enum of /known/ $ReturnItemCollectionMetrics.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReturnItemCollectionMetrics where
    toEnum i = case i of
        0 -> RICMNone
        1 -> RICMSize
        _ -> (error . showText) $ "Unknown index for ReturnItemCollectionMetrics: " <> toText i
    fromEnum x = case x of
        RICMNone -> 0
        RICMSize -> 1
        ReturnItemCollectionMetrics' name -> (error . showText) $ "Unknown ReturnItemCollectionMetrics: " <> original name

-- | Represents the bounds of /known/ $ReturnItemCollectionMetrics.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReturnItemCollectionMetrics where
    minBound = RICMNone
    maxBound = RICMSize

instance Hashable     ReturnItemCollectionMetrics
instance NFData       ReturnItemCollectionMetrics
instance ToByteString ReturnItemCollectionMetrics
instance ToQuery      ReturnItemCollectionMetrics
instance ToHeader     ReturnItemCollectionMetrics

instance ToJSON ReturnItemCollectionMetrics where
    toJSON = toJSONText
