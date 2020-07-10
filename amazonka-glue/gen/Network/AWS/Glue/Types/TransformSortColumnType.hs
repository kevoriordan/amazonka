{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TransformSortColumnType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.TransformSortColumnType (
  TransformSortColumnType (
    ..
    , TSCCreated
    , TSCLastModified
    , TSCName
    , TSCStatus
    , TSCTransformType
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TransformSortColumnType = TransformSortColumnType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern TSCCreated :: TransformSortColumnType
pattern TSCCreated = TransformSortColumnType' "CREATED"

pattern TSCLastModified :: TransformSortColumnType
pattern TSCLastModified = TransformSortColumnType' "LAST_MODIFIED"

pattern TSCName :: TransformSortColumnType
pattern TSCName = TransformSortColumnType' "NAME"

pattern TSCStatus :: TransformSortColumnType
pattern TSCStatus = TransformSortColumnType' "STATUS"

pattern TSCTransformType :: TransformSortColumnType
pattern TSCTransformType = TransformSortColumnType' "TRANSFORM_TYPE"

{-# COMPLETE
  TSCCreated,
  TSCLastModified,
  TSCName,
  TSCStatus,
  TSCTransformType,
  TransformSortColumnType' #-}

instance FromText TransformSortColumnType where
    parser = (TransformSortColumnType' . mk) <$> takeText

instance ToText TransformSortColumnType where
    toText (TransformSortColumnType' ci) = original ci

-- | Represents an enum of /known/ $TransformSortColumnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransformSortColumnType where
    toEnum i = case i of
        0 -> TSCCreated
        1 -> TSCLastModified
        2 -> TSCName
        3 -> TSCStatus
        4 -> TSCTransformType
        _ -> (error . showText) $ "Unknown index for TransformSortColumnType: " <> toText i
    fromEnum x = case x of
        TSCCreated -> 0
        TSCLastModified -> 1
        TSCName -> 2
        TSCStatus -> 3
        TSCTransformType -> 4
        TransformSortColumnType' name -> (error . showText) $ "Unknown TransformSortColumnType: " <> original name

-- | Represents the bounds of /known/ $TransformSortColumnType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransformSortColumnType where
    minBound = TSCCreated
    maxBound = TSCTransformType

instance Hashable     TransformSortColumnType
instance NFData       TransformSortColumnType
instance ToByteString TransformSortColumnType
instance ToQuery      TransformSortColumnType
instance ToHeader     TransformSortColumnType

instance ToJSON TransformSortColumnType where
    toJSON = toJSONText
