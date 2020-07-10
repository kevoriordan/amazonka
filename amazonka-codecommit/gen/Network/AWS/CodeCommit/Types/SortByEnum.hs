{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SortByEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.SortByEnum (
  SortByEnum (
    ..
    , LastModifiedDate
    , RepositoryName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SortByEnum = SortByEnum' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern LastModifiedDate :: SortByEnum
pattern LastModifiedDate = SortByEnum' "lastModifiedDate"

pattern RepositoryName :: SortByEnum
pattern RepositoryName = SortByEnum' "repositoryName"

{-# COMPLETE
  LastModifiedDate,
  RepositoryName,
  SortByEnum' #-}

instance FromText SortByEnum where
    parser = (SortByEnum' . mk) <$> takeText

instance ToText SortByEnum where
    toText (SortByEnum' ci) = original ci

-- | Represents an enum of /known/ $SortByEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortByEnum where
    toEnum i = case i of
        0 -> LastModifiedDate
        1 -> RepositoryName
        _ -> (error . showText) $ "Unknown index for SortByEnum: " <> toText i
    fromEnum x = case x of
        LastModifiedDate -> 0
        RepositoryName -> 1
        SortByEnum' name -> (error . showText) $ "Unknown SortByEnum: " <> original name

-- | Represents the bounds of /known/ $SortByEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortByEnum where
    minBound = LastModifiedDate
    maxBound = RepositoryName

instance Hashable     SortByEnum
instance NFData       SortByEnum
instance ToByteString SortByEnum
instance ToQuery      SortByEnum
instance ToHeader     SortByEnum

instance ToJSON SortByEnum where
    toJSON = toJSONText
