{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSortByType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ProjectSortByType (
  ProjectSortByType (
    ..
    , CreatedTime
    , LastModifiedTime
    , Name
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProjectSortByType = ProjectSortByType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern CreatedTime :: ProjectSortByType
pattern CreatedTime = ProjectSortByType' "CREATED_TIME"

pattern LastModifiedTime :: ProjectSortByType
pattern LastModifiedTime = ProjectSortByType' "LAST_MODIFIED_TIME"

pattern Name :: ProjectSortByType
pattern Name = ProjectSortByType' "NAME"

{-# COMPLETE
  CreatedTime,
  LastModifiedTime,
  Name,
  ProjectSortByType' #-}

instance FromText ProjectSortByType where
    parser = (ProjectSortByType' . mk) <$> takeText

instance ToText ProjectSortByType where
    toText (ProjectSortByType' ci) = original ci

-- | Represents an enum of /known/ $ProjectSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProjectSortByType where
    toEnum i = case i of
        0 -> CreatedTime
        1 -> LastModifiedTime
        2 -> Name
        _ -> (error . showText) $ "Unknown index for ProjectSortByType: " <> toText i
    fromEnum x = case x of
        CreatedTime -> 0
        LastModifiedTime -> 1
        Name -> 2
        ProjectSortByType' name -> (error . showText) $ "Unknown ProjectSortByType: " <> original name

-- | Represents the bounds of /known/ $ProjectSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProjectSortByType where
    minBound = CreatedTime
    maxBound = Name

instance Hashable     ProjectSortByType
instance NFData       ProjectSortByType
instance ToByteString ProjectSortByType
instance ToQuery      ProjectSortByType
instance ToHeader     ProjectSortByType

instance ToJSON ProjectSortByType where
    toJSON = toJSONText
