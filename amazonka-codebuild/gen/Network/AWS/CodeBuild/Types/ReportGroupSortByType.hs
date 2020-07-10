{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportGroupSortByType (
  ReportGroupSortByType (
    ..
    , RGSBTCreatedTime
    , RGSBTLastModifiedTime
    , RGSBTName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportGroupSortByType = ReportGroupSortByType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern RGSBTCreatedTime :: ReportGroupSortByType
pattern RGSBTCreatedTime = ReportGroupSortByType' "CREATED_TIME"

pattern RGSBTLastModifiedTime :: ReportGroupSortByType
pattern RGSBTLastModifiedTime = ReportGroupSortByType' "LAST_MODIFIED_TIME"

pattern RGSBTName :: ReportGroupSortByType
pattern RGSBTName = ReportGroupSortByType' "NAME"

{-# COMPLETE
  RGSBTCreatedTime,
  RGSBTLastModifiedTime,
  RGSBTName,
  ReportGroupSortByType' #-}

instance FromText ReportGroupSortByType where
    parser = (ReportGroupSortByType' . mk) <$> takeText

instance ToText ReportGroupSortByType where
    toText (ReportGroupSortByType' ci) = original ci

-- | Represents an enum of /known/ $ReportGroupSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportGroupSortByType where
    toEnum i = case i of
        0 -> RGSBTCreatedTime
        1 -> RGSBTLastModifiedTime
        2 -> RGSBTName
        _ -> (error . showText) $ "Unknown index for ReportGroupSortByType: " <> toText i
    fromEnum x = case x of
        RGSBTCreatedTime -> 0
        RGSBTLastModifiedTime -> 1
        RGSBTName -> 2
        ReportGroupSortByType' name -> (error . showText) $ "Unknown ReportGroupSortByType: " <> original name

-- | Represents the bounds of /known/ $ReportGroupSortByType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportGroupSortByType where
    minBound = RGSBTCreatedTime
    maxBound = RGSBTName

instance Hashable     ReportGroupSortByType
instance NFData       ReportGroupSortByType
instance ToByteString ReportGroupSortByType
instance ToQuery      ReportGroupSortByType
instance ToHeader     ReportGroupSortByType

instance ToJSON ReportGroupSortByType where
    toJSON = toJSONText
