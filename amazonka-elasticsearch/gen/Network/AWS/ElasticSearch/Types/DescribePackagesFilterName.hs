{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DescribePackagesFilterName (
  DescribePackagesFilterName (
    ..
    , PackageId
    , PackageName
    , PackageStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DescribePackagesFilterName = DescribePackagesFilterName' (CI
                                                                 Text)
                                    deriving (Eq, Ord, Read, Show, Data,
                                              Typeable, Generic)

pattern PackageId :: DescribePackagesFilterName
pattern PackageId = DescribePackagesFilterName' "PackageID"

pattern PackageName :: DescribePackagesFilterName
pattern PackageName = DescribePackagesFilterName' "PackageName"

pattern PackageStatus :: DescribePackagesFilterName
pattern PackageStatus = DescribePackagesFilterName' "PackageStatus"

{-# COMPLETE
  PackageId,
  PackageName,
  PackageStatus,
  DescribePackagesFilterName' #-}

instance FromText DescribePackagesFilterName where
    parser = (DescribePackagesFilterName' . mk) <$> takeText

instance ToText DescribePackagesFilterName where
    toText (DescribePackagesFilterName' ci) = original ci

-- | Represents an enum of /known/ $DescribePackagesFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DescribePackagesFilterName where
    toEnum i = case i of
        0 -> PackageId
        1 -> PackageName
        2 -> PackageStatus
        _ -> (error . showText) $ "Unknown index for DescribePackagesFilterName: " <> toText i
    fromEnum x = case x of
        PackageId -> 0
        PackageName -> 1
        PackageStatus -> 2
        DescribePackagesFilterName' name -> (error . showText) $ "Unknown DescribePackagesFilterName: " <> original name

-- | Represents the bounds of /known/ $DescribePackagesFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DescribePackagesFilterName where
    minBound = PackageId
    maxBound = PackageStatus

instance Hashable     DescribePackagesFilterName
instance NFData       DescribePackagesFilterName
instance ToByteString DescribePackagesFilterName
instance ToQuery      DescribePackagesFilterName
instance ToHeader     DescribePackagesFilterName

instance ToJSON DescribePackagesFilterName where
    toJSON = toJSONText
