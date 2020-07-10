{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.PartitionInstanceType (
  PartitionInstanceType (
    ..
    , Search_M1_Large
    , Search_M1_Small
    , Search_M2_2XLarge
    , Search_M2_XLarge
    , Search_M3_2XLarge
    , Search_M3_Large
    , Search_M3_Medium
    , Search_M3_XLarge
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The instance type (such as @search.m1.small@ ) on which an index partition is hosted.
--
--
data PartitionInstanceType = PartitionInstanceType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Search_M1_Large :: PartitionInstanceType
pattern Search_M1_Large = PartitionInstanceType' "search.m1.large"

pattern Search_M1_Small :: PartitionInstanceType
pattern Search_M1_Small = PartitionInstanceType' "search.m1.small"

pattern Search_M2_2XLarge :: PartitionInstanceType
pattern Search_M2_2XLarge = PartitionInstanceType' "search.m2.2xlarge"

pattern Search_M2_XLarge :: PartitionInstanceType
pattern Search_M2_XLarge = PartitionInstanceType' "search.m2.xlarge"

pattern Search_M3_2XLarge :: PartitionInstanceType
pattern Search_M3_2XLarge = PartitionInstanceType' "search.m3.2xlarge"

pattern Search_M3_Large :: PartitionInstanceType
pattern Search_M3_Large = PartitionInstanceType' "search.m3.large"

pattern Search_M3_Medium :: PartitionInstanceType
pattern Search_M3_Medium = PartitionInstanceType' "search.m3.medium"

pattern Search_M3_XLarge :: PartitionInstanceType
pattern Search_M3_XLarge = PartitionInstanceType' "search.m3.xlarge"

{-# COMPLETE
  Search_M1_Large,
  Search_M1_Small,
  Search_M2_2XLarge,
  Search_M2_XLarge,
  Search_M3_2XLarge,
  Search_M3_Large,
  Search_M3_Medium,
  Search_M3_XLarge,
  PartitionInstanceType' #-}

instance FromText PartitionInstanceType where
    parser = (PartitionInstanceType' . mk) <$> takeText

instance ToText PartitionInstanceType where
    toText (PartitionInstanceType' ci) = original ci

-- | Represents an enum of /known/ $PartitionInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PartitionInstanceType where
    toEnum i = case i of
        0 -> Search_M1_Large
        1 -> Search_M1_Small
        2 -> Search_M2_2XLarge
        3 -> Search_M2_XLarge
        4 -> Search_M3_2XLarge
        5 -> Search_M3_Large
        6 -> Search_M3_Medium
        7 -> Search_M3_XLarge
        _ -> (error . showText) $ "Unknown index for PartitionInstanceType: " <> toText i
    fromEnum x = case x of
        Search_M1_Large -> 0
        Search_M1_Small -> 1
        Search_M2_2XLarge -> 2
        Search_M2_XLarge -> 3
        Search_M3_2XLarge -> 4
        Search_M3_Large -> 5
        Search_M3_Medium -> 6
        Search_M3_XLarge -> 7
        PartitionInstanceType' name -> (error . showText) $ "Unknown PartitionInstanceType: " <> original name

-- | Represents the bounds of /known/ $PartitionInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PartitionInstanceType where
    minBound = Search_M1_Large
    maxBound = Search_M3_XLarge

instance Hashable     PartitionInstanceType
instance NFData       PartitionInstanceType
instance ToByteString PartitionInstanceType
instance ToQuery      PartitionInstanceType
instance ToHeader     PartitionInstanceType

instance FromXML PartitionInstanceType where
    parseXML = parseXMLText "PartitionInstanceType"
