{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType (
  ESWarmPartitionInstanceType (
    ..
    , ESWPITULTRAWARM1_Large_Elasticsearch
    , ESWPITULTRAWARM1_Medium_Elasticsearch
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ESWarmPartitionInstanceType = ESWarmPartitionInstanceType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern ESWPITULTRAWARM1_Large_Elasticsearch :: ESWarmPartitionInstanceType
pattern ESWPITULTRAWARM1_Large_Elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESWPITULTRAWARM1_Medium_Elasticsearch :: ESWarmPartitionInstanceType
pattern ESWPITULTRAWARM1_Medium_Elasticsearch = ESWarmPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  ESWPITULTRAWARM1_Large_Elasticsearch,
  ESWPITULTRAWARM1_Medium_Elasticsearch,
  ESWarmPartitionInstanceType' #-}

instance FromText ESWarmPartitionInstanceType where
    parser = (ESWarmPartitionInstanceType' . mk) <$> takeText

instance ToText ESWarmPartitionInstanceType where
    toText (ESWarmPartitionInstanceType' ci) = original ci

-- | Represents an enum of /known/ $ESWarmPartitionInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ESWarmPartitionInstanceType where
    toEnum i = case i of
        0 -> ESWPITULTRAWARM1_Large_Elasticsearch
        1 -> ESWPITULTRAWARM1_Medium_Elasticsearch
        _ -> (error . showText) $ "Unknown index for ESWarmPartitionInstanceType: " <> toText i
    fromEnum x = case x of
        ESWPITULTRAWARM1_Large_Elasticsearch -> 0
        ESWPITULTRAWARM1_Medium_Elasticsearch -> 1
        ESWarmPartitionInstanceType' name -> (error . showText) $ "Unknown ESWarmPartitionInstanceType: " <> original name

-- | Represents the bounds of /known/ $ESWarmPartitionInstanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ESWarmPartitionInstanceType where
    minBound = ESWPITULTRAWARM1_Large_Elasticsearch
    maxBound = ESWPITULTRAWARM1_Medium_Elasticsearch

instance Hashable     ESWarmPartitionInstanceType
instance NFData       ESWarmPartitionInstanceType
instance ToByteString ESWarmPartitionInstanceType
instance ToQuery      ESWarmPartitionInstanceType
instance ToHeader     ESWarmPartitionInstanceType

instance ToJSON ESWarmPartitionInstanceType where
    toJSON = toJSONText

instance FromJSON ESWarmPartitionInstanceType where
    parseJSON = parseJSONText "ESWarmPartitionInstanceType"
