{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.SourceType (
  SourceType (
    ..
    , CacheCluster
    , CacheParameterGroup
    , CacheSecurityGroup
    , CacheSubnetGroup
    , ReplicationGroup
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SourceType = SourceType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CacheCluster :: SourceType
pattern CacheCluster = SourceType' "cache-cluster"

pattern CacheParameterGroup :: SourceType
pattern CacheParameterGroup = SourceType' "cache-parameter-group"

pattern CacheSecurityGroup :: SourceType
pattern CacheSecurityGroup = SourceType' "cache-security-group"

pattern CacheSubnetGroup :: SourceType
pattern CacheSubnetGroup = SourceType' "cache-subnet-group"

pattern ReplicationGroup :: SourceType
pattern ReplicationGroup = SourceType' "replication-group"

{-# COMPLETE
  CacheCluster,
  CacheParameterGroup,
  CacheSecurityGroup,
  CacheSubnetGroup,
  ReplicationGroup,
  SourceType' #-}

instance FromText SourceType where
    parser = (SourceType' . mk) <$> takeText

instance ToText SourceType where
    toText (SourceType' ci) = original ci

-- | Represents an enum of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SourceType where
    toEnum i = case i of
        0 -> CacheCluster
        1 -> CacheParameterGroup
        2 -> CacheSecurityGroup
        3 -> CacheSubnetGroup
        4 -> ReplicationGroup
        _ -> (error . showText) $ "Unknown index for SourceType: " <> toText i
    fromEnum x = case x of
        CacheCluster -> 0
        CacheParameterGroup -> 1
        CacheSecurityGroup -> 2
        CacheSubnetGroup -> 3
        ReplicationGroup -> 4
        SourceType' name -> (error . showText) $ "Unknown SourceType: " <> original name

-- | Represents the bounds of /known/ $SourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SourceType where
    minBound = CacheCluster
    maxBound = ReplicationGroup

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"
