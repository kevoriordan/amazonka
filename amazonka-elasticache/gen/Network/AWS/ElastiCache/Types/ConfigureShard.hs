{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ConfigureShard
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ConfigureShard where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Node group (shard) configuration options when adding or removing replicas. Each node group (shard) configuration has the following members: NodeGroupId, NewReplicaCount, and PreferredAvailabilityZones. 
--
--
--
-- /See:/ 'configureShard' smart constructor.
data ConfigureShard = ConfigureShard'{_csPreferredAvailabilityZones
                                      :: !(Maybe [Text]),
                                      _csNodeGroupId :: !Text,
                                      _csNewReplicaCount :: !Int}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigureShard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csPreferredAvailabilityZones' - A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
--
-- * 'csNodeGroupId' - The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
--
-- * 'csNewReplicaCount' - The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with. The minimum number of replicas in a shard or replication group is:     * Redis (cluster mode disabled)     * If Multi-AZ with Automatic Failover is enabled: 1     * If Multi-AZ with Automatic Failover is not enable: 0     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
configureShard
    :: Text -- ^ 'csNodeGroupId'
    -> Int -- ^ 'csNewReplicaCount'
    -> ConfigureShard
configureShard pNodeGroupId_ pNewReplicaCount_
  = ConfigureShard'{_csPreferredAvailabilityZones =
                      Nothing,
                    _csNodeGroupId = pNodeGroupId_,
                    _csNewReplicaCount = pNewReplicaCount_}

-- | A list of @PreferredAvailabilityZone@ strings that specify which availability zones the replication group's nodes are to be in. The nummber of @PreferredAvailabilityZone@ values must equal the value of @NewReplicaCount@ plus 1 to account for the primary node. If this member of @ReplicaConfiguration@ is omitted, ElastiCache for Redis selects the availability zone for each of the replicas.
csPreferredAvailabilityZones :: Lens' ConfigureShard [Text]
csPreferredAvailabilityZones = lens _csPreferredAvailabilityZones (\ s a -> s{_csPreferredAvailabilityZones = a}) . _Default . _Coerce

-- | The 4-digit id for the node group you are configuring. For Redis (cluster mode disabled) replication groups, the node group id is always 0001. To find a Redis (cluster mode enabled)'s node group's (shard's) id, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/shard-find-id.html Finding a Shard's Id> .
csNodeGroupId :: Lens' ConfigureShard Text
csNodeGroupId = lens _csNodeGroupId (\ s a -> s{_csNodeGroupId = a})

-- | The number of replicas you want in this node group at the end of this operation. The maximum value for @NewReplicaCount@ is 5. The minimum value depends upon the type of Redis replication group you are working with. The minimum number of replicas in a shard or replication group is:     * Redis (cluster mode disabled)     * If Multi-AZ with Automatic Failover is enabled: 1     * If Multi-AZ with Automatic Failover is not enable: 0     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
csNewReplicaCount :: Lens' ConfigureShard Int
csNewReplicaCount = lens _csNewReplicaCount (\ s a -> s{_csNewReplicaCount = a})

instance Hashable ConfigureShard where

instance NFData ConfigureShard where

instance ToQuery ConfigureShard where
        toQuery ConfigureShard'{..}
          = mconcat
              ["PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _csPreferredAvailabilityZones),
               "NodeGroupId" =: _csNodeGroupId,
               "NewReplicaCount" =: _csNewReplicaCount]
