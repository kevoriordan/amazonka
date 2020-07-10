{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.NodeGroupConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Node group (shard) configuration options. Each node group (shard) configuration has the following: @Slots@ , @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ .
--
--
--
-- /See:/ 'nodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'{_ngcSlots
                                                      :: !(Maybe Text),
                                                      _ngcReplicaCount ::
                                                      !(Maybe Int),
                                                      _ngcPrimaryAvailabilityZone
                                                      :: !(Maybe Text),
                                                      _ngcReplicaAvailabilityZones
                                                      :: !(Maybe [Text]),
                                                      _ngcNodeGroupId ::
                                                      !(Maybe Text)}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'NodeGroupConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngcSlots' - A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@ 
--
-- * 'ngcReplicaCount' - The number of read replica nodes in this node group (shard).
--
-- * 'ngcPrimaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard) is launched.
--
-- * 'ngcReplicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- * 'ngcNodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
nodeGroupConfiguration
    :: NodeGroupConfiguration
nodeGroupConfiguration
  = NodeGroupConfiguration'{_ngcSlots = Nothing,
                            _ngcReplicaCount = Nothing,
                            _ngcPrimaryAvailabilityZone = Nothing,
                            _ngcReplicaAvailabilityZones = Nothing,
                            _ngcNodeGroupId = Nothing}

-- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@ 
ngcSlots :: Lens' NodeGroupConfiguration (Maybe Text)
ngcSlots = lens _ngcSlots (\ s a -> s{_ngcSlots = a})

-- | The number of read replica nodes in this node group (shard).
ngcReplicaCount :: Lens' NodeGroupConfiguration (Maybe Int)
ngcReplicaCount = lens _ngcReplicaCount (\ s a -> s{_ngcReplicaCount = a})

-- | The Availability Zone where the primary node of this node group (shard) is launched.
ngcPrimaryAvailabilityZone :: Lens' NodeGroupConfiguration (Maybe Text)
ngcPrimaryAvailabilityZone = lens _ngcPrimaryAvailabilityZone (\ s a -> s{_ngcPrimaryAvailabilityZone = a})

-- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
ngcReplicaAvailabilityZones :: Lens' NodeGroupConfiguration [Text]
ngcReplicaAvailabilityZones = lens _ngcReplicaAvailabilityZones (\ s a -> s{_ngcReplicaAvailabilityZones = a}) . _Default . _Coerce

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied id for the node group these configuration values apply to.
ngcNodeGroupId :: Lens' NodeGroupConfiguration (Maybe Text)
ngcNodeGroupId = lens _ngcNodeGroupId (\ s a -> s{_ngcNodeGroupId = a})

instance FromXML NodeGroupConfiguration where
        parseXML x
          = NodeGroupConfiguration' <$>
              (x .@? "Slots") <*> (x .@? "ReplicaCount") <*>
                (x .@? "PrimaryAvailabilityZone")
                <*>
                (x .@? "ReplicaAvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "NodeGroupId")

instance Hashable NodeGroupConfiguration where

instance NFData NodeGroupConfiguration where

instance ToQuery NodeGroupConfiguration where
        toQuery NodeGroupConfiguration'{..}
          = mconcat
              ["Slots" =: _ngcSlots,
               "ReplicaCount" =: _ngcReplicaCount,
               "PrimaryAvailabilityZone" =:
                 _ngcPrimaryAvailabilityZone,
               "ReplicaAvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _ngcReplicaAvailabilityZones),
               "NodeGroupId" =: _ngcNodeGroupId]
