{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationPendingModifiedValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationPendingModifiedValues where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the values of pending modifications to a replication instance. This data type is an object of the @ReplicationInstance@ user-defined data type. 
--
--
--
-- /See:/ 'replicationPendingModifiedValues' smart constructor.
data ReplicationPendingModifiedValues = ReplicationPendingModifiedValues'{_rpmvEngineVersion
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _rpmvMultiAZ
                                                                          ::
                                                                          !(Maybe
                                                                              Bool),
                                                                          _rpmvAllocatedStorage
                                                                          ::
                                                                          !(Maybe
                                                                              Int),
                                                                          _rpmvReplicationInstanceClass
                                                                          ::
                                                                          !(Maybe
                                                                              Text)}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'ReplicationPendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpmvEngineVersion' - The engine version number of the replication instance.
--
-- * 'rpmvMultiAZ' - Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ . 
--
-- * 'rpmvAllocatedStorage' - The amount of storage (in gigabytes) that is allocated for the replication instance.
--
-- * 'rpmvReplicationInstanceClass' - The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @ 
replicationPendingModifiedValues
    :: ReplicationPendingModifiedValues
replicationPendingModifiedValues
  = ReplicationPendingModifiedValues'{_rpmvEngineVersion
                                        = Nothing,
                                      _rpmvMultiAZ = Nothing,
                                      _rpmvAllocatedStorage = Nothing,
                                      _rpmvReplicationInstanceClass = Nothing}

-- | The engine version number of the replication instance.
rpmvEngineVersion :: Lens' ReplicationPendingModifiedValues (Maybe Text)
rpmvEngineVersion = lens _rpmvEngineVersion (\ s a -> s{_rpmvEngineVersion = a})

-- | Specifies whether the replication instance is a Multi-AZ deployment. You can't set the @AvailabilityZone@ parameter if the Multi-AZ parameter is set to @true@ . 
rpmvMultiAZ :: Lens' ReplicationPendingModifiedValues (Maybe Bool)
rpmvMultiAZ = lens _rpmvMultiAZ (\ s a -> s{_rpmvMultiAZ = a})

-- | The amount of storage (in gigabytes) that is allocated for the replication instance.
rpmvAllocatedStorage :: Lens' ReplicationPendingModifiedValues (Maybe Int)
rpmvAllocatedStorage = lens _rpmvAllocatedStorage (\ s a -> s{_rpmvAllocatedStorage = a})

-- | The compute and memory capacity of the replication instance. Valid Values: @dms.t2.micro | dms.t2.small | dms.t2.medium | dms.t2.large | dms.c4.large | dms.c4.xlarge | dms.c4.2xlarge | dms.c4.4xlarge @ 
rpmvReplicationInstanceClass :: Lens' ReplicationPendingModifiedValues (Maybe Text)
rpmvReplicationInstanceClass = lens _rpmvReplicationInstanceClass (\ s a -> s{_rpmvReplicationInstanceClass = a})

instance FromJSON ReplicationPendingModifiedValues
         where
        parseJSON
          = withObject "ReplicationPendingModifiedValues"
              (\ x ->
                 ReplicationPendingModifiedValues' <$>
                   (x .:? "EngineVersion") <*> (x .:? "MultiAZ") <*>
                     (x .:? "AllocatedStorage")
                     <*> (x .:? "ReplicationInstanceClass"))

instance Hashable ReplicationPendingModifiedValues
         where

instance NFData ReplicationPendingModifiedValues
         where
