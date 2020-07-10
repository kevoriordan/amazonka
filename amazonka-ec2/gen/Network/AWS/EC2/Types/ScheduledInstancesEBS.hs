{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesEBS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ScheduledInstancesEBS where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EBS volume for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesEBS' smart constructor.
data ScheduledInstancesEBS = ScheduledInstancesEBS'{_sieDeleteOnTermination
                                                    :: !(Maybe Bool),
                                                    _sieVolumeSize ::
                                                    !(Maybe Int),
                                                    _sieIOPS :: !(Maybe Int),
                                                    _sieEncrypted ::
                                                    !(Maybe Bool),
                                                    _sieVolumeType ::
                                                    !(Maybe Text),
                                                    _sieSnapshotId ::
                                                    !(Maybe Text)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ScheduledInstancesEBS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sieDeleteOnTermination' - Indicates whether the volume is deleted on instance termination.
--
-- * 'sieVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'sieIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For io1 volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about @gp2@ baseline performance, I/O credits, and bursting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- * 'sieEncrypted' - Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
--
-- * 'sieVolumeType' - The volume type. @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @gp2@ 
--
-- * 'sieSnapshotId' - The ID of the snapshot.
scheduledInstancesEBS
    :: ScheduledInstancesEBS
scheduledInstancesEBS
  = ScheduledInstancesEBS'{_sieDeleteOnTermination =
                             Nothing,
                           _sieVolumeSize = Nothing, _sieIOPS = Nothing,
                           _sieEncrypted = Nothing, _sieVolumeType = Nothing,
                           _sieSnapshotId = Nothing}

-- | Indicates whether the volume is deleted on instance termination.
sieDeleteOnTermination :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieDeleteOnTermination = lens _sieDeleteOnTermination (\ s a -> s{_sieDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
sieVolumeSize :: Lens' ScheduledInstancesEBS (Maybe Int)
sieVolumeSize = lens _sieVolumeSize (\ s a -> s{_sieVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For io1 volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about @gp2@ baseline performance, I/O credits, and bursting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Constraint: Range is 100-20000 IOPS for @io1@ volumes and 100-10000 IOPS for @gp2@ volumes. Condition: This parameter is required for requests to create @io1@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
sieIOPS :: Lens' ScheduledInstancesEBS (Maybe Int)
sieIOPS = lens _sieIOPS (\ s a -> s{_sieIOPS = a})

-- | Indicates whether the volume is encrypted. You can attached encrypted volumes only to instances that support them.
sieEncrypted :: Lens' ScheduledInstancesEBS (Maybe Bool)
sieEncrypted = lens _sieEncrypted (\ s a -> s{_sieEncrypted = a})

-- | The volume type. @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, Throughput Optimized HDD for @st1@ , Cold HDD for @sc1@ , or @standard@ for Magnetic. Default: @gp2@ 
sieVolumeType :: Lens' ScheduledInstancesEBS (Maybe Text)
sieVolumeType = lens _sieVolumeType (\ s a -> s{_sieVolumeType = a})

-- | The ID of the snapshot.
sieSnapshotId :: Lens' ScheduledInstancesEBS (Maybe Text)
sieSnapshotId = lens _sieSnapshotId (\ s a -> s{_sieSnapshotId = a})

instance Hashable ScheduledInstancesEBS where

instance NFData ScheduledInstancesEBS where

instance ToQuery ScheduledInstancesEBS where
        toQuery ScheduledInstancesEBS'{..}
          = mconcat
              ["DeleteOnTermination" =: _sieDeleteOnTermination,
               "VolumeSize" =: _sieVolumeSize, "Iops" =: _sieIOPS,
               "Encrypted" =: _sieEncrypted,
               "VolumeType" =: _sieVolumeType,
               "SnapshotId" =: _sieSnapshotId]
