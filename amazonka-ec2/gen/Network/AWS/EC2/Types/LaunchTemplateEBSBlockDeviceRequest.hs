{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.VolumeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters for a block device for an EBS volume.
--
--
--
-- /See:/ 'launchTemplateEBSBlockDeviceRequest' smart constructor.
data LaunchTemplateEBSBlockDeviceRequest = LaunchTemplateEBSBlockDeviceRequest'{_ltebdrDeleteOnTermination
                                                                                ::
                                                                                !(Maybe
                                                                                    Bool),
                                                                                _ltebdrVolumeSize
                                                                                ::
                                                                                !(Maybe
                                                                                    Int),
                                                                                _ltebdrIOPS
                                                                                ::
                                                                                !(Maybe
                                                                                    Int),
                                                                                _ltebdrEncrypted
                                                                                ::
                                                                                !(Maybe
                                                                                    Bool),
                                                                                _ltebdrKMSKeyId
                                                                                ::
                                                                                !(Maybe
                                                                                    Text),
                                                                                _ltebdrVolumeType
                                                                                ::
                                                                                !(Maybe
                                                                                    VolumeType),
                                                                                _ltebdrSnapshotId
                                                                                ::
                                                                                !(Maybe
                                                                                    Text)}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'LaunchTemplateEBSBlockDeviceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltebdrDeleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
--
-- * 'ltebdrVolumeSize' - The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'ltebdrIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For io1, this represents the number of IOPS that are provisioned for the volume. For gp2, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Condition: This parameter is required for requests to create io1 volumes; it is not used in requests to create gp2, st1, sc1, or standard volumes.
--
-- * 'ltebdrEncrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
--
-- * 'ltebdrKMSKeyId' - The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- * 'ltebdrVolumeType' - The volume type.
--
-- * 'ltebdrSnapshotId' - The ID of the snapshot.
launchTemplateEBSBlockDeviceRequest
    :: LaunchTemplateEBSBlockDeviceRequest
launchTemplateEBSBlockDeviceRequest
  = LaunchTemplateEBSBlockDeviceRequest'{_ltebdrDeleteOnTermination
                                           = Nothing,
                                         _ltebdrVolumeSize = Nothing,
                                         _ltebdrIOPS = Nothing,
                                         _ltebdrEncrypted = Nothing,
                                         _ltebdrKMSKeyId = Nothing,
                                         _ltebdrVolumeType = Nothing,
                                         _ltebdrSnapshotId = Nothing}

-- | Indicates whether the EBS volume is deleted on instance termination.
ltebdrDeleteOnTermination :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Bool)
ltebdrDeleteOnTermination = lens _ltebdrDeleteOnTermination (\ s a -> s{_ltebdrDeleteOnTermination = a})

-- | The size of the volume, in GiB. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
ltebdrVolumeSize :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Int)
ltebdrVolumeSize = lens _ltebdrVolumeSize (\ s a -> s{_ltebdrVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For io1, this represents the number of IOPS that are provisioned for the volume. For gp2, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information about General Purpose SSD baseline performance, I/O credits, and bursting, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . Condition: This parameter is required for requests to create io1 volumes; it is not used in requests to create gp2, st1, sc1, or standard volumes.
ltebdrIOPS :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Int)
ltebdrIOPS = lens _ltebdrIOPS (\ s a -> s{_ltebdrIOPS = a})

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
ltebdrEncrypted :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Bool)
ltebdrEncrypted = lens _ltebdrEncrypted (\ s a -> s{_ltebdrEncrypted = a})

-- | The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
ltebdrKMSKeyId :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Text)
ltebdrKMSKeyId = lens _ltebdrKMSKeyId (\ s a -> s{_ltebdrKMSKeyId = a})

-- | The volume type.
ltebdrVolumeType :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe VolumeType)
ltebdrVolumeType = lens _ltebdrVolumeType (\ s a -> s{_ltebdrVolumeType = a})

-- | The ID of the snapshot.
ltebdrSnapshotId :: Lens' LaunchTemplateEBSBlockDeviceRequest (Maybe Text)
ltebdrSnapshotId = lens _ltebdrSnapshotId (\ s a -> s{_ltebdrSnapshotId = a})

instance Hashable LaunchTemplateEBSBlockDeviceRequest
         where

instance NFData LaunchTemplateEBSBlockDeviceRequest
         where

instance ToQuery LaunchTemplateEBSBlockDeviceRequest
         where
        toQuery LaunchTemplateEBSBlockDeviceRequest'{..}
          = mconcat
              ["DeleteOnTermination" =: _ltebdrDeleteOnTermination,
               "VolumeSize" =: _ltebdrVolumeSize,
               "Iops" =: _ltebdrIOPS,
               "Encrypted" =: _ltebdrEncrypted,
               "KmsKeyId" =: _ltebdrKMSKeyId,
               "VolumeType" =: _ltebdrVolumeType,
               "SnapshotId" =: _ltebdrSnapshotId]
