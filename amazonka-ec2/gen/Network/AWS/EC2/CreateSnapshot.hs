{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an EBS volume and stores it in Amazon S3. You can use snapshots for backups, to make copies of EBS volumes, and to save data before shutting down an instance.
--
--
-- When a snapshot is created, any AWS Marketplace product codes that are associated with the source volume are propagated to the snapshot.
--
-- You can take a snapshot of an attached volume that is in use. However, snapshots only capture data that has been written to your EBS volume at the time the snapshot command is issued; this may exclude any data that has been cached by any applications or the operating system. If you can pause any file systems on the volume long enough to take a snapshot, your snapshot should be complete. However, if you cannot pause all file writes to the volume, you should unmount the volume from within the instance, issue the snapshot command, and then remount the volume to ensure a consistent and complete snapshot. You may remount and use your volume while the snapshot status is @pending@ .
--
-- To create a snapshot for EBS volumes that serve as root devices, you should stop the instance before taking the snapshot.
--
-- Snapshots that are taken from encrypted volumes are automatically encrypted. Volumes that are created from encrypted snapshots are also automatically encrypted. Your encrypted volumes and any associated snapshots always remain protected.
--
-- You can tag your snapshots during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store> and <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateSnapshot
    (
    -- * Creating a Request
      createSnapshot
    , CreateSnapshot
    -- * Request Lenses
    , crtsnpshtTagSpecifications
    , crtsnpshtDescription
    , crtsnpshtDryRun
    , crtsnpshtVolumeId

    -- * Destructuring the Response
    , snapshot
    , Snapshot
    -- * Response Lenses
    , sStateMessage
    , sOwnerAlias
    , sDataEncryptionKeyId
    , sKMSKeyId
    , sTags
    , sSnapshotId
    , sOwnerId
    , sVolumeId
    , sVolumeSize
    , sDescription
    , sStartTime
    , sProgress
    , sState
    , sEncrypted
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'{_crtsnpshtTagSpecifications
                                      :: !(Maybe [TagSpecification]),
                                      _crtsnpshtDescription :: !(Maybe Text),
                                      _crtsnpshtDryRun :: !(Maybe Bool),
                                      _crtsnpshtVolumeId :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtsnpshtTagSpecifications' - The tags to apply to the snapshot during creation.
--
-- * 'crtsnpshtDescription' - A description for the snapshot.
--
-- * 'crtsnpshtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'crtsnpshtVolumeId' - The ID of the EBS volume.
createSnapshot
    :: Text -- ^ 'crtsnpshtVolumeId'
    -> CreateSnapshot
createSnapshot pVolumeId_
  = CreateSnapshot'{_crtsnpshtTagSpecifications =
                      Nothing,
                    _crtsnpshtDescription = Nothing,
                    _crtsnpshtDryRun = Nothing,
                    _crtsnpshtVolumeId = pVolumeId_}

-- | The tags to apply to the snapshot during creation.
crtsnpshtTagSpecifications :: Lens' CreateSnapshot [TagSpecification]
crtsnpshtTagSpecifications = lens _crtsnpshtTagSpecifications (\ s a -> s{_crtsnpshtTagSpecifications = a}) . _Default . _Coerce

-- | A description for the snapshot.
crtsnpshtDescription :: Lens' CreateSnapshot (Maybe Text)
crtsnpshtDescription = lens _crtsnpshtDescription (\ s a -> s{_crtsnpshtDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
crtsnpshtDryRun :: Lens' CreateSnapshot (Maybe Bool)
crtsnpshtDryRun = lens _crtsnpshtDryRun (\ s a -> s{_crtsnpshtDryRun = a})

-- | The ID of the EBS volume.
crtsnpshtVolumeId :: Lens' CreateSnapshot Text
crtsnpshtVolumeId = lens _crtsnpshtVolumeId (\ s a -> s{_crtsnpshtVolumeId = a})

instance AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = Snapshot
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable CreateSnapshot where

instance NFData CreateSnapshot where

instance ToHeaders CreateSnapshot where
        toHeaders = const mempty

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery CreateSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateSnapshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _crtsnpshtTagSpecifications),
               "Description" =: _crtsnpshtDescription,
               "DryRun" =: _crtsnpshtDryRun,
               "VolumeId" =: _crtsnpshtVolumeId]
