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
-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EBS volume that can be attached to an instance in the same Availability Zone. The volume is created in the regional endpoint that you send the HTTP request to. For more information see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
--
-- You can create a new empty volume or restore a volume from an EBS snapshot. Any AWS Marketplace product codes from the snapshot are propagated to the volume.
--
-- You can create encrypted volumes. Encrypted volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are also automatically encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can tag your volumes during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating an Amazon EBS Volume> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVolume
    (
    -- * Creating a Request
      createVolume
    , CreateVolume
    -- * Request Lenses
    , crtvlmMultiAttachEnabled
    , crtvlmSize
    , crtvlmIOPS
    , crtvlmOutpostARN
    , crtvlmEncrypted
    , crtvlmTagSpecifications
    , crtvlmKMSKeyId
    , crtvlmVolumeType
    , crtvlmDryRun
    , crtvlmSnapshotId
    , crtvlmAvailabilityZone

    -- * Destructuring the Response
    , volume
    , Volume
    -- * Response Lenses
    , vFastRestored
    , vMultiAttachEnabled
    , vAttachments
    , vIOPS
    , vOutpostARN
    , vKMSKeyId
    , vTags
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vSize
    , vSnapshotId
    , vState
    , vVolumeId
    , vVolumeType
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVolume' smart constructor.
data CreateVolume = CreateVolume'{_crtvlmMultiAttachEnabled
                                  :: !(Maybe Bool),
                                  _crtvlmSize :: !(Maybe Int),
                                  _crtvlmIOPS :: !(Maybe Int),
                                  _crtvlmOutpostARN :: !(Maybe Text),
                                  _crtvlmEncrypted :: !(Maybe Bool),
                                  _crtvlmTagSpecifications ::
                                  !(Maybe [TagSpecification]),
                                  _crtvlmKMSKeyId :: !(Maybe Text),
                                  _crtvlmVolumeType :: !(Maybe VolumeType),
                                  _crtvlmDryRun :: !(Maybe Bool),
                                  _crtvlmSnapshotId :: !(Maybe Text),
                                  _crtvlmAvailabilityZone :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtvlmMultiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'crtvlmSize' - The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size. Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'crtvlmIOPS' - The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (io1) volumes.
--
-- * 'crtvlmOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'crtvlmEncrypted' - Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by Default> in the /Amazon Elastic Compute Cloud User Guide/ . Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> .
--
-- * 'crtvlmTagSpecifications' - The tags to apply to the volume during creation.
--
-- * 'crtvlmKMSKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .     * Alias ARN. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ . AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
--
-- * 'crtvlmVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @gp2@ 
--
-- * 'crtvlmDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'crtvlmSnapshotId' - The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
--
-- * 'crtvlmAvailabilityZone' - The Availability Zone in which to create the volume.
createVolume
    :: Text -- ^ 'crtvlmAvailabilityZone'
    -> CreateVolume
createVolume pAvailabilityZone_
  = CreateVolume'{_crtvlmMultiAttachEnabled = Nothing,
                  _crtvlmSize = Nothing, _crtvlmIOPS = Nothing,
                  _crtvlmOutpostARN = Nothing,
                  _crtvlmEncrypted = Nothing,
                  _crtvlmTagSpecifications = Nothing,
                  _crtvlmKMSKeyId = Nothing,
                  _crtvlmVolumeType = Nothing, _crtvlmDryRun = Nothing,
                  _crtvlmSnapshotId = Nothing,
                  _crtvlmAvailabilityZone = pAvailabilityZone_}

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
crtvlmMultiAttachEnabled :: Lens' CreateVolume (Maybe Bool)
crtvlmMultiAttachEnabled = lens _crtvlmMultiAttachEnabled (\ s a -> s{_crtvlmMultiAttachEnabled = a})

-- | The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size. Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
crtvlmSize :: Lens' CreateVolume (Maybe Int)
crtvlmSize = lens _crtvlmSize (\ s a -> s{_crtvlmSize = a})

-- | The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (io1) volumes.
crtvlmIOPS :: Lens' CreateVolume (Maybe Int)
crtvlmIOPS = lens _crtvlmIOPS (\ s a -> s{_crtvlmIOPS = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
crtvlmOutpostARN :: Lens' CreateVolume (Maybe Text)
crtvlmOutpostARN = lens _crtvlmOutpostARN (\ s a -> s{_crtvlmOutpostARN = a})

-- | Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by Default> in the /Amazon Elastic Compute Cloud User Guide/ . Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported Instance Types> .
crtvlmEncrypted :: Lens' CreateVolume (Maybe Bool)
crtvlmEncrypted = lens _crtvlmEncrypted (\ s a -> s{_crtvlmEncrypted = a})

-- | The tags to apply to the volume during creation.
crtvlmTagSpecifications :: Lens' CreateVolume [TagSpecification]
crtvlmTagSpecifications = lens _crtvlmTagSpecifications (\ s a -> s{_crtvlmTagSpecifications = a}) . _Default . _Coerce

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .     * Alias ARN. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ . AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
crtvlmKMSKeyId :: Lens' CreateVolume (Maybe Text)
crtvlmKMSKeyId = lens _crtvlmKMSKeyId (\ s a -> s{_crtvlmKMSKeyId = a})

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @gp2@ 
crtvlmVolumeType :: Lens' CreateVolume (Maybe VolumeType)
crtvlmVolumeType = lens _crtvlmVolumeType (\ s a -> s{_crtvlmVolumeType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
crtvlmDryRun :: Lens' CreateVolume (Maybe Bool)
crtvlmDryRun = lens _crtvlmDryRun (\ s a -> s{_crtvlmDryRun = a})

-- | The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
crtvlmSnapshotId :: Lens' CreateVolume (Maybe Text)
crtvlmSnapshotId = lens _crtvlmSnapshotId (\ s a -> s{_crtvlmSnapshotId = a})

-- | The Availability Zone in which to create the volume.
crtvlmAvailabilityZone :: Lens' CreateVolume Text
crtvlmAvailabilityZone = lens _crtvlmAvailabilityZone (\ s a -> s{_crtvlmAvailabilityZone = a})

instance AWSRequest CreateVolume where
        type Rs CreateVolume = Volume
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable CreateVolume where

instance NFData CreateVolume where

instance ToHeaders CreateVolume where
        toHeaders = const mempty

instance ToPath CreateVolume where
        toPath = const "/"

instance ToQuery CreateVolume where
        toQuery CreateVolume'{..}
          = mconcat
              ["Action" =: ("CreateVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "MultiAttachEnabled" =: _crtvlmMultiAttachEnabled,
               "Size" =: _crtvlmSize, "Iops" =: _crtvlmIOPS,
               "OutpostArn" =: _crtvlmOutpostARN,
               "Encrypted" =: _crtvlmEncrypted,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _crtvlmTagSpecifications),
               "KmsKeyId" =: _crtvlmKMSKeyId,
               "VolumeType" =: _crtvlmVolumeType,
               "DryRun" =: _crtvlmDryRun,
               "SnapshotId" =: _crtvlmSnapshotId,
               "AvailabilityZone" =: _crtvlmAvailabilityZone]
