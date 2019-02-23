{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.Product where

import Network.AWS.EFS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A description of the file system.
--
--
--
-- /See:/ 'fileSystemDescription' smart constructor.
data FileSystemDescription = FileSystemDescription'
  { _fsdProvisionedThroughputInMibps :: !(Maybe Double)
  , _fsdEncrypted                    :: !(Maybe Bool)
  , _fsdThroughputMode               :: !(Maybe ThroughputMode)
  , _fsdKMSKeyId                     :: !(Maybe Text)
  , _fsdName                         :: !(Maybe Text)
  , _fsdOwnerId                      :: !Text
  , _fsdCreationToken                :: !Text
  , _fsdFileSystemId                 :: !Text
  , _fsdCreationTime                 :: !POSIX
  , _fsdLifeCycleState               :: !LifeCycleState
  , _fsdNumberOfMountTargets         :: !Nat
  , _fsdSizeInBytes                  :: !FileSystemSize
  , _fsdPerformanceMode              :: !PerformanceMode
  , _fsdTags                         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSystemDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsdProvisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system. The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- * 'fsdEncrypted' - A Boolean value that, if true, indicates that the file system is encrypted.
--
-- * 'fsdThroughputMode' - The throughput mode for a file system. There are two throughput modes to choose from for your file system: bursting and provisioned. You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
--
-- * 'fsdKMSKeyId' - The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
--
-- * 'fsdName' - You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
--
-- * 'fsdOwnerId' - The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
--
-- * 'fsdCreationToken' - The opaque string specified in the request.
--
-- * 'fsdFileSystemId' - The ID of the file system, assigned by Amazon EFS.
--
-- * 'fsdCreationTime' - The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
--
-- * 'fsdLifeCycleState' - The lifecycle phase of the file system.
--
-- * 'fsdNumberOfMountTargets' - The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
--
-- * 'fsdSizeInBytes' - The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
--
-- * 'fsdPerformanceMode' - The performance mode of the file system.
--
-- * 'fsdTags' - The tags associated with the file system, presented as an array of @Tag@ objects.
fileSystemDescription
    :: Text -- ^ 'fsdOwnerId'
    -> Text -- ^ 'fsdCreationToken'
    -> Text -- ^ 'fsdFileSystemId'
    -> UTCTime -- ^ 'fsdCreationTime'
    -> LifeCycleState -- ^ 'fsdLifeCycleState'
    -> Natural -- ^ 'fsdNumberOfMountTargets'
    -> FileSystemSize -- ^ 'fsdSizeInBytes'
    -> PerformanceMode -- ^ 'fsdPerformanceMode'
    -> FileSystemDescription
fileSystemDescription pOwnerId_ pCreationToken_ pFileSystemId_ pCreationTime_ pLifeCycleState_ pNumberOfMountTargets_ pSizeInBytes_ pPerformanceMode_ =
  FileSystemDescription'
    { _fsdProvisionedThroughputInMibps = Nothing
    , _fsdEncrypted = Nothing
    , _fsdThroughputMode = Nothing
    , _fsdKMSKeyId = Nothing
    , _fsdName = Nothing
    , _fsdOwnerId = pOwnerId_
    , _fsdCreationToken = pCreationToken_
    , _fsdFileSystemId = pFileSystemId_
    , _fsdCreationTime = _Time # pCreationTime_
    , _fsdLifeCycleState = pLifeCycleState_
    , _fsdNumberOfMountTargets = _Nat # pNumberOfMountTargets_
    , _fsdSizeInBytes = pSizeInBytes_
    , _fsdPerformanceMode = pPerformanceMode_
    , _fsdTags = mempty
    }


-- | The throughput, measured in MiB/s, that you want to provision for a file system. The limit on throughput is 1024 MiB/s. You can get these limits increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
fsdProvisionedThroughputInMibps :: Lens' FileSystemDescription (Maybe Double)
fsdProvisionedThroughputInMibps = lens _fsdProvisionedThroughputInMibps (\ s a -> s{_fsdProvisionedThroughputInMibps = a})

-- | A Boolean value that, if true, indicates that the file system is encrypted.
fsdEncrypted :: Lens' FileSystemDescription (Maybe Bool)
fsdEncrypted = lens _fsdEncrypted (\ s a -> s{_fsdEncrypted = a})

-- | The throughput mode for a file system. There are two throughput modes to choose from for your file system: bursting and provisioned. You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change.
fsdThroughputMode :: Lens' FileSystemDescription (Maybe ThroughputMode)
fsdThroughputMode = lens _fsdThroughputMode (\ s a -> s{_fsdThroughputMode = a})

-- | The ID of an AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the encrypted file system.
fsdKMSKeyId :: Lens' FileSystemDescription (Maybe Text)
fsdKMSKeyId = lens _fsdKMSKeyId (\ s a -> s{_fsdKMSKeyId = a})

-- | You can add tags to a file system, including a @Name@ tag. For more information, see 'CreateFileSystem' . If the file system has a @Name@ tag, Amazon EFS returns the value in this field.
fsdName :: Lens' FileSystemDescription (Maybe Text)
fsdName = lens _fsdName (\ s a -> s{_fsdName = a})

-- | The AWS account that created the file system. If the file system was created by an IAM user, the parent account to which the user belongs is the owner.
fsdOwnerId :: Lens' FileSystemDescription Text
fsdOwnerId = lens _fsdOwnerId (\ s a -> s{_fsdOwnerId = a})

-- | The opaque string specified in the request.
fsdCreationToken :: Lens' FileSystemDescription Text
fsdCreationToken = lens _fsdCreationToken (\ s a -> s{_fsdCreationToken = a})

-- | The ID of the file system, assigned by Amazon EFS.
fsdFileSystemId :: Lens' FileSystemDescription Text
fsdFileSystemId = lens _fsdFileSystemId (\ s a -> s{_fsdFileSystemId = a})

-- | The time that the file system was created, in seconds (since 1970-01-01T00:00:00Z).
fsdCreationTime :: Lens' FileSystemDescription UTCTime
fsdCreationTime = lens _fsdCreationTime (\ s a -> s{_fsdCreationTime = a}) . _Time

-- | The lifecycle phase of the file system.
fsdLifeCycleState :: Lens' FileSystemDescription LifeCycleState
fsdLifeCycleState = lens _fsdLifeCycleState (\ s a -> s{_fsdLifeCycleState = a})

-- | The current number of mount targets that the file system has. For more information, see 'CreateMountTarget' .
fsdNumberOfMountTargets :: Lens' FileSystemDescription Natural
fsdNumberOfMountTargets = lens _fsdNumberOfMountTargets (\ s a -> s{_fsdNumberOfMountTargets = a}) . _Nat

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The @Timestamp@ value is the integer number of seconds since 1970-01-01T00:00:00Z. The @SizeInBytes@ value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, @SizeInBytes@ represents actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not the exact size that the file system was at any point in time.
fsdSizeInBytes :: Lens' FileSystemDescription FileSystemSize
fsdSizeInBytes = lens _fsdSizeInBytes (\ s a -> s{_fsdSizeInBytes = a})

-- | The performance mode of the file system.
fsdPerformanceMode :: Lens' FileSystemDescription PerformanceMode
fsdPerformanceMode = lens _fsdPerformanceMode (\ s a -> s{_fsdPerformanceMode = a})

-- | The tags associated with the file system, presented as an array of @Tag@ objects.
fsdTags :: Lens' FileSystemDescription [Tag]
fsdTags = lens _fsdTags (\ s a -> s{_fsdTags = a}) . _Coerce

instance FromJSON FileSystemDescription where
        parseJSON
          = withObject "FileSystemDescription"
              (\ x ->
                 FileSystemDescription' <$>
                   (x .:? "ProvisionedThroughputInMibps") <*>
                     (x .:? "Encrypted")
                     <*> (x .:? "ThroughputMode")
                     <*> (x .:? "KmsKeyId")
                     <*> (x .:? "Name")
                     <*> (x .: "OwnerId")
                     <*> (x .: "CreationToken")
                     <*> (x .: "FileSystemId")
                     <*> (x .: "CreationTime")
                     <*> (x .: "LifeCycleState")
                     <*> (x .: "NumberOfMountTargets")
                     <*> (x .: "SizeInBytes")
                     <*> (x .: "PerformanceMode")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable FileSystemDescription where

instance NFData FileSystemDescription where

-- | The latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. The value doesn't represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value represents the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.
--
--
--
-- /See:/ 'fileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'
  { _fssValueInIA       :: !(Maybe Nat)
  , _fssValueInStandard :: !(Maybe Nat)
  , _fssTimestamp       :: !(Maybe POSIX)
  , _fssValue           :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FileSystemSize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssValueInIA' - The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
--
-- * 'fssValueInStandard' - The latest known metered size (in bytes) of data stored in the Standard storage class.
--
-- * 'fssTimestamp' - The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
--
-- * 'fssValue' - The latest known metered size (in bytes) of data stored in the file system.
fileSystemSize
    :: Natural -- ^ 'fssValue'
    -> FileSystemSize
fileSystemSize pValue_ =
  FileSystemSize'
    { _fssValueInIA = Nothing
    , _fssValueInStandard = Nothing
    , _fssTimestamp = Nothing
    , _fssValue = _Nat # pValue_
    }


-- | The latest known metered size (in bytes) of data stored in the Infrequent Access storage class.
fssValueInIA :: Lens' FileSystemSize (Maybe Natural)
fssValueInIA = lens _fssValueInIA (\ s a -> s{_fssValueInIA = a}) . mapping _Nat

-- | The latest known metered size (in bytes) of data stored in the Standard storage class.
fssValueInStandard :: Lens' FileSystemSize (Maybe Natural)
fssValueInStandard = lens _fssValueInStandard (\ s a -> s{_fssValueInStandard = a}) . mapping _Nat

-- | The time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
fssTimestamp :: Lens' FileSystemSize (Maybe UTCTime)
fssTimestamp = lens _fssTimestamp (\ s a -> s{_fssTimestamp = a}) . mapping _Time

-- | The latest known metered size (in bytes) of data stored in the file system.
fssValue :: Lens' FileSystemSize Natural
fssValue = lens _fssValue (\ s a -> s{_fssValue = a}) . _Nat

instance FromJSON FileSystemSize where
        parseJSON
          = withObject "FileSystemSize"
              (\ x ->
                 FileSystemSize' <$>
                   (x .:? "ValueInIA") <*> (x .:? "ValueInStandard") <*>
                     (x .:? "Timestamp")
                     <*> (x .: "Value"))

instance Hashable FileSystemSize where

instance NFData FileSystemSize where

-- | /See:/ 'lifecycleConfigurationDescription' smart constructor.
newtype LifecycleConfigurationDescription = LifecycleConfigurationDescription'
  { _lcdLifecyclePolicies :: Maybe [LifecyclePolicy]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdLifecyclePolicies' - An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
lifecycleConfigurationDescription
    :: LifecycleConfigurationDescription
lifecycleConfigurationDescription =
  LifecycleConfigurationDescription' {_lcdLifecyclePolicies = Nothing}


-- | An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
lcdLifecyclePolicies :: Lens' LifecycleConfigurationDescription [LifecyclePolicy]
lcdLifecyclePolicies = lens _lcdLifecyclePolicies (\ s a -> s{_lcdLifecyclePolicies = a}) . _Default . _Coerce

instance FromJSON LifecycleConfigurationDescription
         where
        parseJSON
          = withObject "LifecycleConfigurationDescription"
              (\ x ->
                 LifecycleConfigurationDescription' <$>
                   (x .:? "LifecyclePolicies" .!= mempty))

instance Hashable LifecycleConfigurationDescription
         where

instance NFData LifecycleConfigurationDescription
         where

-- | Describes a policy used by EFS lifecycle management to transition files to the Infrequent Access (IA) storage class.
--
--
--
-- /See:/ 'lifecyclePolicy' smart constructor.
newtype LifecyclePolicy = LifecyclePolicy'
  { _lpTransitionToIA :: Maybe TransitionToIARules
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecyclePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpTransitionToIA' - A value that indicates how long it takes to transition files to the IA storage class. Currently, the only valid value is @AFTER_30_DAYS@ . @AFTER_30_DAYS@ indicates files that have not been read from or written to for 30 days are transitioned from the Standard storage class to the IA storage class. Metadata operations such as listing the contents of a directory don't count as a file access event.
lifecyclePolicy
    :: LifecyclePolicy
lifecyclePolicy = LifecyclePolicy' {_lpTransitionToIA = Nothing}


-- | A value that indicates how long it takes to transition files to the IA storage class. Currently, the only valid value is @AFTER_30_DAYS@ . @AFTER_30_DAYS@ indicates files that have not been read from or written to for 30 days are transitioned from the Standard storage class to the IA storage class. Metadata operations such as listing the contents of a directory don't count as a file access event.
lpTransitionToIA :: Lens' LifecyclePolicy (Maybe TransitionToIARules)
lpTransitionToIA = lens _lpTransitionToIA (\ s a -> s{_lpTransitionToIA = a})

instance FromJSON LifecyclePolicy where
        parseJSON
          = withObject "LifecyclePolicy"
              (\ x ->
                 LifecyclePolicy' <$> (x .:? "TransitionToIA"))

instance Hashable LifecyclePolicy where

instance NFData LifecyclePolicy where

instance ToJSON LifecyclePolicy where
        toJSON LifecyclePolicy'{..}
          = object
              (catMaybes
                 [("TransitionToIA" .=) <$> _lpTransitionToIA])

-- | Provides a description of a mount target.
--
--
--
-- /See:/ 'mountTargetDescription' smart constructor.
data MountTargetDescription = MountTargetDescription'
  { _mtdIPAddress          :: !(Maybe Text)
  , _mtdNetworkInterfaceId :: !(Maybe Text)
  , _mtdOwnerId            :: !(Maybe Text)
  , _mtdMountTargetId      :: !Text
  , _mtdFileSystemId       :: !Text
  , _mtdSubnetId           :: !Text
  , _mtdLifeCycleState     :: !LifeCycleState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MountTargetDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtdIPAddress' - Address at which the file system can be mounted by using the mount target.
--
-- * 'mtdNetworkInterfaceId' - The ID of the network interface that Amazon EFS created when it created the mount target.
--
-- * 'mtdOwnerId' - AWS account ID that owns the resource.
--
-- * 'mtdMountTargetId' - System-assigned mount target ID.
--
-- * 'mtdFileSystemId' - The ID of the file system for which the mount target is intended.
--
-- * 'mtdSubnetId' - The ID of the mount target's subnet.
--
-- * 'mtdLifeCycleState' - Lifecycle state of the mount target.
mountTargetDescription
    :: Text -- ^ 'mtdMountTargetId'
    -> Text -- ^ 'mtdFileSystemId'
    -> Text -- ^ 'mtdSubnetId'
    -> LifeCycleState -- ^ 'mtdLifeCycleState'
    -> MountTargetDescription
mountTargetDescription pMountTargetId_ pFileSystemId_ pSubnetId_ pLifeCycleState_ =
  MountTargetDescription'
    { _mtdIPAddress = Nothing
    , _mtdNetworkInterfaceId = Nothing
    , _mtdOwnerId = Nothing
    , _mtdMountTargetId = pMountTargetId_
    , _mtdFileSystemId = pFileSystemId_
    , _mtdSubnetId = pSubnetId_
    , _mtdLifeCycleState = pLifeCycleState_
    }


-- | Address at which the file system can be mounted by using the mount target.
mtdIPAddress :: Lens' MountTargetDescription (Maybe Text)
mtdIPAddress = lens _mtdIPAddress (\ s a -> s{_mtdIPAddress = a})

-- | The ID of the network interface that Amazon EFS created when it created the mount target.
mtdNetworkInterfaceId :: Lens' MountTargetDescription (Maybe Text)
mtdNetworkInterfaceId = lens _mtdNetworkInterfaceId (\ s a -> s{_mtdNetworkInterfaceId = a})

-- | AWS account ID that owns the resource.
mtdOwnerId :: Lens' MountTargetDescription (Maybe Text)
mtdOwnerId = lens _mtdOwnerId (\ s a -> s{_mtdOwnerId = a})

-- | System-assigned mount target ID.
mtdMountTargetId :: Lens' MountTargetDescription Text
mtdMountTargetId = lens _mtdMountTargetId (\ s a -> s{_mtdMountTargetId = a})

-- | The ID of the file system for which the mount target is intended.
mtdFileSystemId :: Lens' MountTargetDescription Text
mtdFileSystemId = lens _mtdFileSystemId (\ s a -> s{_mtdFileSystemId = a})

-- | The ID of the mount target's subnet.
mtdSubnetId :: Lens' MountTargetDescription Text
mtdSubnetId = lens _mtdSubnetId (\ s a -> s{_mtdSubnetId = a})

-- | Lifecycle state of the mount target.
mtdLifeCycleState :: Lens' MountTargetDescription LifeCycleState
mtdLifeCycleState = lens _mtdLifeCycleState (\ s a -> s{_mtdLifeCycleState = a})

instance FromJSON MountTargetDescription where
        parseJSON
          = withObject "MountTargetDescription"
              (\ x ->
                 MountTargetDescription' <$>
                   (x .:? "IpAddress") <*> (x .:? "NetworkInterfaceId")
                     <*> (x .:? "OwnerId")
                     <*> (x .: "MountTargetId")
                     <*> (x .: "FileSystemId")
                     <*> (x .: "SubnetId")
                     <*> (x .: "LifeCycleState"))

instance Hashable MountTargetDescription where

instance NFData MountTargetDescription where

-- | A tag is a key-value pair. Allowed characters are letters, white space, and numbers that can be represented in UTF-8, and the following characters:@+ - = . _ : /@
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The tag key (String). The key can't start with @aws:@ .
--
-- * 'tagValue' - The value of the tag key.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The tag key (String). The key can't start with @aws:@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value of the tag key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])
