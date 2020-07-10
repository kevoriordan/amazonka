{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTask where

import Network.AWS.DMS.Types.MigrationTypeValue
import Network.AWS.DMS.Types.ReplicationTaskStats
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that describes a replication task created by the @CreateReplicationTask@ operation.
--
--
--
-- /See:/ 'replicationTask' smart constructor.
data ReplicationTask = ReplicationTask'{_rReplicationTaskSettings
                                        :: !(Maybe Text),
                                        _rStatus :: !(Maybe Text),
                                        _rStopReason :: !(Maybe Text),
                                        _rTargetEndpointARN :: !(Maybe Text),
                                        _rReplicationTaskIdentifier ::
                                        !(Maybe Text),
                                        _rCdcStartPosition :: !(Maybe Text),
                                        _rReplicationTaskStartDate ::
                                        !(Maybe POSIX),
                                        _rSourceEndpointARN :: !(Maybe Text),
                                        _rRecoveryCheckpoint :: !(Maybe Text),
                                        _rTableMappings :: !(Maybe Text),
                                        _rReplicationTaskCreationDate ::
                                        !(Maybe POSIX),
                                        _rMigrationType ::
                                        !(Maybe MigrationTypeValue),
                                        _rReplicationTaskARN :: !(Maybe Text),
                                        _rTaskData :: !(Maybe Text),
                                        _rCdcStopPosition :: !(Maybe Text),
                                        _rReplicationTaskStats ::
                                        !(Maybe ReplicationTaskStats),
                                        _rReplicationInstanceARN ::
                                        !(Maybe Text),
                                        _rLastFailureMessage :: !(Maybe Text)}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rReplicationTaskSettings' - The settings for the replication task.
--
-- * 'rStatus' - The status of the replication task.
--
-- * 'rStopReason' - The reason the replication task was stopped.
--
-- * 'rTargetEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rReplicationTaskIdentifier' - The user-assigned replication task identifier or name. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'rCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- * 'rReplicationTaskStartDate' - The date the replication task is scheduled to start.
--
-- * 'rSourceEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- * 'rRecoveryCheckpoint' - Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
--
-- * 'rTableMappings' - Table mappings specified in the task.
--
-- * 'rReplicationTaskCreationDate' - The date the replication task was created.
--
-- * 'rMigrationType' - The type of migration.
--
-- * 'rReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
--
-- * 'rTaskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration User Guide./ 
--
-- * 'rCdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
--
-- * 'rReplicationTaskStats' - The statistics for the task, including elapsed time, tables loaded, and table errors.
--
-- * 'rReplicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
--
-- * 'rLastFailureMessage' - The last error (failure) message generated for the replication instance.
replicationTask
    :: ReplicationTask
replicationTask
  = ReplicationTask'{_rReplicationTaskSettings =
                       Nothing,
                     _rStatus = Nothing, _rStopReason = Nothing,
                     _rTargetEndpointARN = Nothing,
                     _rReplicationTaskIdentifier = Nothing,
                     _rCdcStartPosition = Nothing,
                     _rReplicationTaskStartDate = Nothing,
                     _rSourceEndpointARN = Nothing,
                     _rRecoveryCheckpoint = Nothing,
                     _rTableMappings = Nothing,
                     _rReplicationTaskCreationDate = Nothing,
                     _rMigrationType = Nothing,
                     _rReplicationTaskARN = Nothing, _rTaskData = Nothing,
                     _rCdcStopPosition = Nothing,
                     _rReplicationTaskStats = Nothing,
                     _rReplicationInstanceARN = Nothing,
                     _rLastFailureMessage = Nothing}

-- | The settings for the replication task.
rReplicationTaskSettings :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskSettings = lens _rReplicationTaskSettings (\ s a -> s{_rReplicationTaskSettings = a})

-- | The status of the replication task.
rStatus :: Lens' ReplicationTask (Maybe Text)
rStatus = lens _rStatus (\ s a -> s{_rStatus = a})

-- | The reason the replication task was stopped.
rStopReason :: Lens' ReplicationTask (Maybe Text)
rStopReason = lens _rStopReason (\ s a -> s{_rStopReason = a})

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rTargetEndpointARN :: Lens' ReplicationTask (Maybe Text)
rTargetEndpointARN = lens _rTargetEndpointARN (\ s a -> s{_rTargetEndpointARN = a})

-- | The user-assigned replication task identifier or name. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
rReplicationTaskIdentifier :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskIdentifier = lens _rReplicationTaskIdentifier (\ s a -> s{_rReplicationTaskIdentifier = a})

-- | Indicates when you want a change data capture (CDC) operation to start. Use either @CdcStartPosition@ or @CdcStartTime@ to specify when you want the CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position “2018-03-08T12:12:12” Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93" LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
rCdcStartPosition :: Lens' ReplicationTask (Maybe Text)
rCdcStartPosition = lens _rCdcStartPosition (\ s a -> s{_rCdcStartPosition = a})

-- | The date the replication task is scheduled to start.
rReplicationTaskStartDate :: Lens' ReplicationTask (Maybe UTCTime)
rReplicationTaskStartDate = lens _rReplicationTaskStartDate (\ s a -> s{_rReplicationTaskStartDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
rSourceEndpointARN :: Lens' ReplicationTask (Maybe Text)
rSourceEndpointARN = lens _rSourceEndpointARN (\ s a -> s{_rSourceEndpointARN = a})

-- | Indicates the last checkpoint that occurred during a change data capture (CDC) operation. You can provide this value to the @CdcStartPosition@ parameter to start a CDC operation that begins at that checkpoint.
rRecoveryCheckpoint :: Lens' ReplicationTask (Maybe Text)
rRecoveryCheckpoint = lens _rRecoveryCheckpoint (\ s a -> s{_rRecoveryCheckpoint = a})

-- | Table mappings specified in the task.
rTableMappings :: Lens' ReplicationTask (Maybe Text)
rTableMappings = lens _rTableMappings (\ s a -> s{_rTableMappings = a})

-- | The date the replication task was created.
rReplicationTaskCreationDate :: Lens' ReplicationTask (Maybe UTCTime)
rReplicationTaskCreationDate = lens _rReplicationTaskCreationDate (\ s a -> s{_rReplicationTaskCreationDate = a}) . mapping _Time

-- | The type of migration.
rMigrationType :: Lens' ReplicationTask (Maybe MigrationTypeValue)
rMigrationType = lens _rMigrationType (\ s a -> s{_rMigrationType = a})

-- | The Amazon Resource Name (ARN) of the replication task.
rReplicationTaskARN :: Lens' ReplicationTask (Maybe Text)
rReplicationTaskARN = lens _rReplicationTaskARN (\ s a -> s{_rReplicationTaskARN = a})

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration User Guide./ 
rTaskData :: Lens' ReplicationTask (Maybe Text)
rTaskData = lens _rTaskData (\ s a -> s{_rTaskData = a})

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time. Server time example: --cdc-stop-position “server_time:3018-02-09T12:12:12” Commit time example: --cdc-stop-position “commit_time: 3018-02-09T12:12:12 “
rCdcStopPosition :: Lens' ReplicationTask (Maybe Text)
rCdcStopPosition = lens _rCdcStopPosition (\ s a -> s{_rCdcStopPosition = a})

-- | The statistics for the task, including elapsed time, tables loaded, and table errors.
rReplicationTaskStats :: Lens' ReplicationTask (Maybe ReplicationTaskStats)
rReplicationTaskStats = lens _rReplicationTaskStats (\ s a -> s{_rReplicationTaskStats = a})

-- | The Amazon Resource Name (ARN) of the replication instance.
rReplicationInstanceARN :: Lens' ReplicationTask (Maybe Text)
rReplicationInstanceARN = lens _rReplicationInstanceARN (\ s a -> s{_rReplicationInstanceARN = a})

-- | The last error (failure) message generated for the replication instance.
rLastFailureMessage :: Lens' ReplicationTask (Maybe Text)
rLastFailureMessage = lens _rLastFailureMessage (\ s a -> s{_rLastFailureMessage = a})

instance FromJSON ReplicationTask where
        parseJSON
          = withObject "ReplicationTask"
              (\ x ->
                 ReplicationTask' <$>
                   (x .:? "ReplicationTaskSettings") <*>
                     (x .:? "Status")
                     <*> (x .:? "StopReason")
                     <*> (x .:? "TargetEndpointArn")
                     <*> (x .:? "ReplicationTaskIdentifier")
                     <*> (x .:? "CdcStartPosition")
                     <*> (x .:? "ReplicationTaskStartDate")
                     <*> (x .:? "SourceEndpointArn")
                     <*> (x .:? "RecoveryCheckpoint")
                     <*> (x .:? "TableMappings")
                     <*> (x .:? "ReplicationTaskCreationDate")
                     <*> (x .:? "MigrationType")
                     <*> (x .:? "ReplicationTaskArn")
                     <*> (x .:? "TaskData")
                     <*> (x .:? "CdcStopPosition")
                     <*> (x .:? "ReplicationTaskStats")
                     <*> (x .:? "ReplicationInstanceArn")
                     <*> (x .:? "LastFailureMessage"))

instance Hashable ReplicationTask where

instance NFData ReplicationTask where
