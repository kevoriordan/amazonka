{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.AppSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.AppLaunchStatus
import Network.AWS.SMS.Types.AppReplicationStatus
import Network.AWS.SMS.Types.AppStatus
import Network.AWS.SMS.Types.LaunchDetails

-- | Information about the application.
--
--
--
-- /See:/ 'appSummary' smart constructor.
data AppSummary = AppSummary'{_asCreationTime ::
                              !(Maybe POSIX),
                              _asTotalServers :: !(Maybe Int),
                              _asStatus :: !(Maybe AppStatus),
                              _asLaunchDetails :: !(Maybe LaunchDetails),
                              _asLaunchStatusMessage :: !(Maybe Text),
                              _asReplicationStatusMessage :: !(Maybe Text),
                              _asTotalServerGroups :: !(Maybe Int),
                              _asRoleName :: !(Maybe Text),
                              _asLaunchStatus :: !(Maybe AppLaunchStatus),
                              _asAppId :: !(Maybe Text),
                              _asName :: !(Maybe Text),
                              _asStatusMessage :: !(Maybe Text),
                              _asLatestReplicationTime :: !(Maybe POSIX),
                              _asReplicationStatus ::
                              !(Maybe AppReplicationStatus),
                              _asLastModified :: !(Maybe POSIX),
                              _asDescription :: !(Maybe Text)}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asCreationTime' - Time of creation of this application.
--
-- * 'asTotalServers' - Number of servers present in the application.
--
-- * 'asStatus' - Status of the application.
--
-- * 'asLaunchDetails' - Details about the latest launch of the application.
--
-- * 'asLaunchStatusMessage' - A message related to the launch status of the application.
--
-- * 'asReplicationStatusMessage' - A message related to the replication status of the application.
--
-- * 'asTotalServerGroups' - Number of server groups present in the application.
--
-- * 'asRoleName' - Name of the service role in the customer's account used by AWS SMS.
--
-- * 'asLaunchStatus' - Launch status of the application.
--
-- * 'asAppId' - Unique ID of the application.
--
-- * 'asName' - Name of the application.
--
-- * 'asStatusMessage' - A message related to the status of the application
--
-- * 'asLatestReplicationTime' - Timestamp of the application's most recent successful replication.
--
-- * 'asReplicationStatus' - Replication status of the application.
--
-- * 'asLastModified' - Timestamp of the application's creation.
--
-- * 'asDescription' - Description of the application.
appSummary
    :: AppSummary
appSummary
  = AppSummary'{_asCreationTime = Nothing,
                _asTotalServers = Nothing, _asStatus = Nothing,
                _asLaunchDetails = Nothing,
                _asLaunchStatusMessage = Nothing,
                _asReplicationStatusMessage = Nothing,
                _asTotalServerGroups = Nothing,
                _asRoleName = Nothing, _asLaunchStatus = Nothing,
                _asAppId = Nothing, _asName = Nothing,
                _asStatusMessage = Nothing,
                _asLatestReplicationTime = Nothing,
                _asReplicationStatus = Nothing,
                _asLastModified = Nothing, _asDescription = Nothing}

-- | Time of creation of this application.
asCreationTime :: Lens' AppSummary (Maybe UTCTime)
asCreationTime = lens _asCreationTime (\ s a -> s{_asCreationTime = a}) . mapping _Time

-- | Number of servers present in the application.
asTotalServers :: Lens' AppSummary (Maybe Int)
asTotalServers = lens _asTotalServers (\ s a -> s{_asTotalServers = a})

-- | Status of the application.
asStatus :: Lens' AppSummary (Maybe AppStatus)
asStatus = lens _asStatus (\ s a -> s{_asStatus = a})

-- | Details about the latest launch of the application.
asLaunchDetails :: Lens' AppSummary (Maybe LaunchDetails)
asLaunchDetails = lens _asLaunchDetails (\ s a -> s{_asLaunchDetails = a})

-- | A message related to the launch status of the application.
asLaunchStatusMessage :: Lens' AppSummary (Maybe Text)
asLaunchStatusMessage = lens _asLaunchStatusMessage (\ s a -> s{_asLaunchStatusMessage = a})

-- | A message related to the replication status of the application.
asReplicationStatusMessage :: Lens' AppSummary (Maybe Text)
asReplicationStatusMessage = lens _asReplicationStatusMessage (\ s a -> s{_asReplicationStatusMessage = a})

-- | Number of server groups present in the application.
asTotalServerGroups :: Lens' AppSummary (Maybe Int)
asTotalServerGroups = lens _asTotalServerGroups (\ s a -> s{_asTotalServerGroups = a})

-- | Name of the service role in the customer's account used by AWS SMS.
asRoleName :: Lens' AppSummary (Maybe Text)
asRoleName = lens _asRoleName (\ s a -> s{_asRoleName = a})

-- | Launch status of the application.
asLaunchStatus :: Lens' AppSummary (Maybe AppLaunchStatus)
asLaunchStatus = lens _asLaunchStatus (\ s a -> s{_asLaunchStatus = a})

-- | Unique ID of the application.
asAppId :: Lens' AppSummary (Maybe Text)
asAppId = lens _asAppId (\ s a -> s{_asAppId = a})

-- | Name of the application.
asName :: Lens' AppSummary (Maybe Text)
asName = lens _asName (\ s a -> s{_asName = a})

-- | A message related to the status of the application
asStatusMessage :: Lens' AppSummary (Maybe Text)
asStatusMessage = lens _asStatusMessage (\ s a -> s{_asStatusMessage = a})

-- | Timestamp of the application's most recent successful replication.
asLatestReplicationTime :: Lens' AppSummary (Maybe UTCTime)
asLatestReplicationTime = lens _asLatestReplicationTime (\ s a -> s{_asLatestReplicationTime = a}) . mapping _Time

-- | Replication status of the application.
asReplicationStatus :: Lens' AppSummary (Maybe AppReplicationStatus)
asReplicationStatus = lens _asReplicationStatus (\ s a -> s{_asReplicationStatus = a})

-- | Timestamp of the application's creation.
asLastModified :: Lens' AppSummary (Maybe UTCTime)
asLastModified = lens _asLastModified (\ s a -> s{_asLastModified = a}) . mapping _Time

-- | Description of the application.
asDescription :: Lens' AppSummary (Maybe Text)
asDescription = lens _asDescription (\ s a -> s{_asDescription = a})

instance FromJSON AppSummary where
        parseJSON
          = withObject "AppSummary"
              (\ x ->
                 AppSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "totalServers") <*>
                     (x .:? "status")
                     <*> (x .:? "launchDetails")
                     <*> (x .:? "launchStatusMessage")
                     <*> (x .:? "replicationStatusMessage")
                     <*> (x .:? "totalServerGroups")
                     <*> (x .:? "roleName")
                     <*> (x .:? "launchStatus")
                     <*> (x .:? "appId")
                     <*> (x .:? "name")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "latestReplicationTime")
                     <*> (x .:? "replicationStatus")
                     <*> (x .:? "lastModified")
                     <*> (x .:? "description"))

instance Hashable AppSummary where

instance NFData AppSummary where
