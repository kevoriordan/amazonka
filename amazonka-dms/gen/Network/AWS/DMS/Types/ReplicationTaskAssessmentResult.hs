{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The task assessment report in JSON format. 
--
--
--
-- /See:/ 'replicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'{_rtarAssessmentResults
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _rtarAssessmentResultsFile
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _rtarReplicationTaskIdentifier
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _rtarAssessmentStatus
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _rtarS3ObjectURL
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _rtarReplicationTaskLastAssessmentDate
                                                                        ::
                                                                        !(Maybe
                                                                            POSIX),
                                                                        _rtarReplicationTaskARN
                                                                        ::
                                                                        !(Maybe
                                                                            Text)}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'ReplicationTaskAssessmentResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtarAssessmentResults' - The task assessment results in JSON format. 
--
-- * 'rtarAssessmentResultsFile' - The file containing the results of the task assessment. 
--
-- * 'rtarReplicationTaskIdentifier' - The replication task identifier of the task on which the task assessment was run. 
--
-- * 'rtarAssessmentStatus' - The status of the task assessment. 
--
-- * 'rtarS3ObjectURL' - The URL of the S3 object containing the task assessment results. 
--
-- * 'rtarReplicationTaskLastAssessmentDate' - The date the task assessment was completed. 
--
-- * 'rtarReplicationTaskARN' - The Amazon Resource Name (ARN) of the replication task. 
replicationTaskAssessmentResult
    :: ReplicationTaskAssessmentResult
replicationTaskAssessmentResult
  = ReplicationTaskAssessmentResult'{_rtarAssessmentResults
                                       = Nothing,
                                     _rtarAssessmentResultsFile = Nothing,
                                     _rtarReplicationTaskIdentifier = Nothing,
                                     _rtarAssessmentStatus = Nothing,
                                     _rtarS3ObjectURL = Nothing,
                                     _rtarReplicationTaskLastAssessmentDate =
                                       Nothing,
                                     _rtarReplicationTaskARN = Nothing}

-- | The task assessment results in JSON format. 
rtarAssessmentResults :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentResults = lens _rtarAssessmentResults (\ s a -> s{_rtarAssessmentResults = a})

-- | The file containing the results of the task assessment. 
rtarAssessmentResultsFile :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentResultsFile = lens _rtarAssessmentResultsFile (\ s a -> s{_rtarAssessmentResultsFile = a})

-- | The replication task identifier of the task on which the task assessment was run. 
rtarReplicationTaskIdentifier :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarReplicationTaskIdentifier = lens _rtarReplicationTaskIdentifier (\ s a -> s{_rtarReplicationTaskIdentifier = a})

-- | The status of the task assessment. 
rtarAssessmentStatus :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarAssessmentStatus = lens _rtarAssessmentStatus (\ s a -> s{_rtarAssessmentStatus = a})

-- | The URL of the S3 object containing the task assessment results. 
rtarS3ObjectURL :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarS3ObjectURL = lens _rtarS3ObjectURL (\ s a -> s{_rtarS3ObjectURL = a})

-- | The date the task assessment was completed. 
rtarReplicationTaskLastAssessmentDate :: Lens' ReplicationTaskAssessmentResult (Maybe UTCTime)
rtarReplicationTaskLastAssessmentDate = lens _rtarReplicationTaskLastAssessmentDate (\ s a -> s{_rtarReplicationTaskLastAssessmentDate = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the replication task. 
rtarReplicationTaskARN :: Lens' ReplicationTaskAssessmentResult (Maybe Text)
rtarReplicationTaskARN = lens _rtarReplicationTaskARN (\ s a -> s{_rtarReplicationTaskARN = a})

instance FromJSON ReplicationTaskAssessmentResult
         where
        parseJSON
          = withObject "ReplicationTaskAssessmentResult"
              (\ x ->
                 ReplicationTaskAssessmentResult' <$>
                   (x .:? "AssessmentResults") <*>
                     (x .:? "AssessmentResultsFile")
                     <*> (x .:? "ReplicationTaskIdentifier")
                     <*> (x .:? "AssessmentStatus")
                     <*> (x .:? "S3ObjectUrl")
                     <*> (x .:? "ReplicationTaskLastAssessmentDate")
                     <*> (x .:? "ReplicationTaskArn"))

instance Hashable ReplicationTaskAssessmentResult
         where

instance NFData ReplicationTaskAssessmentResult where
