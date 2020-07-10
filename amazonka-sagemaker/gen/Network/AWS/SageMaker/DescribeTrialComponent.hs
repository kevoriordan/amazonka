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
-- Module      : Network.AWS.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component's properties.
--
--
module Network.AWS.SageMaker.DescribeTrialComponent
    (
    -- * Creating a Request
      describeTrialComponent
    , DescribeTrialComponent
    -- * Request Lenses
    , desTrialComponentName

    -- * Destructuring the Response
    , describeTrialComponentResponse
    , DescribeTrialComponentResponse
    -- * Response Lenses
    , dscrbtrlcmpnntrsCreationTime
    , dscrbtrlcmpnntrsStatus
    , dscrbtrlcmpnntrsMetrics
    , dscrbtrlcmpnntrsOutputArtifacts
    , dscrbtrlcmpnntrsStartTime
    , dscrbtrlcmpnntrsCreatedBy
    , dscrbtrlcmpnntrsLastModifiedTime
    , dscrbtrlcmpnntrsEndTime
    , dscrbtrlcmpnntrsTrialComponentName
    , dscrbtrlcmpnntrsParameters
    , dscrbtrlcmpnntrsSource
    , dscrbtrlcmpnntrsDisplayName
    , dscrbtrlcmpnntrsLastModifiedBy
    , dscrbtrlcmpnntrsTrialComponentARN
    , dscrbtrlcmpnntrsInputArtifacts
    , dscrbtrlcmpnntrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeTrialComponent' smart constructor.
newtype DescribeTrialComponent = DescribeTrialComponent'{_desTrialComponentName
                                                         :: Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeTrialComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desTrialComponentName' - The name of the trial component to describe.
describeTrialComponent
    :: Text -- ^ 'desTrialComponentName'
    -> DescribeTrialComponent
describeTrialComponent pTrialComponentName_
  = DescribeTrialComponent'{_desTrialComponentName =
                              pTrialComponentName_}

-- | The name of the trial component to describe.
desTrialComponentName :: Lens' DescribeTrialComponent Text
desTrialComponentName = lens _desTrialComponentName (\ s a -> s{_desTrialComponentName = a})

instance AWSRequest DescribeTrialComponent where
        type Rs DescribeTrialComponent =
             DescribeTrialComponentResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrialComponentResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "Status") <*>
                     (x .?> "Metrics" .!@ mempty)
                     <*> (x .?> "OutputArtifacts" .!@ mempty)
                     <*> (x .?> "StartTime")
                     <*> (x .?> "CreatedBy")
                     <*> (x .?> "LastModifiedTime")
                     <*> (x .?> "EndTime")
                     <*> (x .?> "TrialComponentName")
                     <*> (x .?> "Parameters" .!@ mempty)
                     <*> (x .?> "Source")
                     <*> (x .?> "DisplayName")
                     <*> (x .?> "LastModifiedBy")
                     <*> (x .?> "TrialComponentArn")
                     <*> (x .?> "InputArtifacts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTrialComponent where

instance NFData DescribeTrialComponent where

instance ToHeaders DescribeTrialComponent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeTrialComponent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrialComponent where
        toJSON DescribeTrialComponent'{..}
          = object
              (catMaybes
                 [Just
                    ("TrialComponentName" .= _desTrialComponentName)])

instance ToPath DescribeTrialComponent where
        toPath = const "/"

instance ToQuery DescribeTrialComponent where
        toQuery = const mempty

-- | /See:/ 'describeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'{_dscrbtrlcmpnntrsCreationTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX),
                                                                      _dscrbtrlcmpnntrsStatus
                                                                      ::
                                                                      !(Maybe
                                                                          TrialComponentStatus),
                                                                      _dscrbtrlcmpnntrsMetrics
                                                                      ::
                                                                      !(Maybe
                                                                          [TrialComponentMetricSummary]),
                                                                      _dscrbtrlcmpnntrsOutputArtifacts
                                                                      ::
                                                                      !(Maybe
                                                                          (Map
                                                                             Text
                                                                             TrialComponentArtifact)),
                                                                      _dscrbtrlcmpnntrsStartTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX),
                                                                      _dscrbtrlcmpnntrsCreatedBy
                                                                      ::
                                                                      !(Maybe
                                                                          UserContext),
                                                                      _dscrbtrlcmpnntrsLastModifiedTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX),
                                                                      _dscrbtrlcmpnntrsEndTime
                                                                      ::
                                                                      !(Maybe
                                                                          POSIX),
                                                                      _dscrbtrlcmpnntrsTrialComponentName
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dscrbtrlcmpnntrsParameters
                                                                      ::
                                                                      !(Maybe
                                                                          (Map
                                                                             Text
                                                                             TrialComponentParameterValue)),
                                                                      _dscrbtrlcmpnntrsSource
                                                                      ::
                                                                      !(Maybe
                                                                          TrialComponentSource),
                                                                      _dscrbtrlcmpnntrsDisplayName
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dscrbtrlcmpnntrsLastModifiedBy
                                                                      ::
                                                                      !(Maybe
                                                                          UserContext),
                                                                      _dscrbtrlcmpnntrsTrialComponentARN
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dscrbtrlcmpnntrsInputArtifacts
                                                                      ::
                                                                      !(Maybe
                                                                          (Map
                                                                             Text
                                                                             TrialComponentArtifact)),
                                                                      _dscrbtrlcmpnntrsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'DescribeTrialComponentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbtrlcmpnntrsCreationTime' - When the component was created.
--
-- * 'dscrbtrlcmpnntrsStatus' - The status of the component. States include:     * InProgress     * Completed     * Failed
--
-- * 'dscrbtrlcmpnntrsMetrics' - The metrics for the component.
--
-- * 'dscrbtrlcmpnntrsOutputArtifacts' - The output artifacts of the component.
--
-- * 'dscrbtrlcmpnntrsStartTime' - When the component started.
--
-- * 'dscrbtrlcmpnntrsCreatedBy' - Who created the component.
--
-- * 'dscrbtrlcmpnntrsLastModifiedTime' - When the component was last modified.
--
-- * 'dscrbtrlcmpnntrsEndTime' - When the component ended.
--
-- * 'dscrbtrlcmpnntrsTrialComponentName' - The name of the trial component.
--
-- * 'dscrbtrlcmpnntrsParameters' - The hyperparameters of the component.
--
-- * 'dscrbtrlcmpnntrsSource' - The Amazon Resource Name (ARN) of the source and, optionally, the job type.
--
-- * 'dscrbtrlcmpnntrsDisplayName' - The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
--
-- * 'dscrbtrlcmpnntrsLastModifiedBy' - Who last modified the component.
--
-- * 'dscrbtrlcmpnntrsTrialComponentARN' - The Amazon Resource Name (ARN) of the trial component.
--
-- * 'dscrbtrlcmpnntrsInputArtifacts' - The input artifacts of the component.
--
-- * 'dscrbtrlcmpnntrsResponseStatus' - -- | The response status code.
describeTrialComponentResponse
    :: Int -- ^ 'dscrbtrlcmpnntrsResponseStatus'
    -> DescribeTrialComponentResponse
describeTrialComponentResponse pResponseStatus_
  = DescribeTrialComponentResponse'{_dscrbtrlcmpnntrsCreationTime
                                      = Nothing,
                                    _dscrbtrlcmpnntrsStatus = Nothing,
                                    _dscrbtrlcmpnntrsMetrics = Nothing,
                                    _dscrbtrlcmpnntrsOutputArtifacts = Nothing,
                                    _dscrbtrlcmpnntrsStartTime = Nothing,
                                    _dscrbtrlcmpnntrsCreatedBy = Nothing,
                                    _dscrbtrlcmpnntrsLastModifiedTime = Nothing,
                                    _dscrbtrlcmpnntrsEndTime = Nothing,
                                    _dscrbtrlcmpnntrsTrialComponentName =
                                      Nothing,
                                    _dscrbtrlcmpnntrsParameters = Nothing,
                                    _dscrbtrlcmpnntrsSource = Nothing,
                                    _dscrbtrlcmpnntrsDisplayName = Nothing,
                                    _dscrbtrlcmpnntrsLastModifiedBy = Nothing,
                                    _dscrbtrlcmpnntrsTrialComponentARN =
                                      Nothing,
                                    _dscrbtrlcmpnntrsInputArtifacts = Nothing,
                                    _dscrbtrlcmpnntrsResponseStatus =
                                      pResponseStatus_}

-- | When the component was created.
dscrbtrlcmpnntrsCreationTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
dscrbtrlcmpnntrsCreationTime = lens _dscrbtrlcmpnntrsCreationTime (\ s a -> s{_dscrbtrlcmpnntrsCreationTime = a}) . mapping _Time

-- | The status of the component. States include:     * InProgress     * Completed     * Failed
dscrbtrlcmpnntrsStatus :: Lens' DescribeTrialComponentResponse (Maybe TrialComponentStatus)
dscrbtrlcmpnntrsStatus = lens _dscrbtrlcmpnntrsStatus (\ s a -> s{_dscrbtrlcmpnntrsStatus = a})

-- | The metrics for the component.
dscrbtrlcmpnntrsMetrics :: Lens' DescribeTrialComponentResponse [TrialComponentMetricSummary]
dscrbtrlcmpnntrsMetrics = lens _dscrbtrlcmpnntrsMetrics (\ s a -> s{_dscrbtrlcmpnntrsMetrics = a}) . _Default . _Coerce

-- | The output artifacts of the component.
dscrbtrlcmpnntrsOutputArtifacts :: Lens' DescribeTrialComponentResponse (HashMap Text TrialComponentArtifact)
dscrbtrlcmpnntrsOutputArtifacts = lens _dscrbtrlcmpnntrsOutputArtifacts (\ s a -> s{_dscrbtrlcmpnntrsOutputArtifacts = a}) . _Default . _Map

-- | When the component started.
dscrbtrlcmpnntrsStartTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
dscrbtrlcmpnntrsStartTime = lens _dscrbtrlcmpnntrsStartTime (\ s a -> s{_dscrbtrlcmpnntrsStartTime = a}) . mapping _Time

-- | Who created the component.
dscrbtrlcmpnntrsCreatedBy :: Lens' DescribeTrialComponentResponse (Maybe UserContext)
dscrbtrlcmpnntrsCreatedBy = lens _dscrbtrlcmpnntrsCreatedBy (\ s a -> s{_dscrbtrlcmpnntrsCreatedBy = a})

-- | When the component was last modified.
dscrbtrlcmpnntrsLastModifiedTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
dscrbtrlcmpnntrsLastModifiedTime = lens _dscrbtrlcmpnntrsLastModifiedTime (\ s a -> s{_dscrbtrlcmpnntrsLastModifiedTime = a}) . mapping _Time

-- | When the component ended.
dscrbtrlcmpnntrsEndTime :: Lens' DescribeTrialComponentResponse (Maybe UTCTime)
dscrbtrlcmpnntrsEndTime = lens _dscrbtrlcmpnntrsEndTime (\ s a -> s{_dscrbtrlcmpnntrsEndTime = a}) . mapping _Time

-- | The name of the trial component.
dscrbtrlcmpnntrsTrialComponentName :: Lens' DescribeTrialComponentResponse (Maybe Text)
dscrbtrlcmpnntrsTrialComponentName = lens _dscrbtrlcmpnntrsTrialComponentName (\ s a -> s{_dscrbtrlcmpnntrsTrialComponentName = a})

-- | The hyperparameters of the component.
dscrbtrlcmpnntrsParameters :: Lens' DescribeTrialComponentResponse (HashMap Text TrialComponentParameterValue)
dscrbtrlcmpnntrsParameters = lens _dscrbtrlcmpnntrsParameters (\ s a -> s{_dscrbtrlcmpnntrsParameters = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job type.
dscrbtrlcmpnntrsSource :: Lens' DescribeTrialComponentResponse (Maybe TrialComponentSource)
dscrbtrlcmpnntrsSource = lens _dscrbtrlcmpnntrsSource (\ s a -> s{_dscrbtrlcmpnntrsSource = a})

-- | The name of the component as displayed. If @DisplayName@ isn't specified, @TrialComponentName@ is displayed.
dscrbtrlcmpnntrsDisplayName :: Lens' DescribeTrialComponentResponse (Maybe Text)
dscrbtrlcmpnntrsDisplayName = lens _dscrbtrlcmpnntrsDisplayName (\ s a -> s{_dscrbtrlcmpnntrsDisplayName = a})

-- | Who last modified the component.
dscrbtrlcmpnntrsLastModifiedBy :: Lens' DescribeTrialComponentResponse (Maybe UserContext)
dscrbtrlcmpnntrsLastModifiedBy = lens _dscrbtrlcmpnntrsLastModifiedBy (\ s a -> s{_dscrbtrlcmpnntrsLastModifiedBy = a})

-- | The Amazon Resource Name (ARN) of the trial component.
dscrbtrlcmpnntrsTrialComponentARN :: Lens' DescribeTrialComponentResponse (Maybe Text)
dscrbtrlcmpnntrsTrialComponentARN = lens _dscrbtrlcmpnntrsTrialComponentARN (\ s a -> s{_dscrbtrlcmpnntrsTrialComponentARN = a})

-- | The input artifacts of the component.
dscrbtrlcmpnntrsInputArtifacts :: Lens' DescribeTrialComponentResponse (HashMap Text TrialComponentArtifact)
dscrbtrlcmpnntrsInputArtifacts = lens _dscrbtrlcmpnntrsInputArtifacts (\ s a -> s{_dscrbtrlcmpnntrsInputArtifacts = a}) . _Default . _Map

-- | -- | The response status code.
dscrbtrlcmpnntrsResponseStatus :: Lens' DescribeTrialComponentResponse Int
dscrbtrlcmpnntrsResponseStatus = lens _dscrbtrlcmpnntrsResponseStatus (\ s a -> s{_dscrbtrlcmpnntrsResponseStatus = a})

instance NFData DescribeTrialComponentResponse where
