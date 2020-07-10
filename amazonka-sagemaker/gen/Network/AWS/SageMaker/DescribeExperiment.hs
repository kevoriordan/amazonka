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
-- Module      : Network.AWS.SageMaker.DescribeExperiment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of an experiment's properties.
--
--
module Network.AWS.SageMaker.DescribeExperiment
    (
    -- * Creating a Request
      describeExperiment
    , DescribeExperiment
    -- * Request Lenses
    , deExperimentName

    -- * Destructuring the Response
    , describeExperimentResponse
    , DescribeExperimentResponse
    -- * Response Lenses
    , dscrbexprmntrsCreationTime
    , dscrbexprmntrsCreatedBy
    , dscrbexprmntrsLastModifiedTime
    , dscrbexprmntrsExperimentName
    , dscrbexprmntrsExperimentARN
    , dscrbexprmntrsSource
    , dscrbexprmntrsDisplayName
    , dscrbexprmntrsLastModifiedBy
    , dscrbexprmntrsDescription
    , dscrbexprmntrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeExperiment' smart constructor.
newtype DescribeExperiment = DescribeExperiment'{_deExperimentName
                                                 :: Text}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DescribeExperiment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deExperimentName' - The name of the experiment to describe.
describeExperiment
    :: Text -- ^ 'deExperimentName'
    -> DescribeExperiment
describeExperiment pExperimentName_
  = DescribeExperiment'{_deExperimentName =
                          pExperimentName_}

-- | The name of the experiment to describe.
deExperimentName :: Lens' DescribeExperiment Text
deExperimentName = lens _deExperimentName (\ s a -> s{_deExperimentName = a})

instance AWSRequest DescribeExperiment where
        type Rs DescribeExperiment =
             DescribeExperimentResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeExperimentResponse' <$>
                   (x .?> "CreationTime") <*> (x .?> "CreatedBy") <*>
                     (x .?> "LastModifiedTime")
                     <*> (x .?> "ExperimentName")
                     <*> (x .?> "ExperimentArn")
                     <*> (x .?> "Source")
                     <*> (x .?> "DisplayName")
                     <*> (x .?> "LastModifiedBy")
                     <*> (x .?> "Description")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeExperiment where

instance NFData DescribeExperiment where

instance ToHeaders DescribeExperiment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeExperiment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeExperiment where
        toJSON DescribeExperiment'{..}
          = object
              (catMaybes
                 [Just ("ExperimentName" .= _deExperimentName)])

instance ToPath DescribeExperiment where
        toPath = const "/"

instance ToQuery DescribeExperiment where
        toQuery = const mempty

-- | /See:/ 'describeExperimentResponse' smart constructor.
data DescribeExperimentResponse = DescribeExperimentResponse'{_dscrbexprmntrsCreationTime
                                                              :: !(Maybe POSIX),
                                                              _dscrbexprmntrsCreatedBy
                                                              ::
                                                              !(Maybe
                                                                  UserContext),
                                                              _dscrbexprmntrsLastModifiedTime
                                                              :: !(Maybe POSIX),
                                                              _dscrbexprmntrsExperimentName
                                                              :: !(Maybe Text),
                                                              _dscrbexprmntrsExperimentARN
                                                              :: !(Maybe Text),
                                                              _dscrbexprmntrsSource
                                                              ::
                                                              !(Maybe
                                                                  ExperimentSource),
                                                              _dscrbexprmntrsDisplayName
                                                              :: !(Maybe Text),
                                                              _dscrbexprmntrsLastModifiedBy
                                                              ::
                                                              !(Maybe
                                                                  UserContext),
                                                              _dscrbexprmntrsDescription
                                                              :: !(Maybe Text),
                                                              _dscrbexprmntrsResponseStatus
                                                              :: !Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeExperimentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbexprmntrsCreationTime' - When the experiment was created.
--
-- * 'dscrbexprmntrsCreatedBy' - Who created the experiment.
--
-- * 'dscrbexprmntrsLastModifiedTime' - When the experiment was last modified.
--
-- * 'dscrbexprmntrsExperimentName' - The name of the experiment.
--
-- * 'dscrbexprmntrsExperimentARN' - The Amazon Resource Name (ARN) of the experiment.
--
-- * 'dscrbexprmntrsSource' - The ARN of the source and, optionally, the type.
--
-- * 'dscrbexprmntrsDisplayName' - The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
--
-- * 'dscrbexprmntrsLastModifiedBy' - Who last modified the experiment.
--
-- * 'dscrbexprmntrsDescription' - The description of the experiment.
--
-- * 'dscrbexprmntrsResponseStatus' - -- | The response status code.
describeExperimentResponse
    :: Int -- ^ 'dscrbexprmntrsResponseStatus'
    -> DescribeExperimentResponse
describeExperimentResponse pResponseStatus_
  = DescribeExperimentResponse'{_dscrbexprmntrsCreationTime
                                  = Nothing,
                                _dscrbexprmntrsCreatedBy = Nothing,
                                _dscrbexprmntrsLastModifiedTime = Nothing,
                                _dscrbexprmntrsExperimentName = Nothing,
                                _dscrbexprmntrsExperimentARN = Nothing,
                                _dscrbexprmntrsSource = Nothing,
                                _dscrbexprmntrsDisplayName = Nothing,
                                _dscrbexprmntrsLastModifiedBy = Nothing,
                                _dscrbexprmntrsDescription = Nothing,
                                _dscrbexprmntrsResponseStatus =
                                  pResponseStatus_}

-- | When the experiment was created.
dscrbexprmntrsCreationTime :: Lens' DescribeExperimentResponse (Maybe UTCTime)
dscrbexprmntrsCreationTime = lens _dscrbexprmntrsCreationTime (\ s a -> s{_dscrbexprmntrsCreationTime = a}) . mapping _Time

-- | Who created the experiment.
dscrbexprmntrsCreatedBy :: Lens' DescribeExperimentResponse (Maybe UserContext)
dscrbexprmntrsCreatedBy = lens _dscrbexprmntrsCreatedBy (\ s a -> s{_dscrbexprmntrsCreatedBy = a})

-- | When the experiment was last modified.
dscrbexprmntrsLastModifiedTime :: Lens' DescribeExperimentResponse (Maybe UTCTime)
dscrbexprmntrsLastModifiedTime = lens _dscrbexprmntrsLastModifiedTime (\ s a -> s{_dscrbexprmntrsLastModifiedTime = a}) . mapping _Time

-- | The name of the experiment.
dscrbexprmntrsExperimentName :: Lens' DescribeExperimentResponse (Maybe Text)
dscrbexprmntrsExperimentName = lens _dscrbexprmntrsExperimentName (\ s a -> s{_dscrbexprmntrsExperimentName = a})

-- | The Amazon Resource Name (ARN) of the experiment.
dscrbexprmntrsExperimentARN :: Lens' DescribeExperimentResponse (Maybe Text)
dscrbexprmntrsExperimentARN = lens _dscrbexprmntrsExperimentARN (\ s a -> s{_dscrbexprmntrsExperimentARN = a})

-- | The ARN of the source and, optionally, the type.
dscrbexprmntrsSource :: Lens' DescribeExperimentResponse (Maybe ExperimentSource)
dscrbexprmntrsSource = lens _dscrbexprmntrsSource (\ s a -> s{_dscrbexprmntrsSource = a})

-- | The name of the experiment as displayed. If @DisplayName@ isn't specified, @ExperimentName@ is displayed.
dscrbexprmntrsDisplayName :: Lens' DescribeExperimentResponse (Maybe Text)
dscrbexprmntrsDisplayName = lens _dscrbexprmntrsDisplayName (\ s a -> s{_dscrbexprmntrsDisplayName = a})

-- | Who last modified the experiment.
dscrbexprmntrsLastModifiedBy :: Lens' DescribeExperimentResponse (Maybe UserContext)
dscrbexprmntrsLastModifiedBy = lens _dscrbexprmntrsLastModifiedBy (\ s a -> s{_dscrbexprmntrsLastModifiedBy = a})

-- | The description of the experiment.
dscrbexprmntrsDescription :: Lens' DescribeExperimentResponse (Maybe Text)
dscrbexprmntrsDescription = lens _dscrbexprmntrsDescription (\ s a -> s{_dscrbexprmntrsDescription = a})

-- | -- | The response status code.
dscrbexprmntrsResponseStatus :: Lens' DescribeExperimentResponse Int
dscrbexprmntrsResponseStatus = lens _dscrbexprmntrsResponseStatus (\ s a -> s{_dscrbexprmntrsResponseStatus = a})

instance NFData DescribeExperimentResponse where
