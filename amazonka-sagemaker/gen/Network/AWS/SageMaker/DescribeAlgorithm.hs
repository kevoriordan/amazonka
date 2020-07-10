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
-- Module      : Network.AWS.SageMaker.DescribeAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the specified algorithm that is in your account.
--
--
module Network.AWS.SageMaker.DescribeAlgorithm
    (
    -- * Creating a Request
      describeAlgorithm
    , DescribeAlgorithm
    -- * Request Lenses
    , dAlgorithmName

    -- * Destructuring the Response
    , describeAlgorithmResponse
    , DescribeAlgorithmResponse
    -- * Response Lenses
    , dscrbalgrthmrsValidationSpecification
    , dscrbalgrthmrsInferenceSpecification
    , dscrbalgrthmrsAlgorithmDescription
    , dscrbalgrthmrsCertifyForMarketplace
    , dscrbalgrthmrsProductId
    , dscrbalgrthmrsResponseStatus
    , dscrbalgrthmrsAlgorithmName
    , dscrbalgrthmrsAlgorithmARN
    , dscrbalgrthmrsCreationTime
    , dscrbalgrthmrsTrainingSpecification
    , dscrbalgrthmrsAlgorithmStatus
    , dscrbalgrthmrsAlgorithmStatusDetails
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeAlgorithm' smart constructor.
newtype DescribeAlgorithm = DescribeAlgorithm'{_dAlgorithmName
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeAlgorithm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAlgorithmName' - The name of the algorithm to describe.
describeAlgorithm
    :: Text -- ^ 'dAlgorithmName'
    -> DescribeAlgorithm
describeAlgorithm pAlgorithmName_
  = DescribeAlgorithm'{_dAlgorithmName =
                         pAlgorithmName_}

-- | The name of the algorithm to describe.
dAlgorithmName :: Lens' DescribeAlgorithm Text
dAlgorithmName = lens _dAlgorithmName (\ s a -> s{_dAlgorithmName = a})

instance AWSRequest DescribeAlgorithm where
        type Rs DescribeAlgorithm = DescribeAlgorithmResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAlgorithmResponse' <$>
                   (x .?> "ValidationSpecification") <*>
                     (x .?> "InferenceSpecification")
                     <*> (x .?> "AlgorithmDescription")
                     <*> (x .?> "CertifyForMarketplace")
                     <*> (x .?> "ProductId")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "AlgorithmName")
                     <*> (x .:> "AlgorithmArn")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "TrainingSpecification")
                     <*> (x .:> "AlgorithmStatus")
                     <*> (x .:> "AlgorithmStatusDetails"))

instance Hashable DescribeAlgorithm where

instance NFData DescribeAlgorithm where

instance ToHeaders DescribeAlgorithm where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeAlgorithm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAlgorithm where
        toJSON DescribeAlgorithm'{..}
          = object
              (catMaybes
                 [Just ("AlgorithmName" .= _dAlgorithmName)])

instance ToPath DescribeAlgorithm where
        toPath = const "/"

instance ToQuery DescribeAlgorithm where
        toQuery = const mempty

-- | /See:/ 'describeAlgorithmResponse' smart constructor.
data DescribeAlgorithmResponse = DescribeAlgorithmResponse'{_dscrbalgrthmrsValidationSpecification
                                                            ::
                                                            !(Maybe
                                                                AlgorithmValidationSpecification),
                                                            _dscrbalgrthmrsInferenceSpecification
                                                            ::
                                                            !(Maybe
                                                                InferenceSpecification),
                                                            _dscrbalgrthmrsAlgorithmDescription
                                                            :: !(Maybe Text),
                                                            _dscrbalgrthmrsCertifyForMarketplace
                                                            :: !(Maybe Bool),
                                                            _dscrbalgrthmrsProductId
                                                            :: !(Maybe Text),
                                                            _dscrbalgrthmrsResponseStatus
                                                            :: !Int,
                                                            _dscrbalgrthmrsAlgorithmName
                                                            :: !Text,
                                                            _dscrbalgrthmrsAlgorithmARN
                                                            :: !Text,
                                                            _dscrbalgrthmrsCreationTime
                                                            :: !POSIX,
                                                            _dscrbalgrthmrsTrainingSpecification
                                                            ::
                                                            !TrainingSpecification,
                                                            _dscrbalgrthmrsAlgorithmStatus
                                                            :: !AlgorithmStatus,
                                                            _dscrbalgrthmrsAlgorithmStatusDetails
                                                            ::
                                                            !AlgorithmStatusDetails}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DescribeAlgorithmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbalgrthmrsValidationSpecification' - Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
--
-- * 'dscrbalgrthmrsInferenceSpecification' - Details about inference jobs that the algorithm runs.
--
-- * 'dscrbalgrthmrsAlgorithmDescription' - A brief summary about the algorithm.
--
-- * 'dscrbalgrthmrsCertifyForMarketplace' - Whether the algorithm is certified to be listed in AWS Marketplace.
--
-- * 'dscrbalgrthmrsProductId' - The product identifier of the algorithm.
--
-- * 'dscrbalgrthmrsResponseStatus' - -- | The response status code.
--
-- * 'dscrbalgrthmrsAlgorithmName' - The name of the algorithm being described.
--
-- * 'dscrbalgrthmrsAlgorithmARN' - The Amazon Resource Name (ARN) of the algorithm.
--
-- * 'dscrbalgrthmrsCreationTime' - A timestamp specifying when the algorithm was created.
--
-- * 'dscrbalgrthmrsTrainingSpecification' - Details about training jobs run by this algorithm.
--
-- * 'dscrbalgrthmrsAlgorithmStatus' - The current status of the algorithm.
--
-- * 'dscrbalgrthmrsAlgorithmStatusDetails' - Details about the current status of the algorithm.
describeAlgorithmResponse
    :: Int -- ^ 'dscrbalgrthmrsResponseStatus'
    -> Text -- ^ 'dscrbalgrthmrsAlgorithmName'
    -> Text -- ^ 'dscrbalgrthmrsAlgorithmARN'
    -> UTCTime -- ^ 'dscrbalgrthmrsCreationTime'
    -> TrainingSpecification -- ^ 'dscrbalgrthmrsTrainingSpecification'
    -> AlgorithmStatus -- ^ 'dscrbalgrthmrsAlgorithmStatus'
    -> AlgorithmStatusDetails -- ^ 'dscrbalgrthmrsAlgorithmStatusDetails'
    -> DescribeAlgorithmResponse
describeAlgorithmResponse pResponseStatus_
  pAlgorithmName_ pAlgorithmARN_ pCreationTime_
  pTrainingSpecification_ pAlgorithmStatus_
  pAlgorithmStatusDetails_
  = DescribeAlgorithmResponse'{_dscrbalgrthmrsValidationSpecification
                                 = Nothing,
                               _dscrbalgrthmrsInferenceSpecification = Nothing,
                               _dscrbalgrthmrsAlgorithmDescription = Nothing,
                               _dscrbalgrthmrsCertifyForMarketplace = Nothing,
                               _dscrbalgrthmrsProductId = Nothing,
                               _dscrbalgrthmrsResponseStatus = pResponseStatus_,
                               _dscrbalgrthmrsAlgorithmName = pAlgorithmName_,
                               _dscrbalgrthmrsAlgorithmARN = pAlgorithmARN_,
                               _dscrbalgrthmrsCreationTime =
                                 _Time # pCreationTime_,
                               _dscrbalgrthmrsTrainingSpecification =
                                 pTrainingSpecification_,
                               _dscrbalgrthmrsAlgorithmStatus =
                                 pAlgorithmStatus_,
                               _dscrbalgrthmrsAlgorithmStatusDetails =
                                 pAlgorithmStatusDetails_}

-- | Details about configurations for one or more training jobs that Amazon SageMaker runs to test the algorithm.
dscrbalgrthmrsValidationSpecification :: Lens' DescribeAlgorithmResponse (Maybe AlgorithmValidationSpecification)
dscrbalgrthmrsValidationSpecification = lens _dscrbalgrthmrsValidationSpecification (\ s a -> s{_dscrbalgrthmrsValidationSpecification = a})

-- | Details about inference jobs that the algorithm runs.
dscrbalgrthmrsInferenceSpecification :: Lens' DescribeAlgorithmResponse (Maybe InferenceSpecification)
dscrbalgrthmrsInferenceSpecification = lens _dscrbalgrthmrsInferenceSpecification (\ s a -> s{_dscrbalgrthmrsInferenceSpecification = a})

-- | A brief summary about the algorithm.
dscrbalgrthmrsAlgorithmDescription :: Lens' DescribeAlgorithmResponse (Maybe Text)
dscrbalgrthmrsAlgorithmDescription = lens _dscrbalgrthmrsAlgorithmDescription (\ s a -> s{_dscrbalgrthmrsAlgorithmDescription = a})

-- | Whether the algorithm is certified to be listed in AWS Marketplace.
dscrbalgrthmrsCertifyForMarketplace :: Lens' DescribeAlgorithmResponse (Maybe Bool)
dscrbalgrthmrsCertifyForMarketplace = lens _dscrbalgrthmrsCertifyForMarketplace (\ s a -> s{_dscrbalgrthmrsCertifyForMarketplace = a})

-- | The product identifier of the algorithm.
dscrbalgrthmrsProductId :: Lens' DescribeAlgorithmResponse (Maybe Text)
dscrbalgrthmrsProductId = lens _dscrbalgrthmrsProductId (\ s a -> s{_dscrbalgrthmrsProductId = a})

-- | -- | The response status code.
dscrbalgrthmrsResponseStatus :: Lens' DescribeAlgorithmResponse Int
dscrbalgrthmrsResponseStatus = lens _dscrbalgrthmrsResponseStatus (\ s a -> s{_dscrbalgrthmrsResponseStatus = a})

-- | The name of the algorithm being described.
dscrbalgrthmrsAlgorithmName :: Lens' DescribeAlgorithmResponse Text
dscrbalgrthmrsAlgorithmName = lens _dscrbalgrthmrsAlgorithmName (\ s a -> s{_dscrbalgrthmrsAlgorithmName = a})

-- | The Amazon Resource Name (ARN) of the algorithm.
dscrbalgrthmrsAlgorithmARN :: Lens' DescribeAlgorithmResponse Text
dscrbalgrthmrsAlgorithmARN = lens _dscrbalgrthmrsAlgorithmARN (\ s a -> s{_dscrbalgrthmrsAlgorithmARN = a})

-- | A timestamp specifying when the algorithm was created.
dscrbalgrthmrsCreationTime :: Lens' DescribeAlgorithmResponse UTCTime
dscrbalgrthmrsCreationTime = lens _dscrbalgrthmrsCreationTime (\ s a -> s{_dscrbalgrthmrsCreationTime = a}) . _Time

-- | Details about training jobs run by this algorithm.
dscrbalgrthmrsTrainingSpecification :: Lens' DescribeAlgorithmResponse TrainingSpecification
dscrbalgrthmrsTrainingSpecification = lens _dscrbalgrthmrsTrainingSpecification (\ s a -> s{_dscrbalgrthmrsTrainingSpecification = a})

-- | The current status of the algorithm.
dscrbalgrthmrsAlgorithmStatus :: Lens' DescribeAlgorithmResponse AlgorithmStatus
dscrbalgrthmrsAlgorithmStatus = lens _dscrbalgrthmrsAlgorithmStatus (\ s a -> s{_dscrbalgrthmrsAlgorithmStatus = a})

-- | Details about the current status of the algorithm.
dscrbalgrthmrsAlgorithmStatusDetails :: Lens' DescribeAlgorithmResponse AlgorithmStatusDetails
dscrbalgrthmrsAlgorithmStatusDetails = lens _dscrbalgrthmrsAlgorithmStatusDetails (\ s a -> s{_dscrbalgrthmrsAlgorithmStatusDetails = a})

instance NFData DescribeAlgorithmResponse where
