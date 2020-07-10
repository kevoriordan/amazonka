{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourceMappingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.EventSourceMappingConfiguration where

import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A mapping between an AWS resource and an AWS Lambda function. See 'CreateEventSourceMapping' for details.
--
--
--
-- /See:/ 'eventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'{_esmcEventSourceARN
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcState
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcFunctionARN
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcBisectBatchOnFunctionError
                                                                        ::
                                                                        !(Maybe
                                                                            Bool),
                                                                        _esmcUUId
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcParallelizationFactor
                                                                        ::
                                                                        !(Maybe
                                                                            Nat),
                                                                        _esmcLastProcessingResult
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcMaximumRetryAttempts
                                                                        ::
                                                                        !(Maybe
                                                                            Nat),
                                                                        _esmcBatchSize
                                                                        ::
                                                                        !(Maybe
                                                                            Nat),
                                                                        _esmcStateTransitionReason
                                                                        ::
                                                                        !(Maybe
                                                                            Text),
                                                                        _esmcMaximumBatchingWindowInSeconds
                                                                        ::
                                                                        !(Maybe
                                                                            Nat),
                                                                        _esmcMaximumRecordAgeInSeconds
                                                                        ::
                                                                        !(Maybe
                                                                            Nat),
                                                                        _esmcLastModified
                                                                        ::
                                                                        !(Maybe
                                                                            POSIX),
                                                                        _esmcDestinationConfig
                                                                        ::
                                                                        !(Maybe
                                                                            DestinationConfig)}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'EventSourceMappingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esmcEventSourceARN' - The Amazon Resource Name (ARN) of the event source.
--
-- * 'esmcState' - The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
--
-- * 'esmcFunctionARN' - The ARN of the Lambda function.
--
-- * 'esmcBisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry.
--
-- * 'esmcUUId' - The identifier of the event source mapping.
--
-- * 'esmcParallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
--
-- * 'esmcLastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
--
-- * 'esmcMaximumRetryAttempts' - (Streams) The maximum number of times to retry when the function returns an error.
--
-- * 'esmcBatchSize' - The maximum number of items to retrieve in a single batch.
--
-- * 'esmcStateTransitionReason' - Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
--
-- * 'esmcMaximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- * 'esmcMaximumRecordAgeInSeconds' - (Streams) The maximum age of a record that Lambda sends to a function for processing.
--
-- * 'esmcLastModified' - The date that the event source mapping was last updated, or its state changed.
--
-- * 'esmcDestinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
eventSourceMappingConfiguration
    :: EventSourceMappingConfiguration
eventSourceMappingConfiguration
  = EventSourceMappingConfiguration'{_esmcEventSourceARN
                                       = Nothing,
                                     _esmcState = Nothing,
                                     _esmcFunctionARN = Nothing,
                                     _esmcBisectBatchOnFunctionError = Nothing,
                                     _esmcUUId = Nothing,
                                     _esmcParallelizationFactor = Nothing,
                                     _esmcLastProcessingResult = Nothing,
                                     _esmcMaximumRetryAttempts = Nothing,
                                     _esmcBatchSize = Nothing,
                                     _esmcStateTransitionReason = Nothing,
                                     _esmcMaximumBatchingWindowInSeconds =
                                       Nothing,
                                     _esmcMaximumRecordAgeInSeconds = Nothing,
                                     _esmcLastModified = Nothing,
                                     _esmcDestinationConfig = Nothing}

-- | The Amazon Resource Name (ARN) of the event source.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\ s a -> s{_esmcEventSourceARN = a})

-- | The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\ s a -> s{_esmcState = a})

-- | The ARN of the Lambda function.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\ s a -> s{_esmcFunctionARN = a})

-- | (Streams) If the function returns an error, split the batch in two and retry.
esmcBisectBatchOnFunctionError :: Lens' EventSourceMappingConfiguration (Maybe Bool)
esmcBisectBatchOnFunctionError = lens _esmcBisectBatchOnFunctionError (\ s a -> s{_esmcBisectBatchOnFunctionError = a})

-- | The identifier of the event source mapping.
esmcUUId :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUId = lens _esmcUUId (\ s a -> s{_esmcUUId = a})

-- | (Streams) The number of batches to process from each shard concurrently.
esmcParallelizationFactor :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcParallelizationFactor = lens _esmcParallelizationFactor (\ s a -> s{_esmcParallelizationFactor = a}) . mapping _Nat

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\ s a -> s{_esmcLastProcessingResult = a})

-- | (Streams) The maximum number of times to retry when the function returns an error.
esmcMaximumRetryAttempts :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcMaximumRetryAttempts = lens _esmcMaximumRetryAttempts (\ s a -> s{_esmcMaximumRetryAttempts = a}) . mapping _Nat

-- | The maximum number of items to retrieve in a single batch.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\ s a -> s{_esmcBatchSize = a}) . mapping _Nat

-- | Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\ s a -> s{_esmcStateTransitionReason = a})

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
esmcMaximumBatchingWindowInSeconds :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcMaximumBatchingWindowInSeconds = lens _esmcMaximumBatchingWindowInSeconds (\ s a -> s{_esmcMaximumBatchingWindowInSeconds = a}) . mapping _Nat

-- | (Streams) The maximum age of a record that Lambda sends to a function for processing.
esmcMaximumRecordAgeInSeconds :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcMaximumRecordAgeInSeconds = lens _esmcMaximumRecordAgeInSeconds (\ s a -> s{_esmcMaximumRecordAgeInSeconds = a}) . mapping _Nat

-- | The date that the event source mapping was last updated, or its state changed.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\ s a -> s{_esmcLastModified = a}) . mapping _Time

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
esmcDestinationConfig :: Lens' EventSourceMappingConfiguration (Maybe DestinationConfig)
esmcDestinationConfig = lens _esmcDestinationConfig (\ s a -> s{_esmcDestinationConfig = a})

instance FromJSON EventSourceMappingConfiguration
         where
        parseJSON
          = withObject "EventSourceMappingConfiguration"
              (\ x ->
                 EventSourceMappingConfiguration' <$>
                   (x .:? "EventSourceArn") <*> (x .:? "State") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "BisectBatchOnFunctionError")
                     <*> (x .:? "UUID")
                     <*> (x .:? "ParallelizationFactor")
                     <*> (x .:? "LastProcessingResult")
                     <*> (x .:? "MaximumRetryAttempts")
                     <*> (x .:? "BatchSize")
                     <*> (x .:? "StateTransitionReason")
                     <*> (x .:? "MaximumBatchingWindowInSeconds")
                     <*> (x .:? "MaximumRecordAgeInSeconds")
                     <*> (x .:? "LastModified")
                     <*> (x .:? "DestinationConfig"))

instance Hashable EventSourceMappingConfiguration
         where

instance NFData EventSourceMappingConfiguration where
