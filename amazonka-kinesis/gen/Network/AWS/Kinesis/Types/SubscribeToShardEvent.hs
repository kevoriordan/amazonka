{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.SubscribeToShardEvent where

import Network.AWS.Kinesis.Types.Record
import Network.AWS.Lens
import Network.AWS.Prelude

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer. 
--
--
--
-- /See:/ 'subscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'{_stseRecords
                                                    :: ![Record],
                                                    _stseContinuationSequenceNumber
                                                    :: !Text,
                                                    _stseMillisBehindLatest ::
                                                    !Nat}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'SubscribeToShardEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stseRecords' - 
--
-- * 'stseContinuationSequenceNumber' - Use this as @StartingSequenceNumber@ in the next call to 'SubscribeToShard' .
--
-- * 'stseMillisBehindLatest' - The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
subscribeToShardEvent
    :: Text -- ^ 'stseContinuationSequenceNumber'
    -> Natural -- ^ 'stseMillisBehindLatest'
    -> SubscribeToShardEvent
subscribeToShardEvent pContinuationSequenceNumber_
  pMillisBehindLatest_
  = SubscribeToShardEvent'{_stseRecords = mempty,
                           _stseContinuationSequenceNumber =
                             pContinuationSequenceNumber_,
                           _stseMillisBehindLatest =
                             _Nat # pMillisBehindLatest_}

-- | 
stseRecords :: Lens' SubscribeToShardEvent [Record]
stseRecords = lens _stseRecords (\ s a -> s{_stseRecords = a}) . _Coerce

-- | Use this as @StartingSequenceNumber@ in the next call to 'SubscribeToShard' .
stseContinuationSequenceNumber :: Lens' SubscribeToShardEvent Text
stseContinuationSequenceNumber = lens _stseContinuationSequenceNumber (\ s a -> s{_stseContinuationSequenceNumber = a})

-- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
stseMillisBehindLatest :: Lens' SubscribeToShardEvent Natural
stseMillisBehindLatest = lens _stseMillisBehindLatest (\ s a -> s{_stseMillisBehindLatest = a}) . _Nat

instance FromJSON SubscribeToShardEvent where
        parseJSON
          = withObject "SubscribeToShardEvent"
              (\ x ->
                 SubscribeToShardEvent' <$>
                   (x .:? "Records" .!= mempty) <*>
                     (x .: "ContinuationSequenceNumber")
                     <*> (x .: "MillisBehindLatest"))

instance Hashable SubscribeToShardEvent where

instance NFData SubscribeToShardEvent where
