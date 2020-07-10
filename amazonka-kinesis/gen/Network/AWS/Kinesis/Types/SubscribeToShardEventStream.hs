{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEventStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.SubscribeToShardEventStream where

import Network.AWS.Kinesis.Types.InternalFailureException
import Network.AWS.Kinesis.Types.KMSAccessDeniedException
import Network.AWS.Kinesis.Types.KMSDisabledException
import Network.AWS.Kinesis.Types.KMSInvalidStateException
import Network.AWS.Kinesis.Types.KMSNotFoundException
import Network.AWS.Kinesis.Types.KMSOptInRequired
import Network.AWS.Kinesis.Types.KMSThrottlingException
import Network.AWS.Kinesis.Types.ResourceInUseException
import Network.AWS.Kinesis.Types.ResourceNotFoundException
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'subscribeToShardEventStream' smart constructor.
data SubscribeToShardEventStream = SubscribeToShardEventStream'{_stsesKMSInvalidStateException
                                                                ::
                                                                !(Maybe
                                                                    KMSInvalidStateException),
                                                                _stsesKMSThrottlingException
                                                                ::
                                                                !(Maybe
                                                                    KMSThrottlingException),
                                                                _stsesKMSOptInRequired
                                                                ::
                                                                !(Maybe
                                                                    KMSOptInRequired),
                                                                _stsesKMSNotFoundException
                                                                ::
                                                                !(Maybe
                                                                    KMSNotFoundException),
                                                                _stsesKMSDisabledException
                                                                ::
                                                                !(Maybe
                                                                    KMSDisabledException),
                                                                _stsesInternalFailureException
                                                                ::
                                                                !(Maybe
                                                                    InternalFailureException),
                                                                _stsesResourceNotFoundException
                                                                ::
                                                                !(Maybe
                                                                    ResourceNotFoundException),
                                                                _stsesKMSAccessDeniedException
                                                                ::
                                                                !(Maybe
                                                                    KMSAccessDeniedException),
                                                                _stsesResourceInUseException
                                                                ::
                                                                !(Maybe
                                                                    ResourceInUseException),
                                                                _stsesSubscribeToShardEvent
                                                                ::
                                                                !SubscribeToShardEvent}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'SubscribeToShardEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsesKMSInvalidStateException' - Undocumented member.
--
-- * 'stsesKMSThrottlingException' - Undocumented member.
--
-- * 'stsesKMSOptInRequired' - Undocumented member.
--
-- * 'stsesKMSNotFoundException' - Undocumented member.
--
-- * 'stsesKMSDisabledException' - Undocumented member.
--
-- * 'stsesInternalFailureException' - Undocumented member.
--
-- * 'stsesResourceNotFoundException' - Undocumented member.
--
-- * 'stsesKMSAccessDeniedException' - Undocumented member.
--
-- * 'stsesResourceInUseException' - Undocumented member.
--
-- * 'stsesSubscribeToShardEvent' - Undocumented member.
subscribeToShardEventStream
    :: SubscribeToShardEvent -- ^ 'stsesSubscribeToShardEvent'
    -> SubscribeToShardEventStream
subscribeToShardEventStream pSubscribeToShardEvent_
  = SubscribeToShardEventStream'{_stsesKMSInvalidStateException
                                   = Nothing,
                                 _stsesKMSThrottlingException = Nothing,
                                 _stsesKMSOptInRequired = Nothing,
                                 _stsesKMSNotFoundException = Nothing,
                                 _stsesKMSDisabledException = Nothing,
                                 _stsesInternalFailureException = Nothing,
                                 _stsesResourceNotFoundException = Nothing,
                                 _stsesKMSAccessDeniedException = Nothing,
                                 _stsesResourceInUseException = Nothing,
                                 _stsesSubscribeToShardEvent =
                                   pSubscribeToShardEvent_}

-- | Undocumented member.
stsesKMSInvalidStateException :: Lens' SubscribeToShardEventStream (Maybe KMSInvalidStateException)
stsesKMSInvalidStateException = lens _stsesKMSInvalidStateException (\ s a -> s{_stsesKMSInvalidStateException = a})

-- | Undocumented member.
stsesKMSThrottlingException :: Lens' SubscribeToShardEventStream (Maybe KMSThrottlingException)
stsesKMSThrottlingException = lens _stsesKMSThrottlingException (\ s a -> s{_stsesKMSThrottlingException = a})

-- | Undocumented member.
stsesKMSOptInRequired :: Lens' SubscribeToShardEventStream (Maybe KMSOptInRequired)
stsesKMSOptInRequired = lens _stsesKMSOptInRequired (\ s a -> s{_stsesKMSOptInRequired = a})

-- | Undocumented member.
stsesKMSNotFoundException :: Lens' SubscribeToShardEventStream (Maybe KMSNotFoundException)
stsesKMSNotFoundException = lens _stsesKMSNotFoundException (\ s a -> s{_stsesKMSNotFoundException = a})

-- | Undocumented member.
stsesKMSDisabledException :: Lens' SubscribeToShardEventStream (Maybe KMSDisabledException)
stsesKMSDisabledException = lens _stsesKMSDisabledException (\ s a -> s{_stsesKMSDisabledException = a})

-- | Undocumented member.
stsesInternalFailureException :: Lens' SubscribeToShardEventStream (Maybe InternalFailureException)
stsesInternalFailureException = lens _stsesInternalFailureException (\ s a -> s{_stsesInternalFailureException = a})

-- | Undocumented member.
stsesResourceNotFoundException :: Lens' SubscribeToShardEventStream (Maybe ResourceNotFoundException)
stsesResourceNotFoundException = lens _stsesResourceNotFoundException (\ s a -> s{_stsesResourceNotFoundException = a})

-- | Undocumented member.
stsesKMSAccessDeniedException :: Lens' SubscribeToShardEventStream (Maybe KMSAccessDeniedException)
stsesKMSAccessDeniedException = lens _stsesKMSAccessDeniedException (\ s a -> s{_stsesKMSAccessDeniedException = a})

-- | Undocumented member.
stsesResourceInUseException :: Lens' SubscribeToShardEventStream (Maybe ResourceInUseException)
stsesResourceInUseException = lens _stsesResourceInUseException (\ s a -> s{_stsesResourceInUseException = a})

-- | Undocumented member.
stsesSubscribeToShardEvent :: Lens' SubscribeToShardEventStream SubscribeToShardEvent
stsesSubscribeToShardEvent = lens _stsesSubscribeToShardEvent (\ s a -> s{_stsesSubscribeToShardEvent = a})

instance FromJSON SubscribeToShardEventStream where
        parseJSON
          = withObject "SubscribeToShardEventStream"
              (\ x ->
                 SubscribeToShardEventStream' <$>
                   (x .:? "KMSInvalidStateException") <*>
                     (x .:? "KMSThrottlingException")
                     <*> (x .:? "KMSOptInRequired")
                     <*> (x .:? "KMSNotFoundException")
                     <*> (x .:? "KMSDisabledException")
                     <*> (x .:? "InternalFailureException")
                     <*> (x .:? "ResourceNotFoundException")
                     <*> (x .:? "KMSAccessDeniedException")
                     <*> (x .:? "ResourceInUseException")
                     <*> (x .: "SubscribeToShardEvent"))

instance Hashable SubscribeToShardEventStream where

instance NFData SubscribeToShardEventStream where
