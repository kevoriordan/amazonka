{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains configuration for a Multiplex event
--
-- /See:/ 'multiplexSettings' smart constructor.
data MultiplexSettings = MultiplexSettings'{_mssMaximumVideoBufferDelayMilliseconds
                                            :: !(Maybe Nat),
                                            _mssTransportStreamReservedBitrate
                                            :: !(Maybe Nat),
                                            _mssTransportStreamBitrate :: !Nat,
                                            _mssTransportStreamId :: !Nat}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssMaximumVideoBufferDelayMilliseconds' - Maximum video buffer delay in milliseconds.
--
-- * 'mssTransportStreamReservedBitrate' - Transport stream reserved bit rate.
--
-- * 'mssTransportStreamBitrate' - Transport stream bit rate.
--
-- * 'mssTransportStreamId' - Transport stream ID.
multiplexSettings
    :: Natural -- ^ 'mssTransportStreamBitrate'
    -> Natural -- ^ 'mssTransportStreamId'
    -> MultiplexSettings
multiplexSettings pTransportStreamBitrate_
  pTransportStreamId_
  = MultiplexSettings'{_mssMaximumVideoBufferDelayMilliseconds
                         = Nothing,
                       _mssTransportStreamReservedBitrate = Nothing,
                       _mssTransportStreamBitrate =
                         _Nat # pTransportStreamBitrate_,
                       _mssTransportStreamId = _Nat # pTransportStreamId_}

-- | Maximum video buffer delay in milliseconds.
mssMaximumVideoBufferDelayMilliseconds :: Lens' MultiplexSettings (Maybe Natural)
mssMaximumVideoBufferDelayMilliseconds = lens _mssMaximumVideoBufferDelayMilliseconds (\ s a -> s{_mssMaximumVideoBufferDelayMilliseconds = a}) . mapping _Nat

-- | Transport stream reserved bit rate.
mssTransportStreamReservedBitrate :: Lens' MultiplexSettings (Maybe Natural)
mssTransportStreamReservedBitrate = lens _mssTransportStreamReservedBitrate (\ s a -> s{_mssTransportStreamReservedBitrate = a}) . mapping _Nat

-- | Transport stream bit rate.
mssTransportStreamBitrate :: Lens' MultiplexSettings Natural
mssTransportStreamBitrate = lens _mssTransportStreamBitrate (\ s a -> s{_mssTransportStreamBitrate = a}) . _Nat

-- | Transport stream ID.
mssTransportStreamId :: Lens' MultiplexSettings Natural
mssTransportStreamId = lens _mssTransportStreamId (\ s a -> s{_mssTransportStreamId = a}) . _Nat

instance FromJSON MultiplexSettings where
        parseJSON
          = withObject "MultiplexSettings"
              (\ x ->
                 MultiplexSettings' <$>
                   (x .:? "maximumVideoBufferDelayMilliseconds") <*>
                     (x .:? "transportStreamReservedBitrate")
                     <*> (x .: "transportStreamBitrate")
                     <*> (x .: "transportStreamId"))

instance Hashable MultiplexSettings where

instance NFData MultiplexSettings where

instance ToJSON MultiplexSettings where
        toJSON MultiplexSettings'{..}
          = object
              (catMaybes
                 [("maximumVideoBufferDelayMilliseconds" .=) <$>
                    _mssMaximumVideoBufferDelayMilliseconds,
                  ("transportStreamReservedBitrate" .=) <$>
                    _mssTransportStreamReservedBitrate,
                  Just
                    ("transportStreamBitrate" .=
                       _mssTransportStreamBitrate),
                  Just ("transportStreamId" .= _mssTransportStreamId)])
