{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ChannelSummary where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a channel.
--
--
--
-- /See:/ 'channelSummary' smart constructor.
data ChannelSummary = ChannelSummary'{_csCreationTime
                                      :: !(Maybe POSIX),
                                      _csStatus :: !(Maybe ChannelStatus),
                                      _csChannelName :: !(Maybe Text),
                                      _csChannelStorage ::
                                      !(Maybe ChannelStorageSummary),
                                      _csLastUpdateTime :: !(Maybe POSIX)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCreationTime' - When the channel was created.
--
-- * 'csStatus' - The status of the channel.
--
-- * 'csChannelName' - The name of the channel.
--
-- * 'csChannelStorage' - Where channel data is stored.
--
-- * 'csLastUpdateTime' - The last time the channel was updated.
channelSummary
    :: ChannelSummary
channelSummary
  = ChannelSummary'{_csCreationTime = Nothing,
                    _csStatus = Nothing, _csChannelName = Nothing,
                    _csChannelStorage = Nothing,
                    _csLastUpdateTime = Nothing}

-- | When the channel was created.
csCreationTime :: Lens' ChannelSummary (Maybe UTCTime)
csCreationTime = lens _csCreationTime (\ s a -> s{_csCreationTime = a}) . mapping _Time

-- | The status of the channel.
csStatus :: Lens' ChannelSummary (Maybe ChannelStatus)
csStatus = lens _csStatus (\ s a -> s{_csStatus = a})

-- | The name of the channel.
csChannelName :: Lens' ChannelSummary (Maybe Text)
csChannelName = lens _csChannelName (\ s a -> s{_csChannelName = a})

-- | Where channel data is stored.
csChannelStorage :: Lens' ChannelSummary (Maybe ChannelStorageSummary)
csChannelStorage = lens _csChannelStorage (\ s a -> s{_csChannelStorage = a})

-- | The last time the channel was updated.
csLastUpdateTime :: Lens' ChannelSummary (Maybe UTCTime)
csLastUpdateTime = lens _csLastUpdateTime (\ s a -> s{_csLastUpdateTime = a}) . mapping _Time

instance FromJSON ChannelSummary where
        parseJSON
          = withObject "ChannelSummary"
              (\ x ->
                 ChannelSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "channelName")
                     <*> (x .:? "channelStorage")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable ChannelSummary where

instance NFData ChannelSummary where
