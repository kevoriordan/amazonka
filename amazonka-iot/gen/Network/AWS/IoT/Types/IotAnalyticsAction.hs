{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotAnalyticsAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.IotAnalyticsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Sends message data to an AWS IoT Analytics channel.
--
--
--
-- /See:/ 'iotAnalyticsAction' smart constructor.
data IotAnalyticsAction = IotAnalyticsAction'{_iaaChannelARN
                                              :: !(Maybe Text),
                                              _iaaChannelName :: !(Maybe Text),
                                              _iaaRoleARN :: !(Maybe Text)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IotAnalyticsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaaChannelARN' - (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
--
-- * 'iaaChannelName' - The name of the IoT Analytics channel to which message data will be sent.
--
-- * 'iaaRoleARN' - The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
iotAnalyticsAction
    :: IotAnalyticsAction
iotAnalyticsAction
  = IotAnalyticsAction'{_iaaChannelARN = Nothing,
                        _iaaChannelName = Nothing, _iaaRoleARN = Nothing}

-- | (deprecated) The ARN of the IoT Analytics channel to which message data will be sent.
iaaChannelARN :: Lens' IotAnalyticsAction (Maybe Text)
iaaChannelARN = lens _iaaChannelARN (\ s a -> s{_iaaChannelARN = a})

-- | The name of the IoT Analytics channel to which message data will be sent.
iaaChannelName :: Lens' IotAnalyticsAction (Maybe Text)
iaaChannelName = lens _iaaChannelName (\ s a -> s{_iaaChannelName = a})

-- | The ARN of the role which has a policy that grants IoT Analytics permission to send message data via IoT Analytics (iotanalytics:BatchPutMessage).
iaaRoleARN :: Lens' IotAnalyticsAction (Maybe Text)
iaaRoleARN = lens _iaaRoleARN (\ s a -> s{_iaaRoleARN = a})

instance FromJSON IotAnalyticsAction where
        parseJSON
          = withObject "IotAnalyticsAction"
              (\ x ->
                 IotAnalyticsAction' <$>
                   (x .:? "channelArn") <*> (x .:? "channelName") <*>
                     (x .:? "roleArn"))

instance Hashable IotAnalyticsAction where

instance NFData IotAnalyticsAction where

instance ToJSON IotAnalyticsAction where
        toJSON IotAnalyticsAction'{..}
          = object
              (catMaybes
                 [("channelArn" .=) <$> _iaaChannelARN,
                  ("channelName" .=) <$> _iaaChannelName,
                  ("roleArn" .=) <$> _iaaRoleARN])
