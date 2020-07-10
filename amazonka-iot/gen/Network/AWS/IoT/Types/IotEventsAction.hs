{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.IotEventsAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.IotEventsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Sends an input to an AWS IoT Events detector.
--
--
--
-- /See:/ 'iotEventsAction' smart constructor.
data IotEventsAction = IotEventsAction'{_ieaMessageId
                                        :: !(Maybe Text),
                                        _ieaInputName :: !Text,
                                        _ieaRoleARN :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IotEventsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ieaMessageId' - [Optional] Use this to ensure that only one input (message) with a given messageId will be processed by an AWS IoT Events detector.
--
-- * 'ieaInputName' - The name of the AWS IoT Events input.
--
-- * 'ieaRoleARN' - The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
iotEventsAction
    :: Text -- ^ 'ieaInputName'
    -> Text -- ^ 'ieaRoleARN'
    -> IotEventsAction
iotEventsAction pInputName_ pRoleARN_
  = IotEventsAction'{_ieaMessageId = Nothing,
                     _ieaInputName = pInputName_, _ieaRoleARN = pRoleARN_}

-- | [Optional] Use this to ensure that only one input (message) with a given messageId will be processed by an AWS IoT Events detector.
ieaMessageId :: Lens' IotEventsAction (Maybe Text)
ieaMessageId = lens _ieaMessageId (\ s a -> s{_ieaMessageId = a})

-- | The name of the AWS IoT Events input.
ieaInputName :: Lens' IotEventsAction Text
ieaInputName = lens _ieaInputName (\ s a -> s{_ieaInputName = a})

-- | The ARN of the role that grants AWS IoT permission to send an input to an AWS IoT Events detector. ("Action":"iotevents:BatchPutMessage").
ieaRoleARN :: Lens' IotEventsAction Text
ieaRoleARN = lens _ieaRoleARN (\ s a -> s{_ieaRoleARN = a})

instance FromJSON IotEventsAction where
        parseJSON
          = withObject "IotEventsAction"
              (\ x ->
                 IotEventsAction' <$>
                   (x .:? "messageId") <*> (x .: "inputName") <*>
                     (x .: "roleArn"))

instance Hashable IotEventsAction where

instance NFData IotEventsAction where

instance ToJSON IotEventsAction where
        toJSON IotEventsAction'{..}
          = object
              (catMaybes
                 [("messageId" .=) <$> _ieaMessageId,
                  Just ("inputName" .= _ieaInputName),
                  Just ("roleArn" .= _ieaRoleARN)])
