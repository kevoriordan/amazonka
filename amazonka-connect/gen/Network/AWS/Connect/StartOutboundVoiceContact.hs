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
-- Module      : Network.AWS.Connect.StartOutboundVoiceContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a contact flow to place an outbound call to a customer.
--
--
-- There is a 60 second dialing timeout for this operation. If the call is not connected after 60 seconds, it fails.
--
module Network.AWS.Connect.StartOutboundVoiceContact
    (
    -- * Creating a Request
      startOutboundVoiceContact
    , StartOutboundVoiceContact
    -- * Request Lenses
    , sovcClientToken
    , sovcQueueId
    , sovcAttributes
    , sovcSourcePhoneNumber
    , sovcDestinationPhoneNumber
    , sovcContactFlowId
    , sovcInstanceId

    -- * Destructuring the Response
    , startOutboundVoiceContactResponse
    , StartOutboundVoiceContactResponse
    -- * Response Lenses
    , sovcrsContactId
    , sovcrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startOutboundVoiceContact' smart constructor.
data StartOutboundVoiceContact = StartOutboundVoiceContact'{_sovcClientToken
                                                            :: !(Maybe Text),
                                                            _sovcQueueId ::
                                                            !(Maybe Text),
                                                            _sovcAttributes ::
                                                            !(Maybe
                                                                (Map Text
                                                                   Text)),
                                                            _sovcSourcePhoneNumber
                                                            :: !(Maybe Text),
                                                            _sovcDestinationPhoneNumber
                                                            :: !Text,
                                                            _sovcContactFlowId
                                                            :: !Text,
                                                            _sovcInstanceId ::
                                                            !Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'StartOutboundVoiceContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sovcClientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
--
-- * 'sovcQueueId' - The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
--
-- * 'sovcAttributes' - A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
--
-- * 'sovcSourcePhoneNumber' - The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
--
-- * 'sovcDestinationPhoneNumber' - The phone number of the customer, in E.164 format.
--
-- * 'sovcContactFlowId' - The identifier of the contact flow for the outbound call.
--
-- * 'sovcInstanceId' - The identifier of the Amazon Connect instance.
startOutboundVoiceContact
    :: Text -- ^ 'sovcDestinationPhoneNumber'
    -> Text -- ^ 'sovcContactFlowId'
    -> Text -- ^ 'sovcInstanceId'
    -> StartOutboundVoiceContact
startOutboundVoiceContact pDestinationPhoneNumber_
  pContactFlowId_ pInstanceId_
  = StartOutboundVoiceContact'{_sovcClientToken =
                                 Nothing,
                               _sovcQueueId = Nothing,
                               _sovcAttributes = Nothing,
                               _sovcSourcePhoneNumber = Nothing,
                               _sovcDestinationPhoneNumber =
                                 pDestinationPhoneNumber_,
                               _sovcContactFlowId = pContactFlowId_,
                               _sovcInstanceId = pInstanceId_}

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of the request. The token is valid for 7 days after creation. If a contact is already started, the contact ID is returned. If the contact is disconnected, a new contact is started.
sovcClientToken :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcClientToken = lens _sovcClientToken (\ s a -> s{_sovcClientToken = a})

-- | The queue for the call. If you specify a queue, the phone displayed for caller ID is the phone number specified in the queue. If you do not specify a queue, the queue defined in the contact flow is used. If you do not specify a queue, you must specify a source phone number.
sovcQueueId :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcQueueId = lens _sovcQueueId (\ s a -> s{_sovcQueueId = a})

-- | A custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
sovcAttributes :: Lens' StartOutboundVoiceContact (HashMap Text Text)
sovcAttributes = lens _sovcAttributes (\ s a -> s{_sovcAttributes = a}) . _Default . _Map

-- | The phone number associated with the Amazon Connect instance, in E.164 format. If you do not specify a source phone number, you must specify a queue.
sovcSourcePhoneNumber :: Lens' StartOutboundVoiceContact (Maybe Text)
sovcSourcePhoneNumber = lens _sovcSourcePhoneNumber (\ s a -> s{_sovcSourcePhoneNumber = a})

-- | The phone number of the customer, in E.164 format.
sovcDestinationPhoneNumber :: Lens' StartOutboundVoiceContact Text
sovcDestinationPhoneNumber = lens _sovcDestinationPhoneNumber (\ s a -> s{_sovcDestinationPhoneNumber = a})

-- | The identifier of the contact flow for the outbound call.
sovcContactFlowId :: Lens' StartOutboundVoiceContact Text
sovcContactFlowId = lens _sovcContactFlowId (\ s a -> s{_sovcContactFlowId = a})

-- | The identifier of the Amazon Connect instance.
sovcInstanceId :: Lens' StartOutboundVoiceContact Text
sovcInstanceId = lens _sovcInstanceId (\ s a -> s{_sovcInstanceId = a})

instance AWSRequest StartOutboundVoiceContact where
        type Rs StartOutboundVoiceContact =
             StartOutboundVoiceContactResponse
        request = putJSON connect
        response
          = receiveJSON
              (\ s h x ->
                 StartOutboundVoiceContactResponse' <$>
                   (x .?> "ContactId") <*> (pure (fromEnum s)))

instance Hashable StartOutboundVoiceContact where

instance NFData StartOutboundVoiceContact where

instance ToHeaders StartOutboundVoiceContact where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartOutboundVoiceContact where
        toJSON StartOutboundVoiceContact'{..}
          = object
              (catMaybes
                 [("ClientToken" .=) <$> _sovcClientToken,
                  ("QueueId" .=) <$> _sovcQueueId,
                  ("Attributes" .=) <$> _sovcAttributes,
                  ("SourcePhoneNumber" .=) <$> _sovcSourcePhoneNumber,
                  Just
                    ("DestinationPhoneNumber" .=
                       _sovcDestinationPhoneNumber),
                  Just ("ContactFlowId" .= _sovcContactFlowId),
                  Just ("InstanceId" .= _sovcInstanceId)])

instance ToPath StartOutboundVoiceContact where
        toPath = const "/contact/outbound-voice"

instance ToQuery StartOutboundVoiceContact where
        toQuery = const mempty

-- | /See:/ 'startOutboundVoiceContactResponse' smart constructor.
data StartOutboundVoiceContactResponse = StartOutboundVoiceContactResponse'{_sovcrsContactId
                                                                            ::
                                                                            !(Maybe
                                                                                Text),
                                                                            _sovcrsResponseStatus
                                                                            ::
                                                                            !Int}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'StartOutboundVoiceContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sovcrsContactId' - The identifier of this contact within the Amazon Connect instance.
--
-- * 'sovcrsResponseStatus' - -- | The response status code.
startOutboundVoiceContactResponse
    :: Int -- ^ 'sovcrsResponseStatus'
    -> StartOutboundVoiceContactResponse
startOutboundVoiceContactResponse pResponseStatus_
  = StartOutboundVoiceContactResponse'{_sovcrsContactId
                                         = Nothing,
                                       _sovcrsResponseStatus = pResponseStatus_}

-- | The identifier of this contact within the Amazon Connect instance.
sovcrsContactId :: Lens' StartOutboundVoiceContactResponse (Maybe Text)
sovcrsContactId = lens _sovcrsContactId (\ s a -> s{_sovcrsContactId = a})

-- | -- | The response status code.
sovcrsResponseStatus :: Lens' StartOutboundVoiceContactResponse Int
sovcrsResponseStatus = lens _sovcrsResponseStatus (\ s a -> s{_sovcrsResponseStatus = a})

instance NFData StartOutboundVoiceContactResponse
         where
