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
-- Module      : Network.AWS.MediaLive.DescribeInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces details about an input
module Network.AWS.MediaLive.DescribeInput
    (
    -- * Creating a Request
      describeInput
    , DescribeInput
    -- * Request Lenses
    , dInputId

    -- * Destructuring the Response
    , describeInputResponse
    , DescribeInputResponse
    -- * Response Lenses
    , dscrbinptrsState
    , dscrbinptrsSecurityGroups
    , dscrbinptrsARN
    , dscrbinptrsSources
    , dscrbinptrsDestinations
    , dscrbinptrsName
    , dscrbinptrsAttachedChannels
    , dscrbinptrsId
    , dscrbinptrsInputClass
    , dscrbinptrsType
    , dscrbinptrsMediaConnectFlows
    , dscrbinptrsInputSourceType
    , dscrbinptrsTags
    , dscrbinptrsRoleARN
    , dscrbinptrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeInputRequest
--
-- /See:/ 'describeInput' smart constructor.
newtype DescribeInput = DescribeInput'{_dInputId ::
                                       Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInputId' - Unique ID of the input
describeInput
    :: Text -- ^ 'dInputId'
    -> DescribeInput
describeInput pInputId_
  = DescribeInput'{_dInputId = pInputId_}

-- | Unique ID of the input
dInputId :: Lens' DescribeInput Text
dInputId = lens _dInputId (\ s a -> s{_dInputId = a})

instance AWSRequest DescribeInput where
        type Rs DescribeInput = DescribeInputResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInputResponse' <$>
                   (x .?> "state") <*>
                     (x .?> "securityGroups" .!@ mempty)
                     <*> (x .?> "arn")
                     <*> (x .?> "sources" .!@ mempty)
                     <*> (x .?> "destinations" .!@ mempty)
                     <*> (x .?> "name")
                     <*> (x .?> "attachedChannels" .!@ mempty)
                     <*> (x .?> "id")
                     <*> (x .?> "inputClass")
                     <*> (x .?> "type")
                     <*> (x .?> "mediaConnectFlows" .!@ mempty)
                     <*> (x .?> "inputSourceType")
                     <*> (x .?> "tags" .!@ mempty)
                     <*> (x .?> "roleArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInput where

instance NFData DescribeInput where

instance ToHeaders DescribeInput where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeInput where
        toPath DescribeInput'{..}
          = mconcat ["/prod/inputs/", toBS _dInputId]

instance ToQuery DescribeInput where
        toQuery = const mempty

-- | Placeholder documentation for DescribeInputResponse
--
-- /See:/ 'describeInputResponse' smart constructor.
data DescribeInputResponse = DescribeInputResponse'{_dscrbinptrsState
                                                    :: !(Maybe InputState),
                                                    _dscrbinptrsSecurityGroups
                                                    :: !(Maybe [Text]),
                                                    _dscrbinptrsARN ::
                                                    !(Maybe Text),
                                                    _dscrbinptrsSources ::
                                                    !(Maybe [InputSource]),
                                                    _dscrbinptrsDestinations ::
                                                    !(Maybe [InputDestination]),
                                                    _dscrbinptrsName ::
                                                    !(Maybe Text),
                                                    _dscrbinptrsAttachedChannels
                                                    :: !(Maybe [Text]),
                                                    _dscrbinptrsId ::
                                                    !(Maybe Text),
                                                    _dscrbinptrsInputClass ::
                                                    !(Maybe InputClass),
                                                    _dscrbinptrsType ::
                                                    !(Maybe InputType),
                                                    _dscrbinptrsMediaConnectFlows
                                                    ::
                                                    !(Maybe [MediaConnectFlow]),
                                                    _dscrbinptrsInputSourceType
                                                    :: !(Maybe InputSourceType),
                                                    _dscrbinptrsTags ::
                                                    !(Maybe (Map Text Text)),
                                                    _dscrbinptrsRoleARN ::
                                                    !(Maybe Text),
                                                    _dscrbinptrsResponseStatus
                                                    :: !Int}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'DescribeInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbinptrsState' - Undocumented member.
--
-- * 'dscrbinptrsSecurityGroups' - A list of IDs for all the Input Security Groups attached to the input.
--
-- * 'dscrbinptrsARN' - The Unique ARN of the input (generated, immutable).
--
-- * 'dscrbinptrsSources' - A list of the sources of the input (PULL-type).
--
-- * 'dscrbinptrsDestinations' - A list of the destinations of the input (PUSH-type).
--
-- * 'dscrbinptrsName' - The user-assigned name (This is a mutable value).
--
-- * 'dscrbinptrsAttachedChannels' - A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
--
-- * 'dscrbinptrsId' - The generated ID of the input (unique for user account, immutable).
--
-- * 'dscrbinptrsInputClass' - STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
--
-- * 'dscrbinptrsType' - Undocumented member.
--
-- * 'dscrbinptrsMediaConnectFlows' - A list of MediaConnect Flows for this input.
--
-- * 'dscrbinptrsInputSourceType' - Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
--
-- * 'dscrbinptrsTags' - A collection of key-value pairs.
--
-- * 'dscrbinptrsRoleARN' - The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
--
-- * 'dscrbinptrsResponseStatus' - -- | The response status code.
describeInputResponse
    :: Int -- ^ 'dscrbinptrsResponseStatus'
    -> DescribeInputResponse
describeInputResponse pResponseStatus_
  = DescribeInputResponse'{_dscrbinptrsState = Nothing,
                           _dscrbinptrsSecurityGroups = Nothing,
                           _dscrbinptrsARN = Nothing,
                           _dscrbinptrsSources = Nothing,
                           _dscrbinptrsDestinations = Nothing,
                           _dscrbinptrsName = Nothing,
                           _dscrbinptrsAttachedChannels = Nothing,
                           _dscrbinptrsId = Nothing,
                           _dscrbinptrsInputClass = Nothing,
                           _dscrbinptrsType = Nothing,
                           _dscrbinptrsMediaConnectFlows = Nothing,
                           _dscrbinptrsInputSourceType = Nothing,
                           _dscrbinptrsTags = Nothing,
                           _dscrbinptrsRoleARN = Nothing,
                           _dscrbinptrsResponseStatus = pResponseStatus_}

-- | Undocumented member.
dscrbinptrsState :: Lens' DescribeInputResponse (Maybe InputState)
dscrbinptrsState = lens _dscrbinptrsState (\ s a -> s{_dscrbinptrsState = a})

-- | A list of IDs for all the Input Security Groups attached to the input.
dscrbinptrsSecurityGroups :: Lens' DescribeInputResponse [Text]
dscrbinptrsSecurityGroups = lens _dscrbinptrsSecurityGroups (\ s a -> s{_dscrbinptrsSecurityGroups = a}) . _Default . _Coerce

-- | The Unique ARN of the input (generated, immutable).
dscrbinptrsARN :: Lens' DescribeInputResponse (Maybe Text)
dscrbinptrsARN = lens _dscrbinptrsARN (\ s a -> s{_dscrbinptrsARN = a})

-- | A list of the sources of the input (PULL-type).
dscrbinptrsSources :: Lens' DescribeInputResponse [InputSource]
dscrbinptrsSources = lens _dscrbinptrsSources (\ s a -> s{_dscrbinptrsSources = a}) . _Default . _Coerce

-- | A list of the destinations of the input (PUSH-type).
dscrbinptrsDestinations :: Lens' DescribeInputResponse [InputDestination]
dscrbinptrsDestinations = lens _dscrbinptrsDestinations (\ s a -> s{_dscrbinptrsDestinations = a}) . _Default . _Coerce

-- | The user-assigned name (This is a mutable value).
dscrbinptrsName :: Lens' DescribeInputResponse (Maybe Text)
dscrbinptrsName = lens _dscrbinptrsName (\ s a -> s{_dscrbinptrsName = a})

-- | A list of channel IDs that that input is attached to (currently an input can only be attached to one channel).
dscrbinptrsAttachedChannels :: Lens' DescribeInputResponse [Text]
dscrbinptrsAttachedChannels = lens _dscrbinptrsAttachedChannels (\ s a -> s{_dscrbinptrsAttachedChannels = a}) . _Default . _Coerce

-- | The generated ID of the input (unique for user account, immutable).
dscrbinptrsId :: Lens' DescribeInputResponse (Maybe Text)
dscrbinptrsId = lens _dscrbinptrsId (\ s a -> s{_dscrbinptrsId = a})

-- | STANDARD - MediaLive expects two sources to be connected to this input. If the channel is also STANDARD, both sources will be ingested. If the channel is SINGLE_PIPELINE, only the first source will be ingested; the second source will always be ignored, even if the first source fails. SINGLE_PIPELINE - You can connect only one source to this input. If the ChannelClass is also  SINGLE_PIPELINE, this value is valid. If the ChannelClass is STANDARD, this value is not valid because the channel requires two sources in the input.
dscrbinptrsInputClass :: Lens' DescribeInputResponse (Maybe InputClass)
dscrbinptrsInputClass = lens _dscrbinptrsInputClass (\ s a -> s{_dscrbinptrsInputClass = a})

-- | Undocumented member.
dscrbinptrsType :: Lens' DescribeInputResponse (Maybe InputType)
dscrbinptrsType = lens _dscrbinptrsType (\ s a -> s{_dscrbinptrsType = a})

-- | A list of MediaConnect Flows for this input.
dscrbinptrsMediaConnectFlows :: Lens' DescribeInputResponse [MediaConnectFlow]
dscrbinptrsMediaConnectFlows = lens _dscrbinptrsMediaConnectFlows (\ s a -> s{_dscrbinptrsMediaConnectFlows = a}) . _Default . _Coerce

-- | Certain pull input sources can be dynamic, meaning that they can have their URL's dynamically changes during input switch actions. Presently, this functionality only works with MP4_FILE inputs.
dscrbinptrsInputSourceType :: Lens' DescribeInputResponse (Maybe InputSourceType)
dscrbinptrsInputSourceType = lens _dscrbinptrsInputSourceType (\ s a -> s{_dscrbinptrsInputSourceType = a})

-- | A collection of key-value pairs.
dscrbinptrsTags :: Lens' DescribeInputResponse (HashMap Text Text)
dscrbinptrsTags = lens _dscrbinptrsTags (\ s a -> s{_dscrbinptrsTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role this input assumes during and after creation.
dscrbinptrsRoleARN :: Lens' DescribeInputResponse (Maybe Text)
dscrbinptrsRoleARN = lens _dscrbinptrsRoleARN (\ s a -> s{_dscrbinptrsRoleARN = a})

-- | -- | The response status code.
dscrbinptrsResponseStatus :: Lens' DescribeInputResponse Int
dscrbinptrsResponseStatus = lens _dscrbinptrsResponseStatus (\ s a -> s{_dscrbinptrsResponseStatus = a})

instance NFData DescribeInputResponse where
