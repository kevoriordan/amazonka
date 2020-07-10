{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Channel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Channel where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.EncoderSettings
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.MediaLive.Types.PipelineDetail
import Network.AWS.Prelude

-- | Placeholder documentation for Channel
--
-- /See:/ 'channel' smart constructor.
data Channel = Channel'{_cState ::
                        !(Maybe ChannelState),
                        _cLogLevel :: !(Maybe LogLevel),
                        _cARN :: !(Maybe Text),
                        _cPipelinesRunningCount :: !(Maybe Int),
                        _cPipelineDetails :: !(Maybe [PipelineDetail]),
                        _cInputSpecification :: !(Maybe InputSpecification),
                        _cInputAttachments :: !(Maybe [InputAttachment]),
                        _cDestinations :: !(Maybe [OutputDestination]),
                        _cName :: !(Maybe Text), _cId :: !(Maybe Text),
                        _cChannelClass :: !(Maybe ChannelClass),
                        _cEgressEndpoints ::
                        !(Maybe [ChannelEgressEndpoint]),
                        _cTags :: !(Maybe (Map Text Text)),
                        _cEncoderSettings :: !(Maybe EncoderSettings),
                        _cRoleARN :: !(Maybe Text)}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - Undocumented member.
--
-- * 'cLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'cARN' - The unique arn of the channel.
--
-- * 'cPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'cPipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- * 'cInputSpecification' - Undocumented member.
--
-- * 'cInputAttachments' - List of input attachments for channel.
--
-- * 'cDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'cName' - The name of the channel. (user-mutable)
--
-- * 'cId' - The unique id of the channel.
--
-- * 'cChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'cEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'cTags' - A collection of key-value pairs.
--
-- * 'cEncoderSettings' - Undocumented member.
--
-- * 'cRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
channel
    :: Channel
channel
  = Channel'{_cState = Nothing, _cLogLevel = Nothing,
             _cARN = Nothing, _cPipelinesRunningCount = Nothing,
             _cPipelineDetails = Nothing,
             _cInputSpecification = Nothing,
             _cInputAttachments = Nothing,
             _cDestinations = Nothing, _cName = Nothing,
             _cId = Nothing, _cChannelClass = Nothing,
             _cEgressEndpoints = Nothing, _cTags = Nothing,
             _cEncoderSettings = Nothing, _cRoleARN = Nothing}

-- | Undocumented member.
cState :: Lens' Channel (Maybe ChannelState)
cState = lens _cState (\ s a -> s{_cState = a})

-- | The log level being written to CloudWatch Logs.
cLogLevel :: Lens' Channel (Maybe LogLevel)
cLogLevel = lens _cLogLevel (\ s a -> s{_cLogLevel = a})

-- | The unique arn of the channel.
cARN :: Lens' Channel (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | The number of currently healthy pipelines.
cPipelinesRunningCount :: Lens' Channel (Maybe Int)
cPipelinesRunningCount = lens _cPipelinesRunningCount (\ s a -> s{_cPipelinesRunningCount = a})

-- | Runtime details for the pipelines of a running channel.
cPipelineDetails :: Lens' Channel [PipelineDetail]
cPipelineDetails = lens _cPipelineDetails (\ s a -> s{_cPipelineDetails = a}) . _Default . _Coerce

-- | Undocumented member.
cInputSpecification :: Lens' Channel (Maybe InputSpecification)
cInputSpecification = lens _cInputSpecification (\ s a -> s{_cInputSpecification = a})

-- | List of input attachments for channel.
cInputAttachments :: Lens' Channel [InputAttachment]
cInputAttachments = lens _cInputAttachments (\ s a -> s{_cInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
cDestinations :: Lens' Channel [OutputDestination]
cDestinations = lens _cDestinations (\ s a -> s{_cDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
cName :: Lens' Channel (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | The unique id of the channel.
cId :: Lens' Channel (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
cChannelClass :: Lens' Channel (Maybe ChannelClass)
cChannelClass = lens _cChannelClass (\ s a -> s{_cChannelClass = a})

-- | The endpoints where outgoing connections initiate from
cEgressEndpoints :: Lens' Channel [ChannelEgressEndpoint]
cEgressEndpoints = lens _cEgressEndpoints (\ s a -> s{_cEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
cTags :: Lens' Channel (HashMap Text Text)
cTags = lens _cTags (\ s a -> s{_cTags = a}) . _Default . _Map

-- | Undocumented member.
cEncoderSettings :: Lens' Channel (Maybe EncoderSettings)
cEncoderSettings = lens _cEncoderSettings (\ s a -> s{_cEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
cRoleARN :: Lens' Channel (Maybe Text)
cRoleARN = lens _cRoleARN (\ s a -> s{_cRoleARN = a})

instance FromJSON Channel where
        parseJSON
          = withObject "Channel"
              (\ x ->
                 Channel' <$>
                   (x .:? "state") <*> (x .:? "logLevel") <*>
                     (x .:? "arn")
                     <*> (x .:? "pipelinesRunningCount")
                     <*> (x .:? "pipelineDetails" .!= mempty)
                     <*> (x .:? "inputSpecification")
                     <*> (x .:? "inputAttachments" .!= mempty)
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "channelClass")
                     <*> (x .:? "egressEndpoints" .!= mempty)
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "encoderSettings")
                     <*> (x .:? "roleArn"))

instance Hashable Channel where

instance NFData Channel where
