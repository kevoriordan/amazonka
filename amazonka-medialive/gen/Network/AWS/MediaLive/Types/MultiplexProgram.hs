{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexProgram
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexProgram where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.MultiplexProgramPacketIdentifiersMap
import Network.AWS.MediaLive.Types.MultiplexProgramSettings
import Network.AWS.Prelude

-- | The multiplex program object.
--
-- /See:/ 'multiplexProgram' smart constructor.
data MultiplexProgram = MultiplexProgram'{_mpPacketIdentifiersMap
                                          ::
                                          !(Maybe
                                              MultiplexProgramPacketIdentifiersMap),
                                          _mpProgramName :: !(Maybe Text),
                                          _mpChannelId :: !(Maybe Text),
                                          _mpMultiplexProgramSettings ::
                                          !(Maybe MultiplexProgramSettings)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'mpProgramName' - The name of the multiplex program.
--
-- * 'mpChannelId' - The MediaLive channel associated with the program.
--
-- * 'mpMultiplexProgramSettings' - The settings for this multiplex program.
multiplexProgram
    :: MultiplexProgram
multiplexProgram
  = MultiplexProgram'{_mpPacketIdentifiersMap =
                        Nothing,
                      _mpProgramName = Nothing, _mpChannelId = Nothing,
                      _mpMultiplexProgramSettings = Nothing}

-- | The packet identifier map for this multiplex program.
mpPacketIdentifiersMap :: Lens' MultiplexProgram (Maybe MultiplexProgramPacketIdentifiersMap)
mpPacketIdentifiersMap = lens _mpPacketIdentifiersMap (\ s a -> s{_mpPacketIdentifiersMap = a})

-- | The name of the multiplex program.
mpProgramName :: Lens' MultiplexProgram (Maybe Text)
mpProgramName = lens _mpProgramName (\ s a -> s{_mpProgramName = a})

-- | The MediaLive channel associated with the program.
mpChannelId :: Lens' MultiplexProgram (Maybe Text)
mpChannelId = lens _mpChannelId (\ s a -> s{_mpChannelId = a})

-- | The settings for this multiplex program.
mpMultiplexProgramSettings :: Lens' MultiplexProgram (Maybe MultiplexProgramSettings)
mpMultiplexProgramSettings = lens _mpMultiplexProgramSettings (\ s a -> s{_mpMultiplexProgramSettings = a})

instance FromJSON MultiplexProgram where
        parseJSON
          = withObject "MultiplexProgram"
              (\ x ->
                 MultiplexProgram' <$>
                   (x .:? "packetIdentifiersMap") <*>
                     (x .:? "programName")
                     <*> (x .:? "channelId")
                     <*> (x .:? "multiplexProgramSettings"))

instance Hashable MultiplexProgram where

instance NFData MultiplexProgram where
