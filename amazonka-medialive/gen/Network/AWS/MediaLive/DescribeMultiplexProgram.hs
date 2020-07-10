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
-- Module      : Network.AWS.MediaLive.DescribeMultiplexProgram
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the details for a program in a multiplex.
module Network.AWS.MediaLive.DescribeMultiplexProgram
    (
    -- * Creating a Request
      describeMultiplexProgram
    , DescribeMultiplexProgram
    -- * Request Lenses
    , desMultiplexId
    , desProgramName

    -- * Destructuring the Response
    , describeMultiplexProgramResponse
    , DescribeMultiplexProgramResponse
    -- * Response Lenses
    , dscrbmltplxprgrmrsPacketIdentifiersMap
    , dscrbmltplxprgrmrsProgramName
    , dscrbmltplxprgrmrsChannelId
    , dscrbmltplxprgrmrsMultiplexProgramSettings
    , dscrbmltplxprgrmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeMultiplexProgramRequest
--
-- /See:/ 'describeMultiplexProgram' smart constructor.
data DescribeMultiplexProgram = DescribeMultiplexProgram'{_desMultiplexId
                                                          :: !Text,
                                                          _desProgramName ::
                                                          !Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DescribeMultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desMultiplexId' - The ID of the multiplex that the program belongs to.
--
-- * 'desProgramName' - The name of the program.
describeMultiplexProgram
    :: Text -- ^ 'desMultiplexId'
    -> Text -- ^ 'desProgramName'
    -> DescribeMultiplexProgram
describeMultiplexProgram pMultiplexId_ pProgramName_
  = DescribeMultiplexProgram'{_desMultiplexId =
                                pMultiplexId_,
                              _desProgramName = pProgramName_}

-- | The ID of the multiplex that the program belongs to.
desMultiplexId :: Lens' DescribeMultiplexProgram Text
desMultiplexId = lens _desMultiplexId (\ s a -> s{_desMultiplexId = a})

-- | The name of the program.
desProgramName :: Lens' DescribeMultiplexProgram Text
desProgramName = lens _desProgramName (\ s a -> s{_desProgramName = a})

instance AWSRequest DescribeMultiplexProgram where
        type Rs DescribeMultiplexProgram =
             DescribeMultiplexProgramResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMultiplexProgramResponse' <$>
                   (x .?> "packetIdentifiersMap") <*>
                     (x .?> "programName")
                     <*> (x .?> "channelId")
                     <*> (x .?> "multiplexProgramSettings")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMultiplexProgram where

instance NFData DescribeMultiplexProgram where

instance ToHeaders DescribeMultiplexProgram where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeMultiplexProgram where
        toPath DescribeMultiplexProgram'{..}
          = mconcat
              ["/prod/multiplexes/", toBS _desMultiplexId,
               "/programs/", toBS _desProgramName]

instance ToQuery DescribeMultiplexProgram where
        toQuery = const mempty

-- | Placeholder documentation for DescribeMultiplexProgramResponse
--
-- /See:/ 'describeMultiplexProgramResponse' smart constructor.
data DescribeMultiplexProgramResponse = DescribeMultiplexProgramResponse'{_dscrbmltplxprgrmrsPacketIdentifiersMap
                                                                          ::
                                                                          !(Maybe
                                                                              MultiplexProgramPacketIdentifiersMap),
                                                                          _dscrbmltplxprgrmrsProgramName
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _dscrbmltplxprgrmrsChannelId
                                                                          ::
                                                                          !(Maybe
                                                                              Text),
                                                                          _dscrbmltplxprgrmrsMultiplexProgramSettings
                                                                          ::
                                                                          !(Maybe
                                                                              MultiplexProgramSettings),
                                                                          _dscrbmltplxprgrmrsResponseStatus
                                                                          ::
                                                                          !Int}
                                          deriving (Eq, Read, Show, Data,
                                                    Typeable, Generic)

-- | Creates a value of 'DescribeMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbmltplxprgrmrsPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'dscrbmltplxprgrmrsProgramName' - The name of the multiplex program.
--
-- * 'dscrbmltplxprgrmrsChannelId' - The MediaLive channel associated with the program.
--
-- * 'dscrbmltplxprgrmrsMultiplexProgramSettings' - The settings for this multiplex program.
--
-- * 'dscrbmltplxprgrmrsResponseStatus' - -- | The response status code.
describeMultiplexProgramResponse
    :: Int -- ^ 'dscrbmltplxprgrmrsResponseStatus'
    -> DescribeMultiplexProgramResponse
describeMultiplexProgramResponse pResponseStatus_
  = DescribeMultiplexProgramResponse'{_dscrbmltplxprgrmrsPacketIdentifiersMap
                                        = Nothing,
                                      _dscrbmltplxprgrmrsProgramName = Nothing,
                                      _dscrbmltplxprgrmrsChannelId = Nothing,
                                      _dscrbmltplxprgrmrsMultiplexProgramSettings
                                        = Nothing,
                                      _dscrbmltplxprgrmrsResponseStatus =
                                        pResponseStatus_}

-- | The packet identifier map for this multiplex program.
dscrbmltplxprgrmrsPacketIdentifiersMap :: Lens' DescribeMultiplexProgramResponse (Maybe MultiplexProgramPacketIdentifiersMap)
dscrbmltplxprgrmrsPacketIdentifiersMap = lens _dscrbmltplxprgrmrsPacketIdentifiersMap (\ s a -> s{_dscrbmltplxprgrmrsPacketIdentifiersMap = a})

-- | The name of the multiplex program.
dscrbmltplxprgrmrsProgramName :: Lens' DescribeMultiplexProgramResponse (Maybe Text)
dscrbmltplxprgrmrsProgramName = lens _dscrbmltplxprgrmrsProgramName (\ s a -> s{_dscrbmltplxprgrmrsProgramName = a})

-- | The MediaLive channel associated with the program.
dscrbmltplxprgrmrsChannelId :: Lens' DescribeMultiplexProgramResponse (Maybe Text)
dscrbmltplxprgrmrsChannelId = lens _dscrbmltplxprgrmrsChannelId (\ s a -> s{_dscrbmltplxprgrmrsChannelId = a})

-- | The settings for this multiplex program.
dscrbmltplxprgrmrsMultiplexProgramSettings :: Lens' DescribeMultiplexProgramResponse (Maybe MultiplexProgramSettings)
dscrbmltplxprgrmrsMultiplexProgramSettings = lens _dscrbmltplxprgrmrsMultiplexProgramSettings (\ s a -> s{_dscrbmltplxprgrmrsMultiplexProgramSettings = a})

-- | -- | The response status code.
dscrbmltplxprgrmrsResponseStatus :: Lens' DescribeMultiplexProgramResponse Int
dscrbmltplxprgrmrsResponseStatus = lens _dscrbmltplxprgrmrsResponseStatus (\ s a -> s{_dscrbmltplxprgrmrsResponseStatus = a})

instance NFData DescribeMultiplexProgramResponse
         where
