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
-- Module      : Network.AWS.MediaLive.DeleteMultiplexProgram
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a program from a multiplex.
module Network.AWS.MediaLive.DeleteMultiplexProgram
    (
    -- * Creating a Request
      deleteMultiplexProgram
    , DeleteMultiplexProgram
    -- * Request Lenses
    , dmpMultiplexId
    , dmpProgramName

    -- * Destructuring the Response
    , deleteMultiplexProgramResponse
    , DeleteMultiplexProgramResponse
    -- * Response Lenses
    , dmprsPacketIdentifiersMap
    , dmprsProgramName
    , dmprsChannelId
    , dmprsMultiplexProgramSettings
    , dmprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteMultiplexProgramRequest
--
-- /See:/ 'deleteMultiplexProgram' smart constructor.
data DeleteMultiplexProgram = DeleteMultiplexProgram'{_dmpMultiplexId
                                                      :: !Text,
                                                      _dmpProgramName :: !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DeleteMultiplexProgram' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmpMultiplexId' - The ID of the multiplex that the program belongs to.
--
-- * 'dmpProgramName' - The multiplex program name.
deleteMultiplexProgram
    :: Text -- ^ 'dmpMultiplexId'
    -> Text -- ^ 'dmpProgramName'
    -> DeleteMultiplexProgram
deleteMultiplexProgram pMultiplexId_ pProgramName_
  = DeleteMultiplexProgram'{_dmpMultiplexId =
                              pMultiplexId_,
                            _dmpProgramName = pProgramName_}

-- | The ID of the multiplex that the program belongs to.
dmpMultiplexId :: Lens' DeleteMultiplexProgram Text
dmpMultiplexId = lens _dmpMultiplexId (\ s a -> s{_dmpMultiplexId = a})

-- | The multiplex program name.
dmpProgramName :: Lens' DeleteMultiplexProgram Text
dmpProgramName = lens _dmpProgramName (\ s a -> s{_dmpProgramName = a})

instance AWSRequest DeleteMultiplexProgram where
        type Rs DeleteMultiplexProgram =
             DeleteMultiplexProgramResponse
        request = delete mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DeleteMultiplexProgramResponse' <$>
                   (x .?> "packetIdentifiersMap") <*>
                     (x .?> "programName")
                     <*> (x .?> "channelId")
                     <*> (x .?> "multiplexProgramSettings")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteMultiplexProgram where

instance NFData DeleteMultiplexProgram where

instance ToHeaders DeleteMultiplexProgram where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteMultiplexProgram where
        toPath DeleteMultiplexProgram'{..}
          = mconcat
              ["/prod/multiplexes/", toBS _dmpMultiplexId,
               "/programs/", toBS _dmpProgramName]

instance ToQuery DeleteMultiplexProgram where
        toQuery = const mempty

-- | Placeholder documentation for DeleteMultiplexProgramResponse
--
-- /See:/ 'deleteMultiplexProgramResponse' smart constructor.
data DeleteMultiplexProgramResponse = DeleteMultiplexProgramResponse'{_dmprsPacketIdentifiersMap
                                                                      ::
                                                                      !(Maybe
                                                                          MultiplexProgramPacketIdentifiersMap),
                                                                      _dmprsProgramName
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dmprsChannelId
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dmprsMultiplexProgramSettings
                                                                      ::
                                                                      !(Maybe
                                                                          MultiplexProgramSettings),
                                                                      _dmprsResponseStatus
                                                                      :: !Int}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'DeleteMultiplexProgramResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmprsPacketIdentifiersMap' - The packet identifier map for this multiplex program.
--
-- * 'dmprsProgramName' - The name of the multiplex program.
--
-- * 'dmprsChannelId' - The MediaLive channel associated with the program.
--
-- * 'dmprsMultiplexProgramSettings' - The settings for this multiplex program.
--
-- * 'dmprsResponseStatus' - -- | The response status code.
deleteMultiplexProgramResponse
    :: Int -- ^ 'dmprsResponseStatus'
    -> DeleteMultiplexProgramResponse
deleteMultiplexProgramResponse pResponseStatus_
  = DeleteMultiplexProgramResponse'{_dmprsPacketIdentifiersMap
                                      = Nothing,
                                    _dmprsProgramName = Nothing,
                                    _dmprsChannelId = Nothing,
                                    _dmprsMultiplexProgramSettings = Nothing,
                                    _dmprsResponseStatus = pResponseStatus_}

-- | The packet identifier map for this multiplex program.
dmprsPacketIdentifiersMap :: Lens' DeleteMultiplexProgramResponse (Maybe MultiplexProgramPacketIdentifiersMap)
dmprsPacketIdentifiersMap = lens _dmprsPacketIdentifiersMap (\ s a -> s{_dmprsPacketIdentifiersMap = a})

-- | The name of the multiplex program.
dmprsProgramName :: Lens' DeleteMultiplexProgramResponse (Maybe Text)
dmprsProgramName = lens _dmprsProgramName (\ s a -> s{_dmprsProgramName = a})

-- | The MediaLive channel associated with the program.
dmprsChannelId :: Lens' DeleteMultiplexProgramResponse (Maybe Text)
dmprsChannelId = lens _dmprsChannelId (\ s a -> s{_dmprsChannelId = a})

-- | The settings for this multiplex program.
dmprsMultiplexProgramSettings :: Lens' DeleteMultiplexProgramResponse (Maybe MultiplexProgramSettings)
dmprsMultiplexProgramSettings = lens _dmprsMultiplexProgramSettings (\ s a -> s{_dmprsMultiplexProgramSettings = a})

-- | -- | The response status code.
dmprsResponseStatus :: Lens' DeleteMultiplexProgramResponse Int
dmprsResponseStatus = lens _dmprsResponseStatus (\ s a -> s{_dmprsResponseStatus = a})

instance NFData DeleteMultiplexProgramResponse where
