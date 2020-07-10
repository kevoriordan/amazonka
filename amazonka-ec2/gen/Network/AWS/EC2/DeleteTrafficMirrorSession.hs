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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror session.
--
--
module Network.AWS.EC2.DeleteTrafficMirrorSession
    (
    -- * Creating a Request
      deleteTrafficMirrorSession
    , DeleteTrafficMirrorSession
    -- * Request Lenses
    , dlttrffcmrrrsssnDryRun
    , dlttrffcmrrrsssnTrafficMirrorSessionId

    -- * Destructuring the Response
    , deleteTrafficMirrorSessionResponse
    , DeleteTrafficMirrorSessionResponse
    -- * Response Lenses
    , dlttrffcmrrrsssnrsTrafficMirrorSessionId
    , dlttrffcmrrrsssnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTrafficMirrorSession' smart constructor.
data DeleteTrafficMirrorSession = DeleteTrafficMirrorSession'{_dlttrffcmrrrsssnDryRun
                                                              :: !(Maybe Bool),
                                                              _dlttrffcmrrrsssnTrafficMirrorSessionId
                                                              :: !Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteTrafficMirrorSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrffcmrrrsssnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlttrffcmrrrsssnTrafficMirrorSessionId' - The ID of the Traffic Mirror session.
deleteTrafficMirrorSession
    :: Text -- ^ 'dlttrffcmrrrsssnTrafficMirrorSessionId'
    -> DeleteTrafficMirrorSession
deleteTrafficMirrorSession pTrafficMirrorSessionId_
  = DeleteTrafficMirrorSession'{_dlttrffcmrrrsssnDryRun
                                  = Nothing,
                                _dlttrffcmrrrsssnTrafficMirrorSessionId =
                                  pTrafficMirrorSessionId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlttrffcmrrrsssnDryRun :: Lens' DeleteTrafficMirrorSession (Maybe Bool)
dlttrffcmrrrsssnDryRun = lens _dlttrffcmrrrsssnDryRun (\ s a -> s{_dlttrffcmrrrsssnDryRun = a})

-- | The ID of the Traffic Mirror session.
dlttrffcmrrrsssnTrafficMirrorSessionId :: Lens' DeleteTrafficMirrorSession Text
dlttrffcmrrrsssnTrafficMirrorSessionId = lens _dlttrffcmrrrsssnTrafficMirrorSessionId (\ s a -> s{_dlttrffcmrrrsssnTrafficMirrorSessionId = a})

instance AWSRequest DeleteTrafficMirrorSession where
        type Rs DeleteTrafficMirrorSession =
             DeleteTrafficMirrorSessionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTrafficMirrorSessionResponse' <$>
                   (x .@? "trafficMirrorSessionId") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTrafficMirrorSession where

instance NFData DeleteTrafficMirrorSession where

instance ToHeaders DeleteTrafficMirrorSession where
        toHeaders = const mempty

instance ToPath DeleteTrafficMirrorSession where
        toPath = const "/"

instance ToQuery DeleteTrafficMirrorSession where
        toQuery DeleteTrafficMirrorSession'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTrafficMirrorSession" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dlttrffcmrrrsssnDryRun,
               "TrafficMirrorSessionId" =:
                 _dlttrffcmrrrsssnTrafficMirrorSessionId]

-- | /See:/ 'deleteTrafficMirrorSessionResponse' smart constructor.
data DeleteTrafficMirrorSessionResponse = DeleteTrafficMirrorSessionResponse'{_dlttrffcmrrrsssnrsTrafficMirrorSessionId
                                                                              ::
                                                                              !(Maybe
                                                                                  Text),
                                                                              _dlttrffcmrrrsssnrsResponseStatus
                                                                              ::
                                                                              !Int}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrffcmrrrsssnrsTrafficMirrorSessionId' - The ID of the deleted Traffic Mirror session.
--
-- * 'dlttrffcmrrrsssnrsResponseStatus' - -- | The response status code.
deleteTrafficMirrorSessionResponse
    :: Int -- ^ 'dlttrffcmrrrsssnrsResponseStatus'
    -> DeleteTrafficMirrorSessionResponse
deleteTrafficMirrorSessionResponse pResponseStatus_
  = DeleteTrafficMirrorSessionResponse'{_dlttrffcmrrrsssnrsTrafficMirrorSessionId
                                          = Nothing,
                                        _dlttrffcmrrrsssnrsResponseStatus =
                                          pResponseStatus_}

-- | The ID of the deleted Traffic Mirror session.
dlttrffcmrrrsssnrsTrafficMirrorSessionId :: Lens' DeleteTrafficMirrorSessionResponse (Maybe Text)
dlttrffcmrrrsssnrsTrafficMirrorSessionId = lens _dlttrffcmrrrsssnrsTrafficMirrorSessionId (\ s a -> s{_dlttrffcmrrrsssnrsTrafficMirrorSessionId = a})

-- | -- | The response status code.
dlttrffcmrrrsssnrsResponseStatus :: Lens' DeleteTrafficMirrorSessionResponse Int
dlttrffcmrrrsssnrsResponseStatus = lens _dlttrffcmrrrsssnrsResponseStatus (\ s a -> s{_dlttrffcmrrrsssnrsResponseStatus = a})

instance NFData DeleteTrafficMirrorSessionResponse
         where
