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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror target.
--
--
-- You cannot delete a Traffic Mirror target that is in use by a Traffic Mirror session.
--
module Network.AWS.EC2.DeleteTrafficMirrorTarget
    (
    -- * Creating a Request
      deleteTrafficMirrorTarget
    , DeleteTrafficMirrorTarget
    -- * Request Lenses
    , dlttrffcmrrrtrgtDryRun
    , dlttrffcmrrrtrgtTrafficMirrorTargetId

    -- * Destructuring the Response
    , deleteTrafficMirrorTargetResponse
    , DeleteTrafficMirrorTargetResponse
    -- * Response Lenses
    , dlttrffcmrrrtrgtrsTrafficMirrorTargetId
    , dlttrffcmrrrtrgtrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTrafficMirrorTarget' smart constructor.
data DeleteTrafficMirrorTarget = DeleteTrafficMirrorTarget'{_dlttrffcmrrrtrgtDryRun
                                                            :: !(Maybe Bool),
                                                            _dlttrffcmrrrtrgtTrafficMirrorTargetId
                                                            :: !Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DeleteTrafficMirrorTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrffcmrrrtrgtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlttrffcmrrrtrgtTrafficMirrorTargetId' - The ID of the Traffic Mirror target.
deleteTrafficMirrorTarget
    :: Text -- ^ 'dlttrffcmrrrtrgtTrafficMirrorTargetId'
    -> DeleteTrafficMirrorTarget
deleteTrafficMirrorTarget pTrafficMirrorTargetId_
  = DeleteTrafficMirrorTarget'{_dlttrffcmrrrtrgtDryRun
                                 = Nothing,
                               _dlttrffcmrrrtrgtTrafficMirrorTargetId =
                                 pTrafficMirrorTargetId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlttrffcmrrrtrgtDryRun :: Lens' DeleteTrafficMirrorTarget (Maybe Bool)
dlttrffcmrrrtrgtDryRun = lens _dlttrffcmrrrtrgtDryRun (\ s a -> s{_dlttrffcmrrrtrgtDryRun = a})

-- | The ID of the Traffic Mirror target.
dlttrffcmrrrtrgtTrafficMirrorTargetId :: Lens' DeleteTrafficMirrorTarget Text
dlttrffcmrrrtrgtTrafficMirrorTargetId = lens _dlttrffcmrrrtrgtTrafficMirrorTargetId (\ s a -> s{_dlttrffcmrrrtrgtTrafficMirrorTargetId = a})

instance AWSRequest DeleteTrafficMirrorTarget where
        type Rs DeleteTrafficMirrorTarget =
             DeleteTrafficMirrorTargetResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTrafficMirrorTargetResponse' <$>
                   (x .@? "trafficMirrorTargetId") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteTrafficMirrorTarget where

instance NFData DeleteTrafficMirrorTarget where

instance ToHeaders DeleteTrafficMirrorTarget where
        toHeaders = const mempty

instance ToPath DeleteTrafficMirrorTarget where
        toPath = const "/"

instance ToQuery DeleteTrafficMirrorTarget where
        toQuery DeleteTrafficMirrorTarget'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTrafficMirrorTarget" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dlttrffcmrrrtrgtDryRun,
               "TrafficMirrorTargetId" =:
                 _dlttrffcmrrrtrgtTrafficMirrorTargetId]

-- | /See:/ 'deleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'{_dlttrffcmrrrtrgtrsTrafficMirrorTargetId
                                                                            ::
                                                                            !(Maybe
                                                                                Text),
                                                                            _dlttrffcmrrrtrgtrsResponseStatus
                                                                            ::
                                                                            !Int}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrffcmrrrtrgtrsTrafficMirrorTargetId' - The ID of the deleted Traffic Mirror target.
--
-- * 'dlttrffcmrrrtrgtrsResponseStatus' - -- | The response status code.
deleteTrafficMirrorTargetResponse
    :: Int -- ^ 'dlttrffcmrrrtrgtrsResponseStatus'
    -> DeleteTrafficMirrorTargetResponse
deleteTrafficMirrorTargetResponse pResponseStatus_
  = DeleteTrafficMirrorTargetResponse'{_dlttrffcmrrrtrgtrsTrafficMirrorTargetId
                                         = Nothing,
                                       _dlttrffcmrrrtrgtrsResponseStatus =
                                         pResponseStatus_}

-- | The ID of the deleted Traffic Mirror target.
dlttrffcmrrrtrgtrsTrafficMirrorTargetId :: Lens' DeleteTrafficMirrorTargetResponse (Maybe Text)
dlttrffcmrrrtrgtrsTrafficMirrorTargetId = lens _dlttrffcmrrrtrgtrsTrafficMirrorTargetId (\ s a -> s{_dlttrffcmrrrtrgtrsTrafficMirrorTargetId = a})

-- | -- | The response status code.
dlttrffcmrrrtrgtrsResponseStatus :: Lens' DeleteTrafficMirrorTargetResponse Int
dlttrffcmrrrtrgtrsResponseStatus = lens _dlttrffcmrrrtrgtrsResponseStatus (\ s a -> s{_dlttrffcmrrrtrgtrsResponseStatus = a})

instance NFData DeleteTrafficMirrorTargetResponse
         where
