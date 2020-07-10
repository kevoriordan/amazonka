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
-- Module      : Network.AWS.DirectoryService.DeleteTrust
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your AWS Managed Microsoft AD directory and an external domain.
--
--
module Network.AWS.DirectoryService.DeleteTrust
    (
    -- * Creating a Request
      deleteTrust
    , DeleteTrust
    -- * Request Lenses
    , dtDeleteAssociatedConditionalForwarder
    , dtTrustId

    -- * Destructuring the Response
    , deleteTrustResponse
    , DeleteTrustResponse
    -- * Response Lenses
    , dlttrstrsTrustId
    , dlttrstrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Deletes the local side of an existing trust relationship between the AWS Managed Microsoft AD directory and the external domain.
--
--
--
-- /See:/ 'deleteTrust' smart constructor.
data DeleteTrust = DeleteTrust'{_dtDeleteAssociatedConditionalForwarder
                                :: !(Maybe Bool),
                                _dtTrustId :: !Text}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtDeleteAssociatedConditionalForwarder' - Delete a conditional forwarder as part of a DeleteTrustRequest.
--
-- * 'dtTrustId' - The Trust ID of the trust relationship to be deleted.
deleteTrust
    :: Text -- ^ 'dtTrustId'
    -> DeleteTrust
deleteTrust pTrustId_
  = DeleteTrust'{_dtDeleteAssociatedConditionalForwarder
                   = Nothing,
                 _dtTrustId = pTrustId_}

-- | Delete a conditional forwarder as part of a DeleteTrustRequest.
dtDeleteAssociatedConditionalForwarder :: Lens' DeleteTrust (Maybe Bool)
dtDeleteAssociatedConditionalForwarder = lens _dtDeleteAssociatedConditionalForwarder (\ s a -> s{_dtDeleteAssociatedConditionalForwarder = a})

-- | The Trust ID of the trust relationship to be deleted.
dtTrustId :: Lens' DeleteTrust Text
dtTrustId = lens _dtTrustId (\ s a -> s{_dtTrustId = a})

instance AWSRequest DeleteTrust where
        type Rs DeleteTrust = DeleteTrustResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTrustResponse' <$>
                   (x .?> "TrustId") <*> (pure (fromEnum s)))

instance Hashable DeleteTrust where

instance NFData DeleteTrust where

instance ToHeaders DeleteTrust where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DeleteTrust" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTrust where
        toJSON DeleteTrust'{..}
          = object
              (catMaybes
                 [("DeleteAssociatedConditionalForwarder" .=) <$>
                    _dtDeleteAssociatedConditionalForwarder,
                  Just ("TrustId" .= _dtTrustId)])

instance ToPath DeleteTrust where
        toPath = const "/"

instance ToQuery DeleteTrust where
        toQuery = const mempty

-- | The result of a DeleteTrust request.
--
--
--
-- /See:/ 'deleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'{_dlttrstrsTrustId
                                                :: !(Maybe Text),
                                                _dlttrstrsResponseStatus ::
                                                !Int}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrustResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrstrsTrustId' - The Trust ID of the trust relationship that was deleted.
--
-- * 'dlttrstrsResponseStatus' - -- | The response status code.
deleteTrustResponse
    :: Int -- ^ 'dlttrstrsResponseStatus'
    -> DeleteTrustResponse
deleteTrustResponse pResponseStatus_
  = DeleteTrustResponse'{_dlttrstrsTrustId = Nothing,
                         _dlttrstrsResponseStatus = pResponseStatus_}

-- | The Trust ID of the trust relationship that was deleted.
dlttrstrsTrustId :: Lens' DeleteTrustResponse (Maybe Text)
dlttrstrsTrustId = lens _dlttrstrsTrustId (\ s a -> s{_dlttrstrsTrustId = a})

-- | -- | The response status code.
dlttrstrsResponseStatus :: Lens' DeleteTrustResponse Int
dlttrstrsResponseStatus = lens _dlttrstrsResponseStatus (\ s a -> s{_dlttrstrsResponseStatus = a})

instance NFData DeleteTrustResponse where
