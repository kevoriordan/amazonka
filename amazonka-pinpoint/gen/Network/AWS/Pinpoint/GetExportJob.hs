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
-- Module      : Network.AWS.Pinpoint.GetExportJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of a specific export job for an application.
--
--
module Network.AWS.Pinpoint.GetExportJob
    (
    -- * Creating a Request
      getExportJob
    , GetExportJob
    -- * Request Lenses
    , gejApplicationId
    , gejJobId

    -- * Destructuring the Response
    , getExportJobResponse
    , GetExportJobResponse
    -- * Response Lenses
    , gtexprtjbrsResponseStatus
    , gtexprtjbrsExportJobResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getExportJob' smart constructor.
data GetExportJob = GetExportJob'{_gejApplicationId
                                  :: !Text,
                                  _gejJobId :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExportJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gejApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gejJobId' - The unique identifier for the job.
getExportJob
    :: Text -- ^ 'gejApplicationId'
    -> Text -- ^ 'gejJobId'
    -> GetExportJob
getExportJob pApplicationId_ pJobId_
  = GetExportJob'{_gejApplicationId = pApplicationId_,
                  _gejJobId = pJobId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gejApplicationId :: Lens' GetExportJob Text
gejApplicationId = lens _gejApplicationId (\ s a -> s{_gejApplicationId = a})

-- | The unique identifier for the job.
gejJobId :: Lens' GetExportJob Text
gejJobId = lens _gejJobId (\ s a -> s{_gejJobId = a})

instance AWSRequest GetExportJob where
        type Rs GetExportJob = GetExportJobResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetExportJobResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetExportJob where

instance NFData GetExportJob where

instance ToHeaders GetExportJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetExportJob where
        toPath GetExportJob'{..}
          = mconcat
              ["/v1/apps/", toBS _gejApplicationId,
               "/jobs/export/", toBS _gejJobId]

instance ToQuery GetExportJob where
        toQuery = const mempty

-- | /See:/ 'getExportJobResponse' smart constructor.
data GetExportJobResponse = GetExportJobResponse'{_gtexprtjbrsResponseStatus
                                                  :: !Int,
                                                  _gtexprtjbrsExportJobResponse
                                                  :: !ExportJobResponse}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetExportJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtexprtjbrsResponseStatus' - -- | The response status code.
--
-- * 'gtexprtjbrsExportJobResponse' - Undocumented member.
getExportJobResponse
    :: Int -- ^ 'gtexprtjbrsResponseStatus'
    -> ExportJobResponse -- ^ 'gtexprtjbrsExportJobResponse'
    -> GetExportJobResponse
getExportJobResponse pResponseStatus_
  pExportJobResponse_
  = GetExportJobResponse'{_gtexprtjbrsResponseStatus =
                            pResponseStatus_,
                          _gtexprtjbrsExportJobResponse = pExportJobResponse_}

-- | -- | The response status code.
gtexprtjbrsResponseStatus :: Lens' GetExportJobResponse Int
gtexprtjbrsResponseStatus = lens _gtexprtjbrsResponseStatus (\ s a -> s{_gtexprtjbrsResponseStatus = a})

-- | Undocumented member.
gtexprtjbrsExportJobResponse :: Lens' GetExportJobResponse ExportJobResponse
gtexprtjbrsExportJobResponse = lens _gtexprtjbrsExportJobResponse (\ s a -> s{_gtexprtjbrsExportJobResponse = a})

instance NFData GetExportJobResponse where
