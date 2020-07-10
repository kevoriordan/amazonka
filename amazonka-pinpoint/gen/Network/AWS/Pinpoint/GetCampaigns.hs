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
-- Module      : Network.AWS.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all the campaigns that are associated with an application.
--
--
module Network.AWS.Pinpoint.GetCampaigns
    (
    -- * Creating a Request
      getCampaigns
    , GetCampaigns
    -- * Request Lenses
    , gtcmpgnsToken
    , gtcmpgnsPageSize
    , gtcmpgnsApplicationId

    -- * Destructuring the Response
    , getCampaignsResponse
    , GetCampaignsResponse
    -- * Response Lenses
    , gtcmpgnsrsResponseStatus
    , gtcmpgnsrsCampaignsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'{_gtcmpgnsToken ::
                                  !(Maybe Text),
                                  _gtcmpgnsPageSize :: !(Maybe Text),
                                  _gtcmpgnsApplicationId :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaigns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcmpgnsToken' - The NextToken string that specifies which page of results to return in a paginated response.
--
-- * 'gtcmpgnsPageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- * 'gtcmpgnsApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getCampaigns
    :: Text -- ^ 'gtcmpgnsApplicationId'
    -> GetCampaigns
getCampaigns pApplicationId_
  = GetCampaigns'{_gtcmpgnsToken = Nothing,
                  _gtcmpgnsPageSize = Nothing,
                  _gtcmpgnsApplicationId = pApplicationId_}

-- | The NextToken string that specifies which page of results to return in a paginated response.
gtcmpgnsToken :: Lens' GetCampaigns (Maybe Text)
gtcmpgnsToken = lens _gtcmpgnsToken (\ s a -> s{_gtcmpgnsToken = a})

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
gtcmpgnsPageSize :: Lens' GetCampaigns (Maybe Text)
gtcmpgnsPageSize = lens _gtcmpgnsPageSize (\ s a -> s{_gtcmpgnsPageSize = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gtcmpgnsApplicationId :: Lens' GetCampaigns Text
gtcmpgnsApplicationId = lens _gtcmpgnsApplicationId (\ s a -> s{_gtcmpgnsApplicationId = a})

instance AWSRequest GetCampaigns where
        type Rs GetCampaigns = GetCampaignsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaigns where

instance NFData GetCampaigns where

instance ToHeaders GetCampaigns where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaigns where
        toPath GetCampaigns'{..}
          = mconcat
              ["/v1/apps/", toBS _gtcmpgnsApplicationId,
               "/campaigns"]

instance ToQuery GetCampaigns where
        toQuery GetCampaigns'{..}
          = mconcat
              ["token" =: _gtcmpgnsToken,
               "page-size" =: _gtcmpgnsPageSize]

-- | /See:/ 'getCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'{_gtcmpgnsrsResponseStatus
                                                  :: !Int,
                                                  _gtcmpgnsrsCampaignsResponse
                                                  :: !CampaignsResponse}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcmpgnsrsResponseStatus' - -- | The response status code.
--
-- * 'gtcmpgnsrsCampaignsResponse' - Undocumented member.
getCampaignsResponse
    :: Int -- ^ 'gtcmpgnsrsResponseStatus'
    -> CampaignsResponse -- ^ 'gtcmpgnsrsCampaignsResponse'
    -> GetCampaignsResponse
getCampaignsResponse pResponseStatus_
  pCampaignsResponse_
  = GetCampaignsResponse'{_gtcmpgnsrsResponseStatus =
                            pResponseStatus_,
                          _gtcmpgnsrsCampaignsResponse = pCampaignsResponse_}

-- | -- | The response status code.
gtcmpgnsrsResponseStatus :: Lens' GetCampaignsResponse Int
gtcmpgnsrsResponseStatus = lens _gtcmpgnsrsResponseStatus (\ s a -> s{_gtcmpgnsrsResponseStatus = a})

-- | Undocumented member.
gtcmpgnsrsCampaignsResponse :: Lens' GetCampaignsResponse CampaignsResponse
gtcmpgnsrsCampaignsResponse = lens _gtcmpgnsrsCampaignsResponse (\ s a -> s{_gtcmpgnsrsCampaignsResponse = a})

instance NFData GetCampaignsResponse where
