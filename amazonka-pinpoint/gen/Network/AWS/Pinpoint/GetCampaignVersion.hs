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
-- Module      : Network.AWS.Pinpoint.GetCampaignVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for a specific version of a campaign.
--
--
module Network.AWS.Pinpoint.GetCampaignVersion
    (
    -- * Creating a Request
      getCampaignVersion
    , GetCampaignVersion
    -- * Request Lenses
    , gtcmpgnvrsnVersion
    , gtcmpgnvrsnApplicationId
    , gtcmpgnvrsnCampaignId

    -- * Destructuring the Response
    , getCampaignVersionResponse
    , GetCampaignVersionResponse
    -- * Response Lenses
    , gtcmpgnvrsnrsResponseStatus
    , gtcmpgnvrsnrsCampaignResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCampaignVersion' smart constructor.
data GetCampaignVersion = GetCampaignVersion'{_gtcmpgnvrsnVersion
                                              :: !Text,
                                              _gtcmpgnvrsnApplicationId ::
                                              !Text,
                                              _gtcmpgnvrsnCampaignId :: !Text}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCampaignVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcmpgnvrsnVersion' - The unique version number (Version property) for the campaign version.
--
-- * 'gtcmpgnvrsnApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'gtcmpgnvrsnCampaignId' - The unique identifier for the campaign.
getCampaignVersion
    :: Text -- ^ 'gtcmpgnvrsnVersion'
    -> Text -- ^ 'gtcmpgnvrsnApplicationId'
    -> Text -- ^ 'gtcmpgnvrsnCampaignId'
    -> GetCampaignVersion
getCampaignVersion pVersion_ pApplicationId_
  pCampaignId_
  = GetCampaignVersion'{_gtcmpgnvrsnVersion =
                          pVersion_,
                        _gtcmpgnvrsnApplicationId = pApplicationId_,
                        _gtcmpgnvrsnCampaignId = pCampaignId_}

-- | The unique version number (Version property) for the campaign version.
gtcmpgnvrsnVersion :: Lens' GetCampaignVersion Text
gtcmpgnvrsnVersion = lens _gtcmpgnvrsnVersion (\ s a -> s{_gtcmpgnvrsnVersion = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gtcmpgnvrsnApplicationId :: Lens' GetCampaignVersion Text
gtcmpgnvrsnApplicationId = lens _gtcmpgnvrsnApplicationId (\ s a -> s{_gtcmpgnvrsnApplicationId = a})

-- | The unique identifier for the campaign.
gtcmpgnvrsnCampaignId :: Lens' GetCampaignVersion Text
gtcmpgnvrsnCampaignId = lens _gtcmpgnvrsnCampaignId (\ s a -> s{_gtcmpgnvrsnCampaignId = a})

instance AWSRequest GetCampaignVersion where
        type Rs GetCampaignVersion =
             GetCampaignVersionResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetCampaignVersionResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetCampaignVersion where

instance NFData GetCampaignVersion where

instance ToHeaders GetCampaignVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCampaignVersion where
        toPath GetCampaignVersion'{..}
          = mconcat
              ["/v1/apps/", toBS _gtcmpgnvrsnApplicationId,
               "/campaigns/", toBS _gtcmpgnvrsnCampaignId,
               "/versions/", toBS _gtcmpgnvrsnVersion]

instance ToQuery GetCampaignVersion where
        toQuery = const mempty

-- | /See:/ 'getCampaignVersionResponse' smart constructor.
data GetCampaignVersionResponse = GetCampaignVersionResponse'{_gtcmpgnvrsnrsResponseStatus
                                                              :: !Int,
                                                              _gtcmpgnvrsnrsCampaignResponse
                                                              ::
                                                              !CampaignResponse}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'GetCampaignVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcmpgnvrsnrsResponseStatus' - -- | The response status code.
--
-- * 'gtcmpgnvrsnrsCampaignResponse' - Undocumented member.
getCampaignVersionResponse
    :: Int -- ^ 'gtcmpgnvrsnrsResponseStatus'
    -> CampaignResponse -- ^ 'gtcmpgnvrsnrsCampaignResponse'
    -> GetCampaignVersionResponse
getCampaignVersionResponse pResponseStatus_
  pCampaignResponse_
  = GetCampaignVersionResponse'{_gtcmpgnvrsnrsResponseStatus
                                  = pResponseStatus_,
                                _gtcmpgnvrsnrsCampaignResponse =
                                  pCampaignResponse_}

-- | -- | The response status code.
gtcmpgnvrsnrsResponseStatus :: Lens' GetCampaignVersionResponse Int
gtcmpgnvrsnrsResponseStatus = lens _gtcmpgnvrsnrsResponseStatus (\ s a -> s{_gtcmpgnvrsnrsResponseStatus = a})

-- | Undocumented member.
gtcmpgnvrsnrsCampaignResponse :: Lens' GetCampaignVersionResponse CampaignResponse
gtcmpgnvrsnrsCampaignResponse = lens _gtcmpgnvrsnrsCampaignResponse (\ s a -> s{_gtcmpgnvrsnrsCampaignResponse = a})

instance NFData GetCampaignVersionResponse where
