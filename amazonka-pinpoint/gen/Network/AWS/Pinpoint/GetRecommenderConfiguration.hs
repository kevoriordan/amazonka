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
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an Amazon Pinpoint configuration for a recommender model.
--
--
module Network.AWS.Pinpoint.GetRecommenderConfiguration
    (
    -- * Creating a Request
      getRecommenderConfiguration
    , GetRecommenderConfiguration
    -- * Request Lenses
    , grcRecommenderId

    -- * Destructuring the Response
    , getRecommenderConfigurationResponse
    , GetRecommenderConfigurationResponse
    -- * Response Lenses
    , gtrcmmndrcnfgrtnrsResponseStatus
    , gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRecommenderConfiguration' smart constructor.
newtype GetRecommenderConfiguration = GetRecommenderConfiguration'{_grcRecommenderId
                                                                   :: Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'GetRecommenderConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grcRecommenderId' - The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
getRecommenderConfiguration
    :: Text -- ^ 'grcRecommenderId'
    -> GetRecommenderConfiguration
getRecommenderConfiguration pRecommenderId_
  = GetRecommenderConfiguration'{_grcRecommenderId =
                                   pRecommenderId_}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
grcRecommenderId :: Lens' GetRecommenderConfiguration Text
grcRecommenderId = lens _grcRecommenderId (\ s a -> s{_grcRecommenderId = a})

instance AWSRequest GetRecommenderConfiguration where
        type Rs GetRecommenderConfiguration =
             GetRecommenderConfigurationResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetRecommenderConfigurationResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetRecommenderConfiguration where

instance NFData GetRecommenderConfiguration where

instance ToHeaders GetRecommenderConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetRecommenderConfiguration where
        toPath GetRecommenderConfiguration'{..}
          = mconcat
              ["/v1/recommenders/", toBS _grcRecommenderId]

instance ToQuery GetRecommenderConfiguration where
        toQuery = const mempty

-- | /See:/ 'getRecommenderConfigurationResponse' smart constructor.
data GetRecommenderConfigurationResponse = GetRecommenderConfigurationResponse'{_gtrcmmndrcnfgrtnrsResponseStatus
                                                                                ::
                                                                                !Int,
                                                                                _gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse
                                                                                ::
                                                                                !RecommenderConfigurationResponse}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'GetRecommenderConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrcmmndrcnfgrtnrsResponseStatus' - -- | The response status code.
--
-- * 'gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse' - Undocumented member.
getRecommenderConfigurationResponse
    :: Int -- ^ 'gtrcmmndrcnfgrtnrsResponseStatus'
    -> RecommenderConfigurationResponse -- ^ 'gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse'
    -> GetRecommenderConfigurationResponse
getRecommenderConfigurationResponse pResponseStatus_
  pRecommenderConfigurationResponse_
  = GetRecommenderConfigurationResponse'{_gtrcmmndrcnfgrtnrsResponseStatus
                                           = pResponseStatus_,
                                         _gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse
                                           = pRecommenderConfigurationResponse_}

-- | -- | The response status code.
gtrcmmndrcnfgrtnrsResponseStatus :: Lens' GetRecommenderConfigurationResponse Int
gtrcmmndrcnfgrtnrsResponseStatus = lens _gtrcmmndrcnfgrtnrsResponseStatus (\ s a -> s{_gtrcmmndrcnfgrtnrsResponseStatus = a})

-- | Undocumented member.
gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse :: Lens' GetRecommenderConfigurationResponse RecommenderConfigurationResponse
gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse = lens _gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse (\ s a -> s{_gtrcmmndrcnfgrtnrsRecommenderConfigurationResponse = a})

instance NFData GetRecommenderConfigurationResponse
         where
