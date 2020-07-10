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
-- Module      : Network.AWS.Glue.GetDevEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified development endpoint.
--
--
module Network.AWS.Glue.GetDevEndpoint
    (
    -- * Creating a Request
      getDevEndpoint
    , GetDevEndpoint
    -- * Request Lenses
    , gdeEndpointName

    -- * Destructuring the Response
    , getDevEndpointResponse
    , GetDevEndpointResponse
    -- * Response Lenses
    , gtdvendpntrsDevEndpoint
    , gtdvendpntrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDevEndpoint' smart constructor.
newtype GetDevEndpoint = GetDevEndpoint'{_gdeEndpointName
                                         :: Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdeEndpointName' - Name of the @DevEndpoint@ to retrieve information for.
getDevEndpoint
    :: Text -- ^ 'gdeEndpointName'
    -> GetDevEndpoint
getDevEndpoint pEndpointName_
  = GetDevEndpoint'{_gdeEndpointName = pEndpointName_}

-- | Name of the @DevEndpoint@ to retrieve information for.
gdeEndpointName :: Lens' GetDevEndpoint Text
gdeEndpointName = lens _gdeEndpointName (\ s a -> s{_gdeEndpointName = a})

instance AWSRequest GetDevEndpoint where
        type Rs GetDevEndpoint = GetDevEndpointResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDevEndpointResponse' <$>
                   (x .?> "DevEndpoint") <*> (pure (fromEnum s)))

instance Hashable GetDevEndpoint where

instance NFData GetDevEndpoint where

instance ToHeaders GetDevEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDevEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevEndpoint where
        toJSON GetDevEndpoint'{..}
          = object
              (catMaybes
                 [Just ("EndpointName" .= _gdeEndpointName)])

instance ToPath GetDevEndpoint where
        toPath = const "/"

instance ToQuery GetDevEndpoint where
        toQuery = const mempty

-- | /See:/ 'getDevEndpointResponse' smart constructor.
data GetDevEndpointResponse = GetDevEndpointResponse'{_gtdvendpntrsDevEndpoint
                                                      :: !(Maybe DevEndpoint),
                                                      _gtdvendpntrsResponseStatus
                                                      :: !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'GetDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdvendpntrsDevEndpoint' - A @DevEndpoint@ definition.
--
-- * 'gtdvendpntrsResponseStatus' - -- | The response status code.
getDevEndpointResponse
    :: Int -- ^ 'gtdvendpntrsResponseStatus'
    -> GetDevEndpointResponse
getDevEndpointResponse pResponseStatus_
  = GetDevEndpointResponse'{_gtdvendpntrsDevEndpoint =
                              Nothing,
                            _gtdvendpntrsResponseStatus = pResponseStatus_}

-- | A @DevEndpoint@ definition.
gtdvendpntrsDevEndpoint :: Lens' GetDevEndpointResponse (Maybe DevEndpoint)
gtdvendpntrsDevEndpoint = lens _gtdvendpntrsDevEndpoint (\ s a -> s{_gtdvendpntrsDevEndpoint = a})

-- | -- | The response status code.
gtdvendpntrsResponseStatus :: Lens' GetDevEndpointResponse Int
gtdvendpntrsResponseStatus = lens _gtdvendpntrsResponseStatus (\ s a -> s{_gtdvendpntrsResponseStatus = a})

instance NFData GetDevEndpointResponse where
