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
-- Module      : Network.AWS.Lightsail.GetDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDomains
    (
    -- * Creating a Request
      getDomains
    , GetDomains
    -- * Request Lenses
    , gdPageToken

    -- * Destructuring the Response
    , getDomainsResponse
    , GetDomainsResponse
    -- * Response Lenses
    , gtdmnsrsNextPageToken
    , gtdmnsrsDomains
    , gtdmnsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDomains' smart constructor.
newtype GetDomains = GetDomains'{_gdPageToken ::
                                 Maybe Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getDomains
    :: GetDomains
getDomains = GetDomains'{_gdPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gdPageToken :: Lens' GetDomains (Maybe Text)
gdPageToken = lens _gdPageToken (\ s a -> s{_gdPageToken = a})

instance AWSPager GetDomains where
        page rq rs
          | stop (rs ^. gtdmnsrsNextPageToken) = Nothing
          | stop (rs ^. gtdmnsrsDomains) = Nothing
          | otherwise =
            Just $ rq &
              gdPageToken .~ rs ^. gtdmnsrsNextPageToken

instance AWSRequest GetDomains where
        type Rs GetDomains = GetDomainsResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetDomainsResponse' <$>
                   (x .?> "nextPageToken") <*>
                     (x .?> "domains" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDomains where

instance NFData GetDomains where

instance ToHeaders GetDomains where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetDomains" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDomains where
        toJSON GetDomains'{..}
          = object
              (catMaybes [("pageToken" .=) <$> _gdPageToken])

instance ToPath GetDomains where
        toPath = const "/"

instance ToQuery GetDomains where
        toQuery = const mempty

-- | /See:/ 'getDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'{_gtdmnsrsNextPageToken
                                              :: !(Maybe Text),
                                              _gtdmnsrsDomains ::
                                              !(Maybe [Domain]),
                                              _gtdmnsrsResponseStatus :: !Int}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtdmnsrsNextPageToken' - The token to advance to the next page of resutls from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'gtdmnsrsDomains' - An array of key-value pairs containing information about each of the domain entries in the user's account.
--
-- * 'gtdmnsrsResponseStatus' - -- | The response status code.
getDomainsResponse
    :: Int -- ^ 'gtdmnsrsResponseStatus'
    -> GetDomainsResponse
getDomainsResponse pResponseStatus_
  = GetDomainsResponse'{_gtdmnsrsNextPageToken =
                          Nothing,
                        _gtdmnsrsDomains = Nothing,
                        _gtdmnsrsResponseStatus = pResponseStatus_}

-- | The token to advance to the next page of resutls from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
gtdmnsrsNextPageToken :: Lens' GetDomainsResponse (Maybe Text)
gtdmnsrsNextPageToken = lens _gtdmnsrsNextPageToken (\ s a -> s{_gtdmnsrsNextPageToken = a})

-- | An array of key-value pairs containing information about each of the domain entries in the user's account.
gtdmnsrsDomains :: Lens' GetDomainsResponse [Domain]
gtdmnsrsDomains = lens _gtdmnsrsDomains (\ s a -> s{_gtdmnsrsDomains = a}) . _Default . _Coerce

-- | -- | The response status code.
gtdmnsrsResponseStatus :: Lens' GetDomainsResponse Int
gtdmnsrsResponseStatus = lens _gtdmnsrsResponseStatus (\ s a -> s{_gtdmnsrsResponseStatus = a})

instance NFData GetDomainsResponse where
