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
-- Module      : Network.AWS.Glue.GetCrawler
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a specified crawler.
--
--
module Network.AWS.Glue.GetCrawler
    (
    -- * Creating a Request
      getCrawler
    , GetCrawler
    -- * Request Lenses
    , gtcrwlrName

    -- * Destructuring the Response
    , getCrawlerResponse
    , GetCrawlerResponse
    -- * Response Lenses
    , gtcrwlrrsCrawler
    , gtcrwlrrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCrawler' smart constructor.
newtype GetCrawler = GetCrawler'{_gtcrwlrName ::
                                 Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCrawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcrwlrName' - The name of the crawler to retrieve metadata for.
getCrawler
    :: Text -- ^ 'gtcrwlrName'
    -> GetCrawler
getCrawler pName_
  = GetCrawler'{_gtcrwlrName = pName_}

-- | The name of the crawler to retrieve metadata for.
gtcrwlrName :: Lens' GetCrawler Text
gtcrwlrName = lens _gtcrwlrName (\ s a -> s{_gtcrwlrName = a})

instance AWSRequest GetCrawler where
        type Rs GetCrawler = GetCrawlerResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetCrawlerResponse' <$>
                   (x .?> "Crawler") <*> (pure (fromEnum s)))

instance Hashable GetCrawler where

instance NFData GetCrawler where

instance ToHeaders GetCrawler where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetCrawler" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCrawler where
        toJSON GetCrawler'{..}
          = object (catMaybes [Just ("Name" .= _gtcrwlrName)])

instance ToPath GetCrawler where
        toPath = const "/"

instance ToQuery GetCrawler where
        toQuery = const mempty

-- | /See:/ 'getCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'{_gtcrwlrrsCrawler
                                              :: !(Maybe Crawler),
                                              _gtcrwlrrsResponseStatus :: !Int}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCrawlerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcrwlrrsCrawler' - The metadata for the specified crawler.
--
-- * 'gtcrwlrrsResponseStatus' - -- | The response status code.
getCrawlerResponse
    :: Int -- ^ 'gtcrwlrrsResponseStatus'
    -> GetCrawlerResponse
getCrawlerResponse pResponseStatus_
  = GetCrawlerResponse'{_gtcrwlrrsCrawler = Nothing,
                        _gtcrwlrrsResponseStatus = pResponseStatus_}

-- | The metadata for the specified crawler.
gtcrwlrrsCrawler :: Lens' GetCrawlerResponse (Maybe Crawler)
gtcrwlrrsCrawler = lens _gtcrwlrrsCrawler (\ s a -> s{_gtcrwlrrsCrawler = a})

-- | -- | The response status code.
gtcrwlrrsResponseStatus :: Lens' GetCrawlerResponse Int
gtcrwlrrsResponseStatus = lens _gtcrwlrrsResponseStatus (\ s a -> s{_gtcrwlrrsResponseStatus = a})

instance NFData GetCrawlerResponse where
