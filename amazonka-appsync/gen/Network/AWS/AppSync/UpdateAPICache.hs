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
-- Module      : Network.AWS.AppSync.UpdateAPICache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cache for the GraphQL API.
--
--
module Network.AWS.AppSync.UpdateAPICache
    (
    -- * Creating a Request
      updateAPICache
    , UpdateAPICache
    -- * Request Lenses
    , uacApiId
    , uacTtl
    , uacApiCachingBehavior
    , uacType

    -- * Destructuring the Response
    , updateAPICacheResponse
    , UpdateAPICacheResponse
    -- * Response Lenses
    , uacrsApiCache
    , uacrsResponseStatus
    ) where

import Network.AWS.AppSync.Types
import Network.AWS.AppSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @UpdateApiCache@ operation.
--
--
--
-- /See:/ 'updateAPICache' smart constructor.
data UpdateAPICache = UpdateAPICache'{_uacApiId ::
                                      !Text,
                                      _uacTtl :: !Integer,
                                      _uacApiCachingBehavior ::
                                      !APICachingBehavior,
                                      _uacType :: !APICacheType}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAPICache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uacApiId' - The GraphQL API Id.
--
-- * 'uacTtl' - TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
--
-- * 'uacApiCachingBehavior' - Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resovlers that you specify are cached.
--
-- * 'uacType' - The cache instance type.     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
updateAPICache
    :: Text -- ^ 'uacApiId'
    -> Integer -- ^ 'uacTtl'
    -> APICachingBehavior -- ^ 'uacApiCachingBehavior'
    -> APICacheType -- ^ 'uacType'
    -> UpdateAPICache
updateAPICache pApiId_ pTtl_ pApiCachingBehavior_
  pType_
  = UpdateAPICache'{_uacApiId = pApiId_,
                    _uacTtl = pTtl_,
                    _uacApiCachingBehavior = pApiCachingBehavior_,
                    _uacType = pType_}

-- | The GraphQL API Id.
uacApiId :: Lens' UpdateAPICache Text
uacApiId = lens _uacApiId (\ s a -> s{_uacApiId = a})

-- | TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
uacTtl :: Lens' UpdateAPICache Integer
uacTtl = lens _uacTtl (\ s a -> s{_uacTtl = a})

-- | Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resovlers that you specify are cached.
uacApiCachingBehavior :: Lens' UpdateAPICache APICachingBehavior
uacApiCachingBehavior = lens _uacApiCachingBehavior (\ s a -> s{_uacApiCachingBehavior = a})

-- | The cache instance type.     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
uacType :: Lens' UpdateAPICache APICacheType
uacType = lens _uacType (\ s a -> s{_uacType = a})

instance AWSRequest UpdateAPICache where
        type Rs UpdateAPICache = UpdateAPICacheResponse
        request = postJSON appSync
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPICacheResponse' <$>
                   (x .?> "apiCache") <*> (pure (fromEnum s)))

instance Hashable UpdateAPICache where

instance NFData UpdateAPICache where

instance ToHeaders UpdateAPICache where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPICache where
        toJSON UpdateAPICache'{..}
          = object
              (catMaybes
                 [Just ("ttl" .= _uacTtl),
                  Just
                    ("apiCachingBehavior" .= _uacApiCachingBehavior),
                  Just ("type" .= _uacType)])

instance ToPath UpdateAPICache where
        toPath UpdateAPICache'{..}
          = mconcat
              ["/v1/apis/", toBS _uacApiId, "/ApiCaches/update"]

instance ToQuery UpdateAPICache where
        toQuery = const mempty

-- | Represents the output of a @UpdateApiCache@ operation.
--
--
--
-- /See:/ 'updateAPICacheResponse' smart constructor.
data UpdateAPICacheResponse = UpdateAPICacheResponse'{_uacrsApiCache
                                                      :: !(Maybe APICache),
                                                      _uacrsResponseStatus ::
                                                      !Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'UpdateAPICacheResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uacrsApiCache' - The @ApiCache@ object.
--
-- * 'uacrsResponseStatus' - -- | The response status code.
updateAPICacheResponse
    :: Int -- ^ 'uacrsResponseStatus'
    -> UpdateAPICacheResponse
updateAPICacheResponse pResponseStatus_
  = UpdateAPICacheResponse'{_uacrsApiCache = Nothing,
                            _uacrsResponseStatus = pResponseStatus_}

-- | The @ApiCache@ object.
uacrsApiCache :: Lens' UpdateAPICacheResponse (Maybe APICache)
uacrsApiCache = lens _uacrsApiCache (\ s a -> s{_uacrsApiCache = a})

-- | -- | The response status code.
uacrsResponseStatus :: Lens' UpdateAPICacheResponse Int
uacrsResponseStatus = lens _uacrsResponseStatus (\ s a -> s{_uacrsResponseStatus = a})

instance NFData UpdateAPICacheResponse where
