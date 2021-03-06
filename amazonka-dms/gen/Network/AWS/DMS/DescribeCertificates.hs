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
-- Module      : Network.AWS.DMS.DescribeCertificates
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a description of the certificate.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeCertificates
    (
    -- * Creating a Request
      describeCertificates
    , DescribeCertificates
    -- * Request Lenses
    , dFilters
    , dMarker
    , dMaxRecords

    -- * Destructuring the Response
    , describeCertificatesResponse
    , DescribeCertificatesResponse
    -- * Response Lenses
    , drsCertificates
    , drsMarker
    , drsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCertificates' smart constructor.
data DescribeCertificates = DescribeCertificates'{_dFilters
                                                  :: !(Maybe [Filter]),
                                                  _dMarker :: !(Maybe Text),
                                                  _dMaxRecords :: !(Maybe Int)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dFilters' - Filters applied to the certificate described in the form of key-value pairs.
--
-- * 'dMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
--
-- * 'dMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 10
describeCertificates
    :: DescribeCertificates
describeCertificates
  = DescribeCertificates'{_dFilters = Nothing,
                          _dMarker = Nothing, _dMaxRecords = Nothing}

-- | Filters applied to the certificate described in the form of key-value pairs.
dFilters :: Lens' DescribeCertificates [Filter]
dFilters = lens _dFilters (\ s a -> s{_dFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ . 
dMarker :: Lens' DescribeCertificates (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 10
dMaxRecords :: Lens' DescribeCertificates (Maybe Int)
dMaxRecords = lens _dMaxRecords (\ s a -> s{_dMaxRecords = a})

instance AWSPager DescribeCertificates where
        page rq rs
          | stop (rs ^. drsMarker) = Nothing
          | stop (rs ^. drsCertificates) = Nothing
          | otherwise = Just $ rq & dMarker .~ rs ^. drsMarker

instance AWSRequest DescribeCertificates where
        type Rs DescribeCertificates =
             DescribeCertificatesResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCertificatesResponse' <$>
                   (x .?> "Certificates" .!@ mempty) <*>
                     (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCertificates where

instance NFData DescribeCertificates where

instance ToHeaders DescribeCertificates where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeCertificates" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCertificates where
        toJSON DescribeCertificates'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dFilters,
                  ("Marker" .=) <$> _dMarker,
                  ("MaxRecords" .=) <$> _dMaxRecords])

instance ToPath DescribeCertificates where
        toPath = const "/"

instance ToQuery DescribeCertificates where
        toQuery = const mempty

-- | /See:/ 'describeCertificatesResponse' smart constructor.
data DescribeCertificatesResponse = DescribeCertificatesResponse'{_drsCertificates
                                                                  ::
                                                                  !(Maybe
                                                                      [Certificate]),
                                                                  _drsMarker ::
                                                                  !(Maybe Text),
                                                                  _drsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCertificates' - The Secure Sockets Layer (SSL) certificates associated with the replication instance.
--
-- * 'drsMarker' - The pagination token.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeCertificatesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeCertificatesResponse
describeCertificatesResponse pResponseStatus_
  = DescribeCertificatesResponse'{_drsCertificates =
                                    Nothing,
                                  _drsMarker = Nothing,
                                  _drsResponseStatus = pResponseStatus_}

-- | The Secure Sockets Layer (SSL) certificates associated with the replication instance.
drsCertificates :: Lens' DescribeCertificatesResponse [Certificate]
drsCertificates = lens _drsCertificates (\ s a -> s{_drsCertificates = a}) . _Default . _Coerce

-- | The pagination token.
drsMarker :: Lens' DescribeCertificatesResponse (Maybe Text)
drsMarker = lens _drsMarker (\ s a -> s{_drsMarker = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeCertificatesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeCertificatesResponse where
