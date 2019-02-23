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
-- Module      : Network.AWS.EC2.DescribePublicIPvfourPools
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified IPv4 address pools.
--
--
module Network.AWS.EC2.DescribePublicIPvfourPools
    (
    -- * Creating a Request
      describePublicIPvfourPools
    , DescribePublicIPvfourPools
    -- * Request Lenses
    , dpipPoolIds
    , dpipNextToken
    , dpipMaxResults

    -- * Destructuring the Response
    , describePublicIPvfourPoolsResponse
    , DescribePublicIPvfourPoolsResponse
    -- * Response Lenses
    , dpiprsNextToken
    , dpiprsPublicIPvfourPools
    , dpiprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePublicIPvfourPools' smart constructor.
data DescribePublicIPvfourPools = DescribePublicIPvfourPools'
  { _dpipPoolIds    :: !(Maybe [Text])
  , _dpipNextToken  :: !(Maybe Text)
  , _dpipMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePublicIPvfourPools' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpipPoolIds' - The IDs of the address pools.
--
-- * 'dpipNextToken' - The token for the next page of results.
--
-- * 'dpipMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describePublicIPvfourPools
    :: DescribePublicIPvfourPools
describePublicIPvfourPools =
  DescribePublicIPvfourPools'
    { _dpipPoolIds = Nothing
    , _dpipNextToken = Nothing
    , _dpipMaxResults = Nothing
    }


-- | The IDs of the address pools.
dpipPoolIds :: Lens' DescribePublicIPvfourPools [Text]
dpipPoolIds = lens _dpipPoolIds (\ s a -> s{_dpipPoolIds = a}) . _Default . _Coerce

-- | The token for the next page of results.
dpipNextToken :: Lens' DescribePublicIPvfourPools (Maybe Text)
dpipNextToken = lens _dpipNextToken (\ s a -> s{_dpipNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dpipMaxResults :: Lens' DescribePublicIPvfourPools (Maybe Natural)
dpipMaxResults = lens _dpipMaxResults (\ s a -> s{_dpipMaxResults = a}) . mapping _Nat

instance AWSRequest DescribePublicIPvfourPools where
        type Rs DescribePublicIPvfourPools =
             DescribePublicIPvfourPoolsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribePublicIPvfourPoolsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "publicIpv4PoolSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribePublicIPvfourPools where

instance NFData DescribePublicIPvfourPools where

instance ToHeaders DescribePublicIPvfourPools where
        toHeaders = const mempty

instance ToPath DescribePublicIPvfourPools where
        toPath = const "/"

instance ToQuery DescribePublicIPvfourPools where
        toQuery DescribePublicIPvfourPools'{..}
          = mconcat
              ["Action" =:
                 ("DescribePublicIpvfourPools" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "PoolId" <$> _dpipPoolIds),
               "NextToken" =: _dpipNextToken,
               "MaxResults" =: _dpipMaxResults]

-- | /See:/ 'describePublicIPvfourPoolsResponse' smart constructor.
data DescribePublicIPvfourPoolsResponse = DescribePublicIPvfourPoolsResponse'
  { _dpiprsNextToken          :: !(Maybe Text)
  , _dpiprsPublicIPvfourPools :: !(Maybe [PublicIPvfourPool])
  , _dpiprsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePublicIPvfourPoolsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpiprsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dpiprsPublicIPvfourPools' - Information about the address pools.
--
-- * 'dpiprsResponseStatus' - -- | The response status code.
describePublicIPvfourPoolsResponse
    :: Int -- ^ 'dpiprsResponseStatus'
    -> DescribePublicIPvfourPoolsResponse
describePublicIPvfourPoolsResponse pResponseStatus_ =
  DescribePublicIPvfourPoolsResponse'
    { _dpiprsNextToken = Nothing
    , _dpiprsPublicIPvfourPools = Nothing
    , _dpiprsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dpiprsNextToken :: Lens' DescribePublicIPvfourPoolsResponse (Maybe Text)
dpiprsNextToken = lens _dpiprsNextToken (\ s a -> s{_dpiprsNextToken = a})

-- | Information about the address pools.
dpiprsPublicIPvfourPools :: Lens' DescribePublicIPvfourPoolsResponse [PublicIPvfourPool]
dpiprsPublicIPvfourPools = lens _dpiprsPublicIPvfourPools (\ s a -> s{_dpiprsPublicIPvfourPools = a}) . _Default . _Coerce

-- | -- | The response status code.
dpiprsResponseStatus :: Lens' DescribePublicIPvfourPoolsResponse Int
dpiprsResponseStatus = lens _dpiprsResponseStatus (\ s a -> s{_dpiprsResponseStatus = a})

instance NFData DescribePublicIPvfourPoolsResponse
         where
