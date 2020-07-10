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
-- Module      : Network.AWS.Cloud9.DescribeEnvironments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS Cloud9 development environments.
--
--
module Network.AWS.Cloud9.DescribeEnvironments
    (
    -- * Creating a Request
      describeEnvironments
    , DescribeEnvironments
    -- * Request Lenses
    , deEnvironmentIds

    -- * Destructuring the Response
    , describeEnvironmentsResponse
    , DescribeEnvironmentsResponse
    -- * Response Lenses
    , dscrbenvrnmntsrsEnvironments
    , dscrbenvrnmntsrsResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEnvironments' smart constructor.
newtype DescribeEnvironments = DescribeEnvironments'{_deEnvironmentIds
                                                     :: List1 Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeEnvironments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deEnvironmentIds' - The IDs of individual environments to get information about.
describeEnvironments
    :: NonEmpty Text -- ^ 'deEnvironmentIds'
    -> DescribeEnvironments
describeEnvironments pEnvironmentIds_
  = DescribeEnvironments'{_deEnvironmentIds =
                            _List1 # pEnvironmentIds_}

-- | The IDs of individual environments to get information about.
deEnvironmentIds :: Lens' DescribeEnvironments (NonEmpty Text)
deEnvironmentIds = lens _deEnvironmentIds (\ s a -> s{_deEnvironmentIds = a}) . _List1

instance AWSRequest DescribeEnvironments where
        type Rs DescribeEnvironments =
             DescribeEnvironmentsResponse
        request = postJSON cloud9
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEnvironmentsResponse' <$>
                   (x .?> "environments" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeEnvironments where

instance NFData DescribeEnvironments where

instance ToHeaders DescribeEnvironments where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.DescribeEnvironments"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEnvironments where
        toJSON DescribeEnvironments'{..}
          = object
              (catMaybes
                 [Just ("environmentIds" .= _deEnvironmentIds)])

instance ToPath DescribeEnvironments where
        toPath = const "/"

instance ToQuery DescribeEnvironments where
        toQuery = const mempty

-- | /See:/ 'describeEnvironmentsResponse' smart constructor.
data DescribeEnvironmentsResponse = DescribeEnvironmentsResponse'{_dscrbenvrnmntsrsEnvironments
                                                                  ::
                                                                  !(Maybe
                                                                      [Environment]),
                                                                  _dscrbenvrnmntsrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeEnvironmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbenvrnmntsrsEnvironments' - Information about the environments that are returned.
--
-- * 'dscrbenvrnmntsrsResponseStatus' - -- | The response status code.
describeEnvironmentsResponse
    :: Int -- ^ 'dscrbenvrnmntsrsResponseStatus'
    -> DescribeEnvironmentsResponse
describeEnvironmentsResponse pResponseStatus_
  = DescribeEnvironmentsResponse'{_dscrbenvrnmntsrsEnvironments
                                    = Nothing,
                                  _dscrbenvrnmntsrsResponseStatus =
                                    pResponseStatus_}

-- | Information about the environments that are returned.
dscrbenvrnmntsrsEnvironments :: Lens' DescribeEnvironmentsResponse [Environment]
dscrbenvrnmntsrsEnvironments = lens _dscrbenvrnmntsrsEnvironments (\ s a -> s{_dscrbenvrnmntsrsEnvironments = a}) . _Default . _Coerce

-- | -- | The response status code.
dscrbenvrnmntsrsResponseStatus :: Lens' DescribeEnvironmentsResponse Int
dscrbenvrnmntsrsResponseStatus = lens _dscrbenvrnmntsrsResponseStatus (\ s a -> s{_dscrbenvrnmntsrsResponseStatus = a})

instance NFData DescribeEnvironmentsResponse where
