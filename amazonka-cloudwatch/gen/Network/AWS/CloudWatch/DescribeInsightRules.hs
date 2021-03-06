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
-- Module      : Network.AWS.CloudWatch.DescribeInsightRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the Contributor Insights rules in your account. All rules in your account are returned with a single operation.
--
--
-- For more information about Contributor Insights, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data> .
--
module Network.AWS.CloudWatch.DescribeInsightRules
    (
    -- * Creating a Request
      describeInsightRules
    , DescribeInsightRules
    -- * Request Lenses
    , dirNextToken
    , dirMaxResults

    -- * Destructuring the Response
    , describeInsightRulesResponse
    , DescribeInsightRulesResponse
    -- * Response Lenses
    , drsNextToken
    , drsInsightRules
    , drsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeInsightRules' smart constructor.
data DescribeInsightRules = DescribeInsightRules'{_dirNextToken
                                                  :: !(Maybe Text),
                                                  _dirMaxResults ::
                                                  !(Maybe Nat)}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeInsightRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirNextToken' - Reserved for future use.
--
-- * 'dirMaxResults' - This parameter is not currently used. Reserved for future use. If it is used in the future, the maximum value may be different.
describeInsightRules
    :: DescribeInsightRules
describeInsightRules
  = DescribeInsightRules'{_dirNextToken = Nothing,
                          _dirMaxResults = Nothing}

-- | Reserved for future use.
dirNextToken :: Lens' DescribeInsightRules (Maybe Text)
dirNextToken = lens _dirNextToken (\ s a -> s{_dirNextToken = a})

-- | This parameter is not currently used. Reserved for future use. If it is used in the future, the maximum value may be different.
dirMaxResults :: Lens' DescribeInsightRules (Maybe Natural)
dirMaxResults = lens _dirMaxResults (\ s a -> s{_dirMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeInsightRules where
        type Rs DescribeInsightRules =
             DescribeInsightRulesResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "DescribeInsightRulesResult"
              (\ s h x ->
                 DescribeInsightRulesResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "InsightRules" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInsightRules where

instance NFData DescribeInsightRules where

instance ToHeaders DescribeInsightRules where
        toHeaders = const mempty

instance ToPath DescribeInsightRules where
        toPath = const "/"

instance ToQuery DescribeInsightRules where
        toQuery DescribeInsightRules'{..}
          = mconcat
              ["Action" =: ("DescribeInsightRules" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "NextToken" =: _dirNextToken,
               "MaxResults" =: _dirMaxResults]

-- | /See:/ 'describeInsightRulesResponse' smart constructor.
data DescribeInsightRulesResponse = DescribeInsightRulesResponse'{_drsNextToken
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _drsInsightRules
                                                                  ::
                                                                  !(Maybe
                                                                      [InsightRule]),
                                                                  _drsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'DescribeInsightRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsNextToken' - Reserved for future use.
--
-- * 'drsInsightRules' - The rules returned by the operation.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeInsightRulesResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeInsightRulesResponse
describeInsightRulesResponse pResponseStatus_
  = DescribeInsightRulesResponse'{_drsNextToken =
                                    Nothing,
                                  _drsInsightRules = Nothing,
                                  _drsResponseStatus = pResponseStatus_}

-- | Reserved for future use.
drsNextToken :: Lens' DescribeInsightRulesResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | The rules returned by the operation.
drsInsightRules :: Lens' DescribeInsightRulesResponse [InsightRule]
drsInsightRules = lens _drsInsightRules (\ s a -> s{_drsInsightRules = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeInsightRulesResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeInsightRulesResponse where
