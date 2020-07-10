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
-- Module      : Network.AWS.Organizations.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for the specified resource. 
--
--
-- Currently, you can list tags on an account in AWS Organizations.
--
-- This operation can be called only from the organization's master account or by a member account that is a delegated administrator for an AWS service.
--
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrNextToken
    , ltfrResourceId

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsNextToken
    , ltfrrsTags
    , ltfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'{_ltfrNextToken
                                                :: !(Maybe Text),
                                                _ltfrResourceId :: !Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'ltfrResourceId' - The ID of the resource that you want to retrieve tags for. 
listTagsForResource
    :: Text -- ^ 'ltfrResourceId'
    -> ListTagsForResource
listTagsForResource pResourceId_
  = ListTagsForResource'{_ltfrNextToken = Nothing,
                         _ltfrResourceId = pResourceId_}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
ltfrNextToken :: Lens' ListTagsForResource (Maybe Text)
ltfrNextToken = lens _ltfrNextToken (\ s a -> s{_ltfrNextToken = a})

-- | The ID of the resource that you want to retrieve tags for. 
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\ s a -> s{_ltfrResourceId = a})

instance AWSPager ListTagsForResource where
        page rq rs
          | stop (rs ^. ltfrrsNextToken) = Nothing
          | stop (rs ^. ltfrrsTags) = Nothing
          | otherwise =
            Just $ rq & ltfrNextToken .~ rs ^. ltfrrsNextToken

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.ListTagsForResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForResource where
        toJSON ListTagsForResource'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltfrNextToken,
                  Just ("ResourceId" .= _ltfrResourceId)])

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'{_ltfrrsNextToken
                                                                ::
                                                                !(Maybe Text),
                                                                _ltfrrsTags ::
                                                                !(Maybe [Tag]),
                                                                _ltfrrsResponseStatus
                                                                :: !Int}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'ltfrrsTags' - The tags that are assigned to the resource.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_
  = ListTagsForResourceResponse'{_ltfrrsNextToken =
                                   Nothing,
                                 _ltfrrsTags = Nothing,
                                 _ltfrrsResponseStatus = pResponseStatus_}

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
ltfrrsNextToken :: Lens' ListTagsForResourceResponse (Maybe Text)
ltfrrsNextToken = lens _ltfrrsNextToken (\ s a -> s{_ltfrrsNextToken = a})

-- | The tags that are assigned to the resource.
ltfrrsTags :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTags = lens _ltfrrsTags (\ s a -> s{_ltfrrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse where
