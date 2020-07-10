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
-- Module      : Network.AWS.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.DescribeSchedule
    (
    -- * Creating a Request
      describeSchedule
    , DescribeSchedule
    -- * Request Lenses
    , dNextToken
    , dMaxResults
    , dChannelId

    -- * Destructuring the Response
    , describeScheduleResponse
    , DescribeScheduleResponse
    -- * Response Lenses
    , dscrbschdlrsNextToken
    , dscrbschdlrsScheduleActions
    , dscrbschdlrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'describeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'{_dNextToken
                                          :: !(Maybe Text),
                                          _dMaxResults :: !(Maybe Nat),
                                          _dChannelId :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken' - Undocumented member.
--
-- * 'dMaxResults' - Undocumented member.
--
-- * 'dChannelId' - Id of the channel whose schedule is being updated.
describeSchedule
    :: Text -- ^ 'dChannelId'
    -> DescribeSchedule
describeSchedule pChannelId_
  = DescribeSchedule'{_dNextToken = Nothing,
                      _dMaxResults = Nothing, _dChannelId = pChannelId_}

-- | Undocumented member.
dNextToken :: Lens' DescribeSchedule (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | Undocumented member.
dMaxResults :: Lens' DescribeSchedule (Maybe Natural)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a}) . mapping _Nat

-- | Id of the channel whose schedule is being updated.
dChannelId :: Lens' DescribeSchedule Text
dChannelId = lens _dChannelId (\ s a -> s{_dChannelId = a})

instance AWSPager DescribeSchedule where
        page rq rs
          | stop (rs ^. dscrbschdlrsNextToken) = Nothing
          | stop (rs ^. dscrbschdlrsScheduleActions) = Nothing
          | otherwise =
            Just $ rq & dNextToken .~ rs ^. dscrbschdlrsNextToken

instance AWSRequest DescribeSchedule where
        type Rs DescribeSchedule = DescribeScheduleResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeScheduleResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "scheduleActions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSchedule where

instance NFData DescribeSchedule where

instance ToHeaders DescribeSchedule where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeSchedule where
        toPath DescribeSchedule'{..}
          = mconcat
              ["/prod/channels/", toBS _dChannelId, "/schedule"]

instance ToQuery DescribeSchedule where
        toQuery DescribeSchedule'{..}
          = mconcat
              ["nextToken" =: _dNextToken,
               "maxResults" =: _dMaxResults]

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'describeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'{_dscrbschdlrsNextToken
                                                          :: !(Maybe Text),
                                                          _dscrbschdlrsScheduleActions
                                                          ::
                                                          !(Maybe
                                                              [ScheduleAction]),
                                                          _dscrbschdlrsResponseStatus
                                                          :: !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DescribeScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbschdlrsNextToken' - The next token; for use in pagination.
--
-- * 'dscrbschdlrsScheduleActions' - The list of actions in the schedule.
--
-- * 'dscrbschdlrsResponseStatus' - -- | The response status code.
describeScheduleResponse
    :: Int -- ^ 'dscrbschdlrsResponseStatus'
    -> DescribeScheduleResponse
describeScheduleResponse pResponseStatus_
  = DescribeScheduleResponse'{_dscrbschdlrsNextToken =
                                Nothing,
                              _dscrbschdlrsScheduleActions = Nothing,
                              _dscrbschdlrsResponseStatus = pResponseStatus_}

-- | The next token; for use in pagination.
dscrbschdlrsNextToken :: Lens' DescribeScheduleResponse (Maybe Text)
dscrbschdlrsNextToken = lens _dscrbschdlrsNextToken (\ s a -> s{_dscrbschdlrsNextToken = a})

-- | The list of actions in the schedule.
dscrbschdlrsScheduleActions :: Lens' DescribeScheduleResponse [ScheduleAction]
dscrbschdlrsScheduleActions = lens _dscrbschdlrsScheduleActions (\ s a -> s{_dscrbschdlrsScheduleActions = a}) . _Default . _Coerce

-- | -- | The response status code.
dscrbschdlrsResponseStatus :: Lens' DescribeScheduleResponse Int
dscrbschdlrsResponseStatus = lens _dscrbschdlrsResponseStatus (\ s a -> s{_dscrbschdlrsResponseStatus = a})

instance NFData DescribeScheduleResponse where
