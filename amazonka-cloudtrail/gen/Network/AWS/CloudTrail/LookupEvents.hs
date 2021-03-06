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
-- Module      : Network.AWS.CloudTrail.LookupEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Looks up <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-management-events management events> or <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-concepts.html#cloudtrail-concepts-insights-events CloudTrail Insights events> that are captured by CloudTrail. You can look up events that occurred in a region within the last 90 days. Lookup supports the following attributes for management events:
--
--
--     * AWS access key
--
--     * Event ID
--
--     * Event name
--
--     * Event source
--
--     * Read only
--
--     * Resource name
--
--     * Resource type
--
--     * User name
--
--
--
-- Lookup supports the following attributes for Insights events:
--
--     * Event ID
--
--     * Event name
--
--     * Event source
--
--
--
-- All attributes are optional. The default number of results returned is 50, with a maximum of 50 possible. The response includes a token that you can use to get the next page of results.
--
-- /Important:/ The rate of lookup requests is limited to two per second per account. If this limit is exceeded, a throttling error occurs.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.LookupEvents
    (
    -- * Creating a Request
      lookupEvents
    , LookupEvents
    -- * Request Lenses
    , leEventCategory
    , leStartTime
    , leLookupAttributes
    , leNextToken
    , leEndTime
    , leMaxResults

    -- * Destructuring the Response
    , lookupEventsResponse
    , LookupEventsResponse
    -- * Response Lenses
    , lersNextToken
    , lersEvents
    , lersResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains a request for LookupEvents.
--
--
--
-- /See:/ 'lookupEvents' smart constructor.
data LookupEvents = LookupEvents'{_leEventCategory ::
                                  !(Maybe EventCategory),
                                  _leStartTime :: !(Maybe POSIX),
                                  _leLookupAttributes ::
                                  !(Maybe [LookupAttribute]),
                                  _leNextToken :: !(Maybe Text),
                                  _leEndTime :: !(Maybe POSIX),
                                  _leMaxResults :: !(Maybe Nat)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LookupEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leEventCategory' - Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
--
-- * 'leStartTime' - Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
--
-- * 'leLookupAttributes' - Contains a list of lookup attributes. Currently the list can contain only one item.
--
-- * 'leNextToken' - The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- * 'leEndTime' - Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
--
-- * 'leMaxResults' - The number of events to return. Possible values are 1 through 50. The default is 50.
lookupEvents
    :: LookupEvents
lookupEvents
  = LookupEvents'{_leEventCategory = Nothing,
                  _leStartTime = Nothing,
                  _leLookupAttributes = Nothing,
                  _leNextToken = Nothing, _leEndTime = Nothing,
                  _leMaxResults = Nothing}

-- | Specifies the event category. If you do not specify an event category, events of the category are not returned in the response. For example, if you do not specify @insight@ as the value of @EventCategory@ , no Insights events are returned.
leEventCategory :: Lens' LookupEvents (Maybe EventCategory)
leEventCategory = lens _leEventCategory (\ s a -> s{_leEventCategory = a})

-- | Specifies that only events that occur after or at the specified time are returned. If the specified start time is after the specified end time, an error is returned.
leStartTime :: Lens' LookupEvents (Maybe UTCTime)
leStartTime = lens _leStartTime (\ s a -> s{_leStartTime = a}) . mapping _Time

-- | Contains a list of lookup attributes. Currently the list can contain only one item.
leLookupAttributes :: Lens' LookupEvents [LookupAttribute]
leLookupAttributes = lens _leLookupAttributes (\ s a -> s{_leLookupAttributes = a}) . _Default . _Coerce

-- | The token to use to get the next page of results after a previous API call. This token must be passed in with the same parameters that were specified in the the original call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
leNextToken :: Lens' LookupEvents (Maybe Text)
leNextToken = lens _leNextToken (\ s a -> s{_leNextToken = a})

-- | Specifies that only events that occur before or at the specified time are returned. If the specified end time is before the specified start time, an error is returned.
leEndTime :: Lens' LookupEvents (Maybe UTCTime)
leEndTime = lens _leEndTime (\ s a -> s{_leEndTime = a}) . mapping _Time

-- | The number of events to return. Possible values are 1 through 50. The default is 50.
leMaxResults :: Lens' LookupEvents (Maybe Natural)
leMaxResults = lens _leMaxResults (\ s a -> s{_leMaxResults = a}) . mapping _Nat

instance AWSPager LookupEvents where
        page rq rs
          | stop (rs ^. lersNextToken) = Nothing
          | stop (rs ^. lersEvents) = Nothing
          | otherwise =
            Just $ rq & leNextToken .~ rs ^. lersNextToken

instance AWSRequest LookupEvents where
        type Rs LookupEvents = LookupEventsResponse
        request = postJSON cloudTrail
        response
          = receiveJSON
              (\ s h x ->
                 LookupEventsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Events" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable LookupEvents where

instance NFData LookupEvents where

instance ToHeaders LookupEvents where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.LookupEvents"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LookupEvents where
        toJSON LookupEvents'{..}
          = object
              (catMaybes
                 [("EventCategory" .=) <$> _leEventCategory,
                  ("StartTime" .=) <$> _leStartTime,
                  ("LookupAttributes" .=) <$> _leLookupAttributes,
                  ("NextToken" .=) <$> _leNextToken,
                  ("EndTime" .=) <$> _leEndTime,
                  ("MaxResults" .=) <$> _leMaxResults])

instance ToPath LookupEvents where
        toPath = const "/"

instance ToQuery LookupEvents where
        toQuery = const mempty

-- | Contains a response to a LookupEvents action.
--
--
--
-- /See:/ 'lookupEventsResponse' smart constructor.
data LookupEventsResponse = LookupEventsResponse'{_lersNextToken
                                                  :: !(Maybe Text),
                                                  _lersEvents ::
                                                  !(Maybe [Event]),
                                                  _lersResponseStatus :: !Int}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LookupEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lersNextToken' - The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
--
-- * 'lersEvents' - A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
--
-- * 'lersResponseStatus' - -- | The response status code.
lookupEventsResponse
    :: Int -- ^ 'lersResponseStatus'
    -> LookupEventsResponse
lookupEventsResponse pResponseStatus_
  = LookupEventsResponse'{_lersNextToken = Nothing,
                          _lersEvents = Nothing,
                          _lersResponseStatus = pResponseStatus_}

-- | The token to use to get the next page of results after a previous API call. If the token does not appear, there are no more results to return. The token must be passed in with the same parameters as the previous call. For example, if the original call specified an AttributeKey of 'Username' with a value of 'root', the call with NextToken should include those same parameters.
lersNextToken :: Lens' LookupEventsResponse (Maybe Text)
lersNextToken = lens _lersNextToken (\ s a -> s{_lersNextToken = a})

-- | A list of events returned based on the lookup attributes specified and the CloudTrail event. The events list is sorted by time. The most recent event is listed first.
lersEvents :: Lens' LookupEventsResponse [Event]
lersEvents = lens _lersEvents (\ s a -> s{_lersEvents = a}) . _Default . _Coerce

-- | -- | The response status code.
lersResponseStatus :: Lens' LookupEventsResponse Int
lersResponseStatus = lens _lersResponseStatus (\ s a -> s{_lersResponseStatus = a})

instance NFData LookupEventsResponse where
