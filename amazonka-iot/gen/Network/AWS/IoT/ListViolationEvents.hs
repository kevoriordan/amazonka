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
-- Module      : Network.AWS.IoT.ListViolationEvents
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profile violations discovered during the given time period. You can use filters to limit the results to those alerts issued for a particular security profile, behavior, or thing (device).
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListViolationEvents
    (
    -- * Creating a Request
      listViolationEvents
    , ListViolationEvents
    -- * Request Lenses
    , lveNextToken
    , lveSecurityProfileName
    , lveThingName
    , lveMaxResults
    , lveStartTime
    , lveEndTime

    -- * Destructuring the Response
    , listViolationEventsResponse
    , ListViolationEventsResponse
    -- * Response Lenses
    , lversViolationEvents
    , lversNextToken
    , lversResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listViolationEvents' smart constructor.
data ListViolationEvents = ListViolationEvents'{_lveNextToken
                                                :: !(Maybe Text),
                                                _lveSecurityProfileName ::
                                                !(Maybe Text),
                                                _lveThingName :: !(Maybe Text),
                                                _lveMaxResults :: !(Maybe Nat),
                                                _lveStartTime :: !POSIX,
                                                _lveEndTime :: !POSIX}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListViolationEvents' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lveNextToken' - The token for the next set of results.
--
-- * 'lveSecurityProfileName' - A filter to limit results to those alerts generated by the specified security profile.
--
-- * 'lveThingName' - A filter to limit results to those alerts caused by the specified thing.
--
-- * 'lveMaxResults' - The maximum number of results to return at one time.
--
-- * 'lveStartTime' - The start time for the alerts to be listed.
--
-- * 'lveEndTime' - The end time for the alerts to be listed.
listViolationEvents
    :: UTCTime -- ^ 'lveStartTime'
    -> UTCTime -- ^ 'lveEndTime'
    -> ListViolationEvents
listViolationEvents pStartTime_ pEndTime_
  = ListViolationEvents'{_lveNextToken = Nothing,
                         _lveSecurityProfileName = Nothing,
                         _lveThingName = Nothing, _lveMaxResults = Nothing,
                         _lveStartTime = _Time # pStartTime_,
                         _lveEndTime = _Time # pEndTime_}

-- | The token for the next set of results.
lveNextToken :: Lens' ListViolationEvents (Maybe Text)
lveNextToken = lens _lveNextToken (\ s a -> s{_lveNextToken = a})

-- | A filter to limit results to those alerts generated by the specified security profile.
lveSecurityProfileName :: Lens' ListViolationEvents (Maybe Text)
lveSecurityProfileName = lens _lveSecurityProfileName (\ s a -> s{_lveSecurityProfileName = a})

-- | A filter to limit results to those alerts caused by the specified thing.
lveThingName :: Lens' ListViolationEvents (Maybe Text)
lveThingName = lens _lveThingName (\ s a -> s{_lveThingName = a})

-- | The maximum number of results to return at one time.
lveMaxResults :: Lens' ListViolationEvents (Maybe Natural)
lveMaxResults = lens _lveMaxResults (\ s a -> s{_lveMaxResults = a}) . mapping _Nat

-- | The start time for the alerts to be listed.
lveStartTime :: Lens' ListViolationEvents UTCTime
lveStartTime = lens _lveStartTime (\ s a -> s{_lveStartTime = a}) . _Time

-- | The end time for the alerts to be listed.
lveEndTime :: Lens' ListViolationEvents UTCTime
lveEndTime = lens _lveEndTime (\ s a -> s{_lveEndTime = a}) . _Time

instance AWSPager ListViolationEvents where
        page rq rs
          | stop (rs ^. lversNextToken) = Nothing
          | stop (rs ^. lversViolationEvents) = Nothing
          | otherwise =
            Just $ rq & lveNextToken .~ rs ^. lversNextToken

instance AWSRequest ListViolationEvents where
        type Rs ListViolationEvents =
             ListViolationEventsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListViolationEventsResponse' <$>
                   (x .?> "violationEvents" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListViolationEvents where

instance NFData ListViolationEvents where

instance ToHeaders ListViolationEvents where
        toHeaders = const mempty

instance ToPath ListViolationEvents where
        toPath = const "/violation-events"

instance ToQuery ListViolationEvents where
        toQuery ListViolationEvents'{..}
          = mconcat
              ["nextToken" =: _lveNextToken,
               "securityProfileName" =: _lveSecurityProfileName,
               "thingName" =: _lveThingName,
               "maxResults" =: _lveMaxResults,
               "startTime" =: _lveStartTime,
               "endTime" =: _lveEndTime]

-- | /See:/ 'listViolationEventsResponse' smart constructor.
data ListViolationEventsResponse = ListViolationEventsResponse'{_lversViolationEvents
                                                                ::
                                                                !(Maybe
                                                                    [ViolationEvent]),
                                                                _lversNextToken
                                                                ::
                                                                !(Maybe Text),
                                                                _lversResponseStatus
                                                                :: !Int}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'ListViolationEventsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lversViolationEvents' - The security profile violation alerts issued for this account during the given time period, potentially filtered by security profile, behavior violated, or thing (device) violating.
--
-- * 'lversNextToken' - A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- * 'lversResponseStatus' - -- | The response status code.
listViolationEventsResponse
    :: Int -- ^ 'lversResponseStatus'
    -> ListViolationEventsResponse
listViolationEventsResponse pResponseStatus_
  = ListViolationEventsResponse'{_lversViolationEvents
                                   = Nothing,
                                 _lversNextToken = Nothing,
                                 _lversResponseStatus = pResponseStatus_}

-- | The security profile violation alerts issued for this account during the given time period, potentially filtered by security profile, behavior violated, or thing (device) violating.
lversViolationEvents :: Lens' ListViolationEventsResponse [ViolationEvent]
lversViolationEvents = lens _lversViolationEvents (\ s a -> s{_lversViolationEvents = a}) . _Default . _Coerce

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
lversNextToken :: Lens' ListViolationEventsResponse (Maybe Text)
lversNextToken = lens _lversNextToken (\ s a -> s{_lversNextToken = a})

-- | -- | The response status code.
lversResponseStatus :: Lens' ListViolationEventsResponse Int
lversResponseStatus = lens _lversResponseStatus (\ s a -> s{_lversResponseStatus = a})

instance NFData ListViolationEventsResponse where
