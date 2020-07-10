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
-- Module      : Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about one or more specified events for one or more accounts in your organization. Information includes standard event data (Region, service, and so on, as returned by 'DescribeEventsForOrganization' , a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the 'DescribeAffectedEntitiesForOrganization' operation.
--
--
-- Before you can call this operation, you must first enable AWS Health to work with AWS Organizations. To do this, call the 'EnableHealthServiceAccessForOrganization' operation from your organization's master account.
--
module Network.AWS.AWSHealth.DescribeEventDetailsForOrganization
    (
    -- * Creating a Request
      describeEventDetailsForOrganization
    , DescribeEventDetailsForOrganization
    -- * Request Lenses
    , dedfoLocale
    , dedfoOrganizationEventDetailFilters

    -- * Destructuring the Response
    , describeEventDetailsForOrganizationResponse
    , DescribeEventDetailsForOrganizationResponse
    -- * Response Lenses
    , dedforsSuccessfulSet
    , dedforsFailedSet
    , dedforsResponseStatus
    ) where

import Network.AWS.AWSHealth.Types
import Network.AWS.AWSHealth.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventDetailsForOrganization' smart constructor.
data DescribeEventDetailsForOrganization = DescribeEventDetailsForOrganization'{_dedfoLocale
                                                                                ::
                                                                                !(Maybe
                                                                                    Text),
                                                                                _dedfoOrganizationEventDetailFilters
                                                                                ::
                                                                                !(List1
                                                                                    EventAccountFilter)}
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'DescribeEventDetailsForOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedfoLocale' - The locale (language) to return information in. English (en) is the default and the only supported value at this time.
--
-- * 'dedfoOrganizationEventDetailFilters' - A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
describeEventDetailsForOrganization
    :: NonEmpty EventAccountFilter -- ^ 'dedfoOrganizationEventDetailFilters'
    -> DescribeEventDetailsForOrganization
describeEventDetailsForOrganization
  pOrganizationEventDetailFilters_
  = DescribeEventDetailsForOrganization'{_dedfoLocale =
                                           Nothing,
                                         _dedfoOrganizationEventDetailFilters =
                                           _List1 #
                                             pOrganizationEventDetailFilters_}

-- | The locale (language) to return information in. English (en) is the default and the only supported value at this time.
dedfoLocale :: Lens' DescribeEventDetailsForOrganization (Maybe Text)
dedfoLocale = lens _dedfoLocale (\ s a -> s{_dedfoLocale = a})

-- | A set of JSON elements that includes the @awsAccountId@ and the @eventArn@ .
dedfoOrganizationEventDetailFilters :: Lens' DescribeEventDetailsForOrganization (NonEmpty EventAccountFilter)
dedfoOrganizationEventDetailFilters = lens _dedfoOrganizationEventDetailFilters (\ s a -> s{_dedfoOrganizationEventDetailFilters = a}) . _List1

instance AWSRequest
           DescribeEventDetailsForOrganization
         where
        type Rs DescribeEventDetailsForOrganization =
             DescribeEventDetailsForOrganizationResponse
        request = postJSON awsHealth
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventDetailsForOrganizationResponse' <$>
                   (x .?> "successfulSet" .!@ mempty) <*>
                     (x .?> "failedSet" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventDetailsForOrganization
         where

instance NFData DescribeEventDetailsForOrganization
         where

instance ToHeaders
           DescribeEventDetailsForOrganization
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSHealth_20160804.DescribeEventDetailsForOrganization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventDetailsForOrganization
         where
        toJSON DescribeEventDetailsForOrganization'{..}
          = object
              (catMaybes
                 [("locale" .=) <$> _dedfoLocale,
                  Just
                    ("organizationEventDetailFilters" .=
                       _dedfoOrganizationEventDetailFilters)])

instance ToPath DescribeEventDetailsForOrganization
         where
        toPath = const "/"

instance ToQuery DescribeEventDetailsForOrganization
         where
        toQuery = const mempty

-- | /See:/ 'describeEventDetailsForOrganizationResponse' smart constructor.
data DescribeEventDetailsForOrganizationResponse = DescribeEventDetailsForOrganizationResponse'{_dedforsSuccessfulSet
                                                                                                ::
                                                                                                !(Maybe
                                                                                                    [OrganizationEventDetails]),
                                                                                                _dedforsFailedSet
                                                                                                ::
                                                                                                !(Maybe
                                                                                                    [OrganizationEventDetailsErrorItem]),
                                                                                                _dedforsResponseStatus
                                                                                                ::
                                                                                                !Int}
                                                     deriving (Eq, Read, Show,
                                                               Data, Typeable,
                                                               Generic)

-- | Creates a value of 'DescribeEventDetailsForOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedforsSuccessfulSet' - Information about the events that could be retrieved.
--
-- * 'dedforsFailedSet' - Error messages for any events that could not be retrieved.
--
-- * 'dedforsResponseStatus' - -- | The response status code.
describeEventDetailsForOrganizationResponse
    :: Int -- ^ 'dedforsResponseStatus'
    -> DescribeEventDetailsForOrganizationResponse
describeEventDetailsForOrganizationResponse
  pResponseStatus_
  = DescribeEventDetailsForOrganizationResponse'{_dedforsSuccessfulSet
                                                   = Nothing,
                                                 _dedforsFailedSet = Nothing,
                                                 _dedforsResponseStatus =
                                                   pResponseStatus_}

-- | Information about the events that could be retrieved.
dedforsSuccessfulSet :: Lens' DescribeEventDetailsForOrganizationResponse [OrganizationEventDetails]
dedforsSuccessfulSet = lens _dedforsSuccessfulSet (\ s a -> s{_dedforsSuccessfulSet = a}) . _Default . _Coerce

-- | Error messages for any events that could not be retrieved.
dedforsFailedSet :: Lens' DescribeEventDetailsForOrganizationResponse [OrganizationEventDetailsErrorItem]
dedforsFailedSet = lens _dedforsFailedSet (\ s a -> s{_dedforsFailedSet = a}) . _Default . _Coerce

-- | -- | The response status code.
dedforsResponseStatus :: Lens' DescribeEventDetailsForOrganizationResponse Int
dedforsResponseStatus = lens _dedforsResponseStatus (\ s a -> s{_dedforsResponseStatus = a})

instance NFData
           DescribeEventDetailsForOrganizationResponse
         where
