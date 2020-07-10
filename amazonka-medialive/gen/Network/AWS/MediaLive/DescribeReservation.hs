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
-- Module      : Network.AWS.MediaLive.DescribeReservation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for a reservation.
module Network.AWS.MediaLive.DescribeReservation
    (
    -- * Creating a Request
      describeReservation
    , DescribeReservation
    -- * Request Lenses
    , drReservationId

    -- * Destructuring the Response
    , describeReservationResponse
    , DescribeReservationResponse
    -- * Response Lenses
    , dscrbrsrvtnrsState
    , dscrbrsrvtnrsResourceSpecification
    , dscrbrsrvtnrsCurrencyCode
    , dscrbrsrvtnrsARN
    , dscrbrsrvtnrsStart
    , dscrbrsrvtnrsCount
    , dscrbrsrvtnrsEnd
    , dscrbrsrvtnrsName
    , dscrbrsrvtnrsReservationId
    , dscrbrsrvtnrsOfferingId
    , dscrbrsrvtnrsRegion
    , dscrbrsrvtnrsOfferingType
    , dscrbrsrvtnrsUsagePrice
    , dscrbrsrvtnrsFixedPrice
    , dscrbrsrvtnrsDurationUnits
    , dscrbrsrvtnrsOfferingDescription
    , dscrbrsrvtnrsDuration
    , dscrbrsrvtnrsTags
    , dscrbrsrvtnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeReservationRequest
--
-- /See:/ 'describeReservation' smart constructor.
newtype DescribeReservation = DescribeReservation'{_drReservationId
                                                   :: Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DescribeReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drReservationId' - Unique reservation ID, e.g. '1234567'
describeReservation
    :: Text -- ^ 'drReservationId'
    -> DescribeReservation
describeReservation pReservationId_
  = DescribeReservation'{_drReservationId =
                           pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
drReservationId :: Lens' DescribeReservation Text
drReservationId = lens _drReservationId (\ s a -> s{_drReservationId = a})

instance AWSRequest DescribeReservation where
        type Rs DescribeReservation =
             DescribeReservationResponse
        request = get mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReservationResponse' <$>
                   (x .?> "state") <*> (x .?> "resourceSpecification")
                     <*> (x .?> "currencyCode")
                     <*> (x .?> "arn")
                     <*> (x .?> "start")
                     <*> (x .?> "count")
                     <*> (x .?> "end")
                     <*> (x .?> "name")
                     <*> (x .?> "reservationId")
                     <*> (x .?> "offeringId")
                     <*> (x .?> "region")
                     <*> (x .?> "offeringType")
                     <*> (x .?> "usagePrice")
                     <*> (x .?> "fixedPrice")
                     <*> (x .?> "durationUnits")
                     <*> (x .?> "offeringDescription")
                     <*> (x .?> "duration")
                     <*> (x .?> "tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReservation where

instance NFData DescribeReservation where

instance ToHeaders DescribeReservation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeReservation where
        toPath DescribeReservation'{..}
          = mconcat
              ["/prod/reservations/", toBS _drReservationId]

instance ToQuery DescribeReservation where
        toQuery = const mempty

-- | Placeholder documentation for DescribeReservationResponse
--
-- /See:/ 'describeReservationResponse' smart constructor.
data DescribeReservationResponse = DescribeReservationResponse'{_dscrbrsrvtnrsState
                                                                ::
                                                                !(Maybe
                                                                    ReservationState),
                                                                _dscrbrsrvtnrsResourceSpecification
                                                                ::
                                                                !(Maybe
                                                                    ReservationResourceSpecification),
                                                                _dscrbrsrvtnrsCurrencyCode
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsARN
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsStart
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsCount
                                                                :: !(Maybe Int),
                                                                _dscrbrsrvtnrsEnd
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsName
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsReservationId
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsOfferingId
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsRegion
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsOfferingType
                                                                ::
                                                                !(Maybe
                                                                    OfferingType),
                                                                _dscrbrsrvtnrsUsagePrice
                                                                ::
                                                                !(Maybe Double),
                                                                _dscrbrsrvtnrsFixedPrice
                                                                ::
                                                                !(Maybe Double),
                                                                _dscrbrsrvtnrsDurationUnits
                                                                ::
                                                                !(Maybe
                                                                    OfferingDurationUnits),
                                                                _dscrbrsrvtnrsOfferingDescription
                                                                ::
                                                                !(Maybe Text),
                                                                _dscrbrsrvtnrsDuration
                                                                :: !(Maybe Int),
                                                                _dscrbrsrvtnrsTags
                                                                ::
                                                                !(Maybe
                                                                    (Map Text
                                                                       Text)),
                                                                _dscrbrsrvtnrsResponseStatus
                                                                :: !Int}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'DescribeReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbrsrvtnrsState' - Current state of reservation, e.g. 'ACTIVE'
--
-- * 'dscrbrsrvtnrsResourceSpecification' - Resource configuration details
--
-- * 'dscrbrsrvtnrsCurrencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- * 'dscrbrsrvtnrsARN' - Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
--
-- * 'dscrbrsrvtnrsStart' - Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
--
-- * 'dscrbrsrvtnrsCount' - Number of reserved resources
--
-- * 'dscrbrsrvtnrsEnd' - Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
--
-- * 'dscrbrsrvtnrsName' - User specified reservation name
--
-- * 'dscrbrsrvtnrsReservationId' - Unique reservation ID, e.g. '1234567'
--
-- * 'dscrbrsrvtnrsOfferingId' - Unique offering ID, e.g. '87654321'
--
-- * 'dscrbrsrvtnrsRegion' - AWS region, e.g. 'us-west-2'
--
-- * 'dscrbrsrvtnrsOfferingType' - Offering type, e.g. 'NO_UPFRONT'
--
-- * 'dscrbrsrvtnrsUsagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- * 'dscrbrsrvtnrsFixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- * 'dscrbrsrvtnrsDurationUnits' - Units for duration, e.g. 'MONTHS'
--
-- * 'dscrbrsrvtnrsOfferingDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- * 'dscrbrsrvtnrsDuration' - Lease duration, e.g. '12'
--
-- * 'dscrbrsrvtnrsTags' - A collection of key-value pairs
--
-- * 'dscrbrsrvtnrsResponseStatus' - -- | The response status code.
describeReservationResponse
    :: Int -- ^ 'dscrbrsrvtnrsResponseStatus'
    -> DescribeReservationResponse
describeReservationResponse pResponseStatus_
  = DescribeReservationResponse'{_dscrbrsrvtnrsState =
                                   Nothing,
                                 _dscrbrsrvtnrsResourceSpecification = Nothing,
                                 _dscrbrsrvtnrsCurrencyCode = Nothing,
                                 _dscrbrsrvtnrsARN = Nothing,
                                 _dscrbrsrvtnrsStart = Nothing,
                                 _dscrbrsrvtnrsCount = Nothing,
                                 _dscrbrsrvtnrsEnd = Nothing,
                                 _dscrbrsrvtnrsName = Nothing,
                                 _dscrbrsrvtnrsReservationId = Nothing,
                                 _dscrbrsrvtnrsOfferingId = Nothing,
                                 _dscrbrsrvtnrsRegion = Nothing,
                                 _dscrbrsrvtnrsOfferingType = Nothing,
                                 _dscrbrsrvtnrsUsagePrice = Nothing,
                                 _dscrbrsrvtnrsFixedPrice = Nothing,
                                 _dscrbrsrvtnrsDurationUnits = Nothing,
                                 _dscrbrsrvtnrsOfferingDescription = Nothing,
                                 _dscrbrsrvtnrsDuration = Nothing,
                                 _dscrbrsrvtnrsTags = Nothing,
                                 _dscrbrsrvtnrsResponseStatus =
                                   pResponseStatus_}

-- | Current state of reservation, e.g. 'ACTIVE'
dscrbrsrvtnrsState :: Lens' DescribeReservationResponse (Maybe ReservationState)
dscrbrsrvtnrsState = lens _dscrbrsrvtnrsState (\ s a -> s{_dscrbrsrvtnrsState = a})

-- | Resource configuration details
dscrbrsrvtnrsResourceSpecification :: Lens' DescribeReservationResponse (Maybe ReservationResourceSpecification)
dscrbrsrvtnrsResourceSpecification = lens _dscrbrsrvtnrsResourceSpecification (\ s a -> s{_dscrbrsrvtnrsResourceSpecification = a})

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
dscrbrsrvtnrsCurrencyCode :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsCurrencyCode = lens _dscrbrsrvtnrsCurrencyCode (\ s a -> s{_dscrbrsrvtnrsCurrencyCode = a})

-- | Unique reservation ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:reservation:1234567'
dscrbrsrvtnrsARN :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsARN = lens _dscrbrsrvtnrsARN (\ s a -> s{_dscrbrsrvtnrsARN = a})

-- | Reservation UTC start date and time in ISO-8601 format, e.g. '2018-03-01T00:00:00'
dscrbrsrvtnrsStart :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsStart = lens _dscrbrsrvtnrsStart (\ s a -> s{_dscrbrsrvtnrsStart = a})

-- | Number of reserved resources
dscrbrsrvtnrsCount :: Lens' DescribeReservationResponse (Maybe Int)
dscrbrsrvtnrsCount = lens _dscrbrsrvtnrsCount (\ s a -> s{_dscrbrsrvtnrsCount = a})

-- | Reservation UTC end date and time in ISO-8601 format, e.g. '2019-03-01T00:00:00'
dscrbrsrvtnrsEnd :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsEnd = lens _dscrbrsrvtnrsEnd (\ s a -> s{_dscrbrsrvtnrsEnd = a})

-- | User specified reservation name
dscrbrsrvtnrsName :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsName = lens _dscrbrsrvtnrsName (\ s a -> s{_dscrbrsrvtnrsName = a})

-- | Unique reservation ID, e.g. '1234567'
dscrbrsrvtnrsReservationId :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsReservationId = lens _dscrbrsrvtnrsReservationId (\ s a -> s{_dscrbrsrvtnrsReservationId = a})

-- | Unique offering ID, e.g. '87654321'
dscrbrsrvtnrsOfferingId :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsOfferingId = lens _dscrbrsrvtnrsOfferingId (\ s a -> s{_dscrbrsrvtnrsOfferingId = a})

-- | AWS region, e.g. 'us-west-2'
dscrbrsrvtnrsRegion :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsRegion = lens _dscrbrsrvtnrsRegion (\ s a -> s{_dscrbrsrvtnrsRegion = a})

-- | Offering type, e.g. 'NO_UPFRONT'
dscrbrsrvtnrsOfferingType :: Lens' DescribeReservationResponse (Maybe OfferingType)
dscrbrsrvtnrsOfferingType = lens _dscrbrsrvtnrsOfferingType (\ s a -> s{_dscrbrsrvtnrsOfferingType = a})

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
dscrbrsrvtnrsUsagePrice :: Lens' DescribeReservationResponse (Maybe Double)
dscrbrsrvtnrsUsagePrice = lens _dscrbrsrvtnrsUsagePrice (\ s a -> s{_dscrbrsrvtnrsUsagePrice = a})

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
dscrbrsrvtnrsFixedPrice :: Lens' DescribeReservationResponse (Maybe Double)
dscrbrsrvtnrsFixedPrice = lens _dscrbrsrvtnrsFixedPrice (\ s a -> s{_dscrbrsrvtnrsFixedPrice = a})

-- | Units for duration, e.g. 'MONTHS'
dscrbrsrvtnrsDurationUnits :: Lens' DescribeReservationResponse (Maybe OfferingDurationUnits)
dscrbrsrvtnrsDurationUnits = lens _dscrbrsrvtnrsDurationUnits (\ s a -> s{_dscrbrsrvtnrsDurationUnits = a})

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
dscrbrsrvtnrsOfferingDescription :: Lens' DescribeReservationResponse (Maybe Text)
dscrbrsrvtnrsOfferingDescription = lens _dscrbrsrvtnrsOfferingDescription (\ s a -> s{_dscrbrsrvtnrsOfferingDescription = a})

-- | Lease duration, e.g. '12'
dscrbrsrvtnrsDuration :: Lens' DescribeReservationResponse (Maybe Int)
dscrbrsrvtnrsDuration = lens _dscrbrsrvtnrsDuration (\ s a -> s{_dscrbrsrvtnrsDuration = a})

-- | A collection of key-value pairs
dscrbrsrvtnrsTags :: Lens' DescribeReservationResponse (HashMap Text Text)
dscrbrsrvtnrsTags = lens _dscrbrsrvtnrsTags (\ s a -> s{_dscrbrsrvtnrsTags = a}) . _Default . _Map

-- | -- | The response status code.
dscrbrsrvtnrsResponseStatus :: Lens' DescribeReservationResponse Int
dscrbrsrvtnrsResponseStatus = lens _dscrbrsrvtnrsResponseStatus (\ s a -> s{_dscrbrsrvtnrsResponseStatus = a})

instance NFData DescribeReservationResponse where
