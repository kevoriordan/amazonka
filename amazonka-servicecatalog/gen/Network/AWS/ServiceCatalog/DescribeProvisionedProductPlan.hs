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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resource changes for the specified plan.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
    (
    -- * Creating a Request
      describeProvisionedProductPlan
    , DescribeProvisionedProductPlan
    -- * Request Lenses
    , dpppAcceptLanguage
    , dpppPageToken
    , dpppPageSize
    , dpppPlanId

    -- * Destructuring the Response
    , describeProvisionedProductPlanResponse
    , DescribeProvisionedProductPlanResponse
    -- * Response Lenses
    , dscrbprvsndprdctplnrsNextPageToken
    , dscrbprvsndprdctplnrsProvisionedProductPlanDetails
    , dscrbprvsndprdctplnrsResourceChanges
    , dscrbprvsndprdctplnrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'{_dpppAcceptLanguage
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dpppPageToken
                                                                      ::
                                                                      !(Maybe
                                                                          Text),
                                                                      _dpppPageSize
                                                                      ::
                                                                      !(Maybe
                                                                          Nat),
                                                                      _dpppPlanId
                                                                      :: !Text}
                                        deriving (Eq, Read, Show, Data,
                                                  Typeable, Generic)

-- | Creates a value of 'DescribeProvisionedProductPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'dpppPageSize' - The maximum number of items to return with this call.
--
-- * 'dpppPlanId' - The plan identifier.
describeProvisionedProductPlan
    :: Text -- ^ 'dpppPlanId'
    -> DescribeProvisionedProductPlan
describeProvisionedProductPlan pPlanId_
  = DescribeProvisionedProductPlan'{_dpppAcceptLanguage
                                      = Nothing,
                                    _dpppPageToken = Nothing,
                                    _dpppPageSize = Nothing,
                                    _dpppPlanId = pPlanId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpppAcceptLanguage :: Lens' DescribeProvisionedProductPlan (Maybe Text)
dpppAcceptLanguage = lens _dpppAcceptLanguage (\ s a -> s{_dpppAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
dpppPageToken :: Lens' DescribeProvisionedProductPlan (Maybe Text)
dpppPageToken = lens _dpppPageToken (\ s a -> s{_dpppPageToken = a})

-- | The maximum number of items to return with this call.
dpppPageSize :: Lens' DescribeProvisionedProductPlan (Maybe Natural)
dpppPageSize = lens _dpppPageSize (\ s a -> s{_dpppPageSize = a}) . mapping _Nat

-- | The plan identifier.
dpppPlanId :: Lens' DescribeProvisionedProductPlan Text
dpppPlanId = lens _dpppPlanId (\ s a -> s{_dpppPlanId = a})

instance AWSRequest DescribeProvisionedProductPlan
         where
        type Rs DescribeProvisionedProductPlan =
             DescribeProvisionedProductPlanResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisionedProductPlanResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProductPlanDetails")
                     <*> (x .?> "ResourceChanges" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisionedProductPlan
         where

instance NFData DescribeProvisionedProductPlan where

instance ToHeaders DescribeProvisionedProductPlan
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisionedProductPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisionedProductPlan where
        toJSON DescribeProvisionedProductPlan'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpppAcceptLanguage,
                  ("PageToken" .=) <$> _dpppPageToken,
                  ("PageSize" .=) <$> _dpppPageSize,
                  Just ("PlanId" .= _dpppPlanId)])

instance ToPath DescribeProvisionedProductPlan where
        toPath = const "/"

instance ToQuery DescribeProvisionedProductPlan where
        toQuery = const mempty

-- | /See:/ 'describeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'{_dscrbprvsndprdctplnrsNextPageToken
                                                                                      ::
                                                                                      !(Maybe
                                                                                          Text),
                                                                                      _dscrbprvsndprdctplnrsProvisionedProductPlanDetails
                                                                                      ::
                                                                                      !(Maybe
                                                                                          ProvisionedProductPlanDetails),
                                                                                      _dscrbprvsndprdctplnrsResourceChanges
                                                                                      ::
                                                                                      !(Maybe
                                                                                          [ResourceChange]),
                                                                                      _dscrbprvsndprdctplnrsResponseStatus
                                                                                      ::
                                                                                      !Int}
                                                deriving (Eq, Read, Show, Data,
                                                          Typeable, Generic)

-- | Creates a value of 'DescribeProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbprvsndprdctplnrsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'dscrbprvsndprdctplnrsProvisionedProductPlanDetails' - Information about the plan.
--
-- * 'dscrbprvsndprdctplnrsResourceChanges' - Information about the resource changes that will occur when the plan is executed.
--
-- * 'dscrbprvsndprdctplnrsResponseStatus' - -- | The response status code.
describeProvisionedProductPlanResponse
    :: Int -- ^ 'dscrbprvsndprdctplnrsResponseStatus'
    -> DescribeProvisionedProductPlanResponse
describeProvisionedProductPlanResponse
  pResponseStatus_
  = DescribeProvisionedProductPlanResponse'{_dscrbprvsndprdctplnrsNextPageToken
                                              = Nothing,
                                            _dscrbprvsndprdctplnrsProvisionedProductPlanDetails
                                              = Nothing,
                                            _dscrbprvsndprdctplnrsResourceChanges
                                              = Nothing,
                                            _dscrbprvsndprdctplnrsResponseStatus
                                              = pResponseStatus_}

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
dscrbprvsndprdctplnrsNextPageToken :: Lens' DescribeProvisionedProductPlanResponse (Maybe Text)
dscrbprvsndprdctplnrsNextPageToken = lens _dscrbprvsndprdctplnrsNextPageToken (\ s a -> s{_dscrbprvsndprdctplnrsNextPageToken = a})

-- | Information about the plan.
dscrbprvsndprdctplnrsProvisionedProductPlanDetails :: Lens' DescribeProvisionedProductPlanResponse (Maybe ProvisionedProductPlanDetails)
dscrbprvsndprdctplnrsProvisionedProductPlanDetails = lens _dscrbprvsndprdctplnrsProvisionedProductPlanDetails (\ s a -> s{_dscrbprvsndprdctplnrsProvisionedProductPlanDetails = a})

-- | Information about the resource changes that will occur when the plan is executed.
dscrbprvsndprdctplnrsResourceChanges :: Lens' DescribeProvisionedProductPlanResponse [ResourceChange]
dscrbprvsndprdctplnrsResourceChanges = lens _dscrbprvsndprdctplnrsResourceChanges (\ s a -> s{_dscrbprvsndprdctplnrsResourceChanges = a}) . _Default . _Coerce

-- | -- | The response status code.
dscrbprvsndprdctplnrsResponseStatus :: Lens' DescribeProvisionedProductPlanResponse Int
dscrbprvsndprdctplnrsResponseStatus = lens _dscrbprvsndprdctplnrsResponseStatus (\ s a -> s{_dscrbprvsndprdctplnrsResponseStatus = a})

instance NFData
           DescribeProvisionedProductPlanResponse
         where
