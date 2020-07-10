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
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
--
--
module Network.AWS.ServiceCatalog.DescribeProduct
    (
    -- * Creating a Request
      describeProduct
    , DescribeProduct
    -- * Request Lenses
    , dpAcceptLanguage
    , dpId

    -- * Destructuring the Response
    , describeProductResponse
    , DescribeProductResponse
    -- * Response Lenses
    , dscrbprdctrsProductViewSummary
    , dscrbprdctrsProvisioningArtifacts
    , dscrbprdctrsBudgets
    , dscrbprdctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProduct' smart constructor.
data DescribeProduct = DescribeProduct'{_dpAcceptLanguage
                                        :: !(Maybe Text),
                                        _dpId :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpId' - The product identifier.
describeProduct
    :: Text -- ^ 'dpId'
    -> DescribeProduct
describeProduct pId_
  = DescribeProduct'{_dpAcceptLanguage = Nothing,
                     _dpId = pId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpAcceptLanguage :: Lens' DescribeProduct (Maybe Text)
dpAcceptLanguage = lens _dpAcceptLanguage (\ s a -> s{_dpAcceptLanguage = a})

-- | The product identifier.
dpId :: Lens' DescribeProduct Text
dpId = lens _dpId (\ s a -> s{_dpId = a})

instance AWSRequest DescribeProduct where
        type Rs DescribeProduct = DescribeProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProductResponse' <$>
                   (x .?> "ProductViewSummary") <*>
                     (x .?> "ProvisioningArtifacts" .!@ mempty)
                     <*> (x .?> "Budgets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProduct where

instance NFData DescribeProduct where

instance ToHeaders DescribeProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProduct where
        toJSON DescribeProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpAcceptLanguage,
                  Just ("Id" .= _dpId)])

instance ToPath DescribeProduct where
        toPath = const "/"

instance ToQuery DescribeProduct where
        toQuery = const mempty

-- | /See:/ 'describeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'{_dscrbprdctrsProductViewSummary
                                                        ::
                                                        !(Maybe
                                                            ProductViewSummary),
                                                        _dscrbprdctrsProvisioningArtifacts
                                                        ::
                                                        !(Maybe
                                                            [ProvisioningArtifact]),
                                                        _dscrbprdctrsBudgets ::
                                                        !(Maybe [BudgetDetail]),
                                                        _dscrbprdctrsResponseStatus
                                                        :: !Int}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'DescribeProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbprdctrsProductViewSummary' - Summary information about the product view.
--
-- * 'dscrbprdctrsProvisioningArtifacts' - Information about the provisioning artifacts for the specified product.
--
-- * 'dscrbprdctrsBudgets' - Information about the associated budgets.
--
-- * 'dscrbprdctrsResponseStatus' - -- | The response status code.
describeProductResponse
    :: Int -- ^ 'dscrbprdctrsResponseStatus'
    -> DescribeProductResponse
describeProductResponse pResponseStatus_
  = DescribeProductResponse'{_dscrbprdctrsProductViewSummary
                               = Nothing,
                             _dscrbprdctrsProvisioningArtifacts = Nothing,
                             _dscrbprdctrsBudgets = Nothing,
                             _dscrbprdctrsResponseStatus = pResponseStatus_}

-- | Summary information about the product view.
dscrbprdctrsProductViewSummary :: Lens' DescribeProductResponse (Maybe ProductViewSummary)
dscrbprdctrsProductViewSummary = lens _dscrbprdctrsProductViewSummary (\ s a -> s{_dscrbprdctrsProductViewSummary = a})

-- | Information about the provisioning artifacts for the specified product.
dscrbprdctrsProvisioningArtifacts :: Lens' DescribeProductResponse [ProvisioningArtifact]
dscrbprdctrsProvisioningArtifacts = lens _dscrbprdctrsProvisioningArtifacts (\ s a -> s{_dscrbprdctrsProvisioningArtifacts = a}) . _Default . _Coerce

-- | Information about the associated budgets.
dscrbprdctrsBudgets :: Lens' DescribeProductResponse [BudgetDetail]
dscrbprdctrsBudgets = lens _dscrbprdctrsBudgets (\ s a -> s{_dscrbprdctrsBudgets = a}) . _Default . _Coerce

-- | -- | The response status code.
dscrbprdctrsResponseStatus :: Lens' DescribeProductResponse Int
dscrbprdctrsResponseStatus = lens _dscrbprdctrsResponseStatus (\ s a -> s{_dscrbprdctrsResponseStatus = a})

instance NFData DescribeProductResponse where
