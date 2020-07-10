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
-- Module      : Network.AWS.ServiceCatalog.DeleteProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified product.
--
--
-- You cannot delete a product if it was shared with you or is associated with a portfolio.
--
module Network.AWS.ServiceCatalog.DeleteProduct
    (
    -- * Creating a Request
      deleteProduct
    , DeleteProduct
    -- * Request Lenses
    , dltprdctAcceptLanguage
    , dltprdctId

    -- * Destructuring the Response
    , deleteProductResponse
    , DeleteProductResponse
    -- * Response Lenses
    , dltprdctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteProduct' smart constructor.
data DeleteProduct = DeleteProduct'{_dltprdctAcceptLanguage
                                    :: !(Maybe Text),
                                    _dltprdctId :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltprdctAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dltprdctId' - The product identifier.
deleteProduct
    :: Text -- ^ 'dltprdctId'
    -> DeleteProduct
deleteProduct pId_
  = DeleteProduct'{_dltprdctAcceptLanguage = Nothing,
                   _dltprdctId = pId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dltprdctAcceptLanguage :: Lens' DeleteProduct (Maybe Text)
dltprdctAcceptLanguage = lens _dltprdctAcceptLanguage (\ s a -> s{_dltprdctAcceptLanguage = a})

-- | The product identifier.
dltprdctId :: Lens' DeleteProduct Text
dltprdctId = lens _dltprdctId (\ s a -> s{_dltprdctId = a})

instance AWSRequest DeleteProduct where
        type Rs DeleteProduct = DeleteProductResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProductResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteProduct where

instance NFData DeleteProduct where

instance ToHeaders DeleteProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProduct where
        toJSON DeleteProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dltprdctAcceptLanguage,
                  Just ("Id" .= _dltprdctId)])

instance ToPath DeleteProduct where
        toPath = const "/"

instance ToQuery DeleteProduct where
        toQuery = const mempty

-- | /See:/ 'deleteProductResponse' smart constructor.
newtype DeleteProductResponse = DeleteProductResponse'{_dltprdctrsResponseStatus
                                                       :: Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'DeleteProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltprdctrsResponseStatus' - -- | The response status code.
deleteProductResponse
    :: Int -- ^ 'dltprdctrsResponseStatus'
    -> DeleteProductResponse
deleteProductResponse pResponseStatus_
  = DeleteProductResponse'{_dltprdctrsResponseStatus =
                             pResponseStatus_}

-- | -- | The response status code.
dltprdctrsResponseStatus :: Lens' DeleteProductResponse Int
dltprdctrsResponseStatus = lens _dltprdctrsResponseStatus (\ s a -> s{_dltprdctrsResponseStatus = a})

instance NFData DeleteProductResponse where
