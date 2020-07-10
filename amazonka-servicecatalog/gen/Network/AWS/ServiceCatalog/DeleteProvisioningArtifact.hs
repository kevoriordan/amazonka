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
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified provisioning artifact (also known as a version) for the specified product.
--
--
-- You cannot delete a provisioning artifact associated with a product that was shared with you. You cannot delete the last provisioning artifact for a product, because a product must have at least one provisioning artifact.
--
module Network.AWS.ServiceCatalog.DeleteProvisioningArtifact
    (
    -- * Creating a Request
      deleteProvisioningArtifact
    , DeleteProvisioningArtifact
    -- * Request Lenses
    , dltprvsnngartfctAcceptLanguage
    , dltprvsnngartfctProductId
    , dltprvsnngartfctProvisioningArtifactId

    -- * Destructuring the Response
    , deleteProvisioningArtifactResponse
    , DeleteProvisioningArtifactResponse
    -- * Response Lenses
    , dparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteProvisioningArtifact' smart constructor.
data DeleteProvisioningArtifact = DeleteProvisioningArtifact'{_dltprvsnngartfctAcceptLanguage
                                                              :: !(Maybe Text),
                                                              _dltprvsnngartfctProductId
                                                              :: !Text,
                                                              _dltprvsnngartfctProvisioningArtifactId
                                                              :: !Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeleteProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltprvsnngartfctAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dltprvsnngartfctProductId' - The product identifier.
--
-- * 'dltprvsnngartfctProvisioningArtifactId' - The identifier of the provisioning artifact.
deleteProvisioningArtifact
    :: Text -- ^ 'dltprvsnngartfctProductId'
    -> Text -- ^ 'dltprvsnngartfctProvisioningArtifactId'
    -> DeleteProvisioningArtifact
deleteProvisioningArtifact pProductId_
  pProvisioningArtifactId_
  = DeleteProvisioningArtifact'{_dltprvsnngartfctAcceptLanguage
                                  = Nothing,
                                _dltprvsnngartfctProductId = pProductId_,
                                _dltprvsnngartfctProvisioningArtifactId =
                                  pProvisioningArtifactId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dltprvsnngartfctAcceptLanguage :: Lens' DeleteProvisioningArtifact (Maybe Text)
dltprvsnngartfctAcceptLanguage = lens _dltprvsnngartfctAcceptLanguage (\ s a -> s{_dltprvsnngartfctAcceptLanguage = a})

-- | The product identifier.
dltprvsnngartfctProductId :: Lens' DeleteProvisioningArtifact Text
dltprvsnngartfctProductId = lens _dltprvsnngartfctProductId (\ s a -> s{_dltprvsnngartfctProductId = a})

-- | The identifier of the provisioning artifact.
dltprvsnngartfctProvisioningArtifactId :: Lens' DeleteProvisioningArtifact Text
dltprvsnngartfctProvisioningArtifactId = lens _dltprvsnngartfctProvisioningArtifactId (\ s a -> s{_dltprvsnngartfctProvisioningArtifactId = a})

instance AWSRequest DeleteProvisioningArtifact where
        type Rs DeleteProvisioningArtifact =
             DeleteProvisioningArtifactResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProvisioningArtifactResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteProvisioningArtifact where

instance NFData DeleteProvisioningArtifact where

instance ToHeaders DeleteProvisioningArtifact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteProvisioningArtifact"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProvisioningArtifact where
        toJSON DeleteProvisioningArtifact'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$>
                    _dltprvsnngartfctAcceptLanguage,
                  Just ("ProductId" .= _dltprvsnngartfctProductId),
                  Just
                    ("ProvisioningArtifactId" .=
                       _dltprvsnngartfctProvisioningArtifactId)])

instance ToPath DeleteProvisioningArtifact where
        toPath = const "/"

instance ToQuery DeleteProvisioningArtifact where
        toQuery = const mempty

-- | /See:/ 'deleteProvisioningArtifactResponse' smart constructor.
newtype DeleteProvisioningArtifactResponse = DeleteProvisioningArtifactResponse'{_dparsResponseStatus
                                                                                 ::
                                                                                 Int}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'DeleteProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dparsResponseStatus' - -- | The response status code.
deleteProvisioningArtifactResponse
    :: Int -- ^ 'dparsResponseStatus'
    -> DeleteProvisioningArtifactResponse
deleteProvisioningArtifactResponse pResponseStatus_
  = DeleteProvisioningArtifactResponse'{_dparsResponseStatus
                                          = pResponseStatus_}

-- | -- | The response status code.
dparsResponseStatus :: Lens' DeleteProvisioningArtifactResponse Int
dparsResponseStatus = lens _dparsResponseStatus (\ s a -> s{_dparsResponseStatus = a})

instance NFData DeleteProvisioningArtifactResponse
         where
