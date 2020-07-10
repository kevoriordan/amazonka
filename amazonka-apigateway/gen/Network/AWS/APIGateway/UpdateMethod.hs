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
-- Module      : Network.AWS.APIGateway.UpdateMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Method' resource.
--
--
module Network.AWS.APIGateway.UpdateMethod
    (
    -- * Creating a Request
      updateMethod
    , UpdateMethod
    -- * Request Lenses
    , updtmthdPatchOperations
    , updtmthdRestAPIId
    , updtmthdResourceId
    , updtmthdHttpMethod

    -- * Destructuring the Response
    , method
    , Method
    -- * Response Lenses
    , mMethodResponses
    , mHttpMethod
    , mAuthorizationScopes
    , mRequestValidatorId
    , mRequestModels
    , mRequestParameters
    , mAuthorizerId
    , mOperationName
    , mAuthorizationType
    , mApiKeyRequired
    , mMethodIntegration
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an existing 'Method' resource.
--
--
--
-- /See:/ 'updateMethod' smart constructor.
data UpdateMethod = UpdateMethod'{_updtmthdPatchOperations
                                  :: !(Maybe [PatchOperation]),
                                  _updtmthdRestAPIId :: !Text,
                                  _updtmthdResourceId :: !Text,
                                  _updtmthdHttpMethod :: !Text}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updtmthdPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'updtmthdRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'updtmthdResourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
--
-- * 'updtmthdHttpMethod' - [Required] The HTTP verb of the 'Method' resource.
updateMethod
    :: Text -- ^ 'updtmthdRestAPIId'
    -> Text -- ^ 'updtmthdResourceId'
    -> Text -- ^ 'updtmthdHttpMethod'
    -> UpdateMethod
updateMethod pRestAPIId_ pResourceId_ pHttpMethod_
  = UpdateMethod'{_updtmthdPatchOperations = Nothing,
                  _updtmthdRestAPIId = pRestAPIId_,
                  _updtmthdResourceId = pResourceId_,
                  _updtmthdHttpMethod = pHttpMethod_}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
updtmthdPatchOperations :: Lens' UpdateMethod [PatchOperation]
updtmthdPatchOperations = lens _updtmthdPatchOperations (\ s a -> s{_updtmthdPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
updtmthdRestAPIId :: Lens' UpdateMethod Text
updtmthdRestAPIId = lens _updtmthdRestAPIId (\ s a -> s{_updtmthdRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'Method' resource.
updtmthdResourceId :: Lens' UpdateMethod Text
updtmthdResourceId = lens _updtmthdResourceId (\ s a -> s{_updtmthdResourceId = a})

-- | [Required] The HTTP verb of the 'Method' resource.
updtmthdHttpMethod :: Lens' UpdateMethod Text
updtmthdHttpMethod = lens _updtmthdHttpMethod (\ s a -> s{_updtmthdHttpMethod = a})

instance AWSRequest UpdateMethod where
        type Rs UpdateMethod = Method
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateMethod where

instance NFData UpdateMethod where

instance ToHeaders UpdateMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateMethod where
        toJSON UpdateMethod'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$>
                    _updtmthdPatchOperations])

instance ToPath UpdateMethod where
        toPath UpdateMethod'{..}
          = mconcat
              ["/restapis/", toBS _updtmthdRestAPIId,
               "/resources/", toBS _updtmthdResourceId, "/methods/",
               toBS _updtmthdHttpMethod]

instance ToQuery UpdateMethod where
        toQuery = const mempty
