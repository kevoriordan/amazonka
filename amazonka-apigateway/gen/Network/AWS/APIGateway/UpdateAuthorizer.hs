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
-- Module      : Network.AWS.APIGateway.UpdateAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing 'Authorizer' resource.
--
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/update-authorizer.html AWS CLI> 
module Network.AWS.APIGateway.UpdateAuthorizer
    (
    -- * Creating a Request
      updateAuthorizer
    , UpdateAuthorizer
    -- * Request Lenses
    , updtathrzrPatchOperations
    , updtathrzrRestAPIId
    , updtathrzrAuthorizerId

    -- * Destructuring the Response
    , authorizer
    , Authorizer
    -- * Response Lenses
    , aAuthorizerURI
    , aIdentityValidationExpression
    , aProviderARNs
    , aName
    , aId
    , aAuthorizerResultTtlInSeconds
    , aAuthType
    , aType
    , aIdentitySource
    , aAuthorizerCredentials
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update an existing 'Authorizer' resource.
--
--
--
-- /See:/ 'updateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'{_updtathrzrPatchOperations
                                          :: !(Maybe [PatchOperation]),
                                          _updtathrzrRestAPIId :: !Text,
                                          _updtathrzrAuthorizerId :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'updtathrzrPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'updtathrzrRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'updtathrzrAuthorizerId' - [Required] The identifier of the 'Authorizer' resource.
updateAuthorizer
    :: Text -- ^ 'updtathrzrRestAPIId'
    -> Text -- ^ 'updtathrzrAuthorizerId'
    -> UpdateAuthorizer
updateAuthorizer pRestAPIId_ pAuthorizerId_
  = UpdateAuthorizer'{_updtathrzrPatchOperations =
                        Nothing,
                      _updtathrzrRestAPIId = pRestAPIId_,
                      _updtathrzrAuthorizerId = pAuthorizerId_}

-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
updtathrzrPatchOperations :: Lens' UpdateAuthorizer [PatchOperation]
updtathrzrPatchOperations = lens _updtathrzrPatchOperations (\ s a -> s{_updtathrzrPatchOperations = a}) . _Default . _Coerce

-- | [Required] The string identifier of the associated 'RestApi' .
updtathrzrRestAPIId :: Lens' UpdateAuthorizer Text
updtathrzrRestAPIId = lens _updtathrzrRestAPIId (\ s a -> s{_updtathrzrRestAPIId = a})

-- | [Required] The identifier of the 'Authorizer' resource.
updtathrzrAuthorizerId :: Lens' UpdateAuthorizer Text
updtathrzrAuthorizerId = lens _updtathrzrAuthorizerId (\ s a -> s{_updtathrzrAuthorizerId = a})

instance AWSRequest UpdateAuthorizer where
        type Rs UpdateAuthorizer = Authorizer
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateAuthorizer where

instance NFData UpdateAuthorizer where

instance ToHeaders UpdateAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateAuthorizer where
        toJSON UpdateAuthorizer'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$>
                    _updtathrzrPatchOperations])

instance ToPath UpdateAuthorizer where
        toPath UpdateAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _updtathrzrRestAPIId,
               "/authorizers/", toBS _updtathrzrAuthorizerId]

instance ToQuery UpdateAuthorizer where
        toQuery = const mempty
