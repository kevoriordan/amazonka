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
-- Module      : Network.AWS.APIGateway.GetAuthorizer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Authorizer' resource.
--
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/get-authorizer.html AWS CLI> 
module Network.AWS.APIGateway.GetAuthorizer
    (
    -- * Creating a Request
      getAuthorizer
    , GetAuthorizer
    -- * Request Lenses
    , gtathrzrRestAPIId
    , gtathrzrAuthorizerId

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

-- | Request to describe an existing 'Authorizer' resource.
--
--
--
-- /See:/ 'getAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'{_gtathrzrRestAPIId
                                    :: !Text,
                                    _gtathrzrAuthorizerId :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAuthorizer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtathrzrRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gtathrzrAuthorizerId' - [Required] The identifier of the 'Authorizer' resource.
getAuthorizer
    :: Text -- ^ 'gtathrzrRestAPIId'
    -> Text -- ^ 'gtathrzrAuthorizerId'
    -> GetAuthorizer
getAuthorizer pRestAPIId_ pAuthorizerId_
  = GetAuthorizer'{_gtathrzrRestAPIId = pRestAPIId_,
                   _gtathrzrAuthorizerId = pAuthorizerId_}

-- | [Required] The string identifier of the associated 'RestApi' .
gtathrzrRestAPIId :: Lens' GetAuthorizer Text
gtathrzrRestAPIId = lens _gtathrzrRestAPIId (\ s a -> s{_gtathrzrRestAPIId = a})

-- | [Required] The identifier of the 'Authorizer' resource.
gtathrzrAuthorizerId :: Lens' GetAuthorizer Text
gtathrzrAuthorizerId = lens _gtathrzrAuthorizerId (\ s a -> s{_gtathrzrAuthorizerId = a})

instance AWSRequest GetAuthorizer where
        type Rs GetAuthorizer = Authorizer
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetAuthorizer where

instance NFData GetAuthorizer where

instance ToHeaders GetAuthorizer where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetAuthorizer where
        toPath GetAuthorizer'{..}
          = mconcat
              ["/restapis/", toBS _gtathrzrRestAPIId,
               "/authorizers/", toBS _gtathrzrAuthorizerId]

instance ToQuery GetAuthorizer where
        toQuery = const mempty
