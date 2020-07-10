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
-- Module      : Network.AWS.APIGateway.GetMethod
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe an existing 'Method' resource.
--
--
module Network.AWS.APIGateway.GetMethod
    (
    -- * Creating a Request
      getMethod
    , GetMethod
    -- * Request Lenses
    , gtmthdRestAPIId
    , gtmthdResourceId
    , gtmthdHttpMethod

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

-- | Request to describe an existing 'Method' resource.
--
--
--
-- /See:/ 'getMethod' smart constructor.
data GetMethod = GetMethod'{_gtmthdRestAPIId ::
                            !Text,
                            _gtmthdResourceId :: !Text,
                            _gtmthdHttpMethod :: !Text}
                   deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtmthdRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gtmthdResourceId' - [Required] The 'Resource' identifier for the 'Method' resource.
--
-- * 'gtmthdHttpMethod' - [Required] Specifies the method request's HTTP method type.
getMethod
    :: Text -- ^ 'gtmthdRestAPIId'
    -> Text -- ^ 'gtmthdResourceId'
    -> Text -- ^ 'gtmthdHttpMethod'
    -> GetMethod
getMethod pRestAPIId_ pResourceId_ pHttpMethod_
  = GetMethod'{_gtmthdRestAPIId = pRestAPIId_,
               _gtmthdResourceId = pResourceId_,
               _gtmthdHttpMethod = pHttpMethod_}

-- | [Required] The string identifier of the associated 'RestApi' .
gtmthdRestAPIId :: Lens' GetMethod Text
gtmthdRestAPIId = lens _gtmthdRestAPIId (\ s a -> s{_gtmthdRestAPIId = a})

-- | [Required] The 'Resource' identifier for the 'Method' resource.
gtmthdResourceId :: Lens' GetMethod Text
gtmthdResourceId = lens _gtmthdResourceId (\ s a -> s{_gtmthdResourceId = a})

-- | [Required] Specifies the method request's HTTP method type.
gtmthdHttpMethod :: Lens' GetMethod Text
gtmthdHttpMethod = lens _gtmthdHttpMethod (\ s a -> s{_gtmthdHttpMethod = a})

instance AWSRequest GetMethod where
        type Rs GetMethod = Method
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetMethod where

instance NFData GetMethod where

instance ToHeaders GetMethod where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetMethod where
        toPath GetMethod'{..}
          = mconcat
              ["/restapis/", toBS _gtmthdRestAPIId, "/resources/",
               toBS _gtmthdResourceId, "/methods/",
               toBS _gtmthdHttpMethod]

instance ToQuery GetMethod where
        toQuery = const mempty
