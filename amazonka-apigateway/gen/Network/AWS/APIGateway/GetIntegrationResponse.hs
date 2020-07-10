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
-- Module      : Network.AWS.APIGateway.GetIntegrationResponse
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a get integration response.
--
--
module Network.AWS.APIGateway.GetIntegrationResponse
    (
    -- * Creating a Request
      getIntegrationResponse
    , GetIntegrationResponse
    -- * Request Lenses
    , gtintgrtnRestAPIId
    , gtintgrtnResourceId
    , gtintgrtnHttpMethod
    , gtintgrtnStatusCode

    -- * Destructuring the Response
    , integrationResponse
    , IntegrationResponse
    -- * Response Lenses
    , intContentHandling
    , intResponseTemplates
    , intSelectionPattern
    , intStatusCode
    , intResponseParameters
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a get integration response request.
--
--
--
-- /See:/ 'getIntegrationResponse' smart constructor.
data GetIntegrationResponse = GetIntegrationResponse'{_gtintgrtnRestAPIId
                                                      :: !Text,
                                                      _gtintgrtnResourceId ::
                                                      !Text,
                                                      _gtintgrtnHttpMethod ::
                                                      !Text,
                                                      _gtintgrtnStatusCode ::
                                                      !Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'GetIntegrationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtintgrtnRestAPIId' - [Required] The string identifier of the associated 'RestApi' .
--
-- * 'gtintgrtnResourceId' - [Required] Specifies a get integration response request's resource identifier.
--
-- * 'gtintgrtnHttpMethod' - [Required] Specifies a get integration response request's HTTP method.
--
-- * 'gtintgrtnStatusCode' - [Required] Specifies a get integration response request's status code.
getIntegrationResponse
    :: Text -- ^ 'gtintgrtnRestAPIId'
    -> Text -- ^ 'gtintgrtnResourceId'
    -> Text -- ^ 'gtintgrtnHttpMethod'
    -> Text -- ^ 'gtintgrtnStatusCode'
    -> GetIntegrationResponse
getIntegrationResponse pRestAPIId_ pResourceId_
  pHttpMethod_ pStatusCode_
  = GetIntegrationResponse'{_gtintgrtnRestAPIId =
                              pRestAPIId_,
                            _gtintgrtnResourceId = pResourceId_,
                            _gtintgrtnHttpMethod = pHttpMethod_,
                            _gtintgrtnStatusCode = pStatusCode_}

-- | [Required] The string identifier of the associated 'RestApi' .
gtintgrtnRestAPIId :: Lens' GetIntegrationResponse Text
gtintgrtnRestAPIId = lens _gtintgrtnRestAPIId (\ s a -> s{_gtintgrtnRestAPIId = a})

-- | [Required] Specifies a get integration response request's resource identifier.
gtintgrtnResourceId :: Lens' GetIntegrationResponse Text
gtintgrtnResourceId = lens _gtintgrtnResourceId (\ s a -> s{_gtintgrtnResourceId = a})

-- | [Required] Specifies a get integration response request's HTTP method.
gtintgrtnHttpMethod :: Lens' GetIntegrationResponse Text
gtintgrtnHttpMethod = lens _gtintgrtnHttpMethod (\ s a -> s{_gtintgrtnHttpMethod = a})

-- | [Required] Specifies a get integration response request's status code.
gtintgrtnStatusCode :: Lens' GetIntegrationResponse Text
gtintgrtnStatusCode = lens _gtintgrtnStatusCode (\ s a -> s{_gtintgrtnStatusCode = a})

instance AWSRequest GetIntegrationResponse where
        type Rs GetIntegrationResponse = IntegrationResponse
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetIntegrationResponse where

instance NFData GetIntegrationResponse where

instance ToHeaders GetIntegrationResponse where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetIntegrationResponse where
        toPath GetIntegrationResponse'{..}
          = mconcat
              ["/restapis/", toBS _gtintgrtnRestAPIId,
               "/resources/", toBS _gtintgrtnResourceId,
               "/methods/", toBS _gtintgrtnHttpMethod,
               "/integration/responses/", toBS _gtintgrtnStatusCode]

instance ToQuery GetIntegrationResponse where
        toQuery = const mempty
