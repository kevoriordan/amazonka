{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPart
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.DocumentationPart where

import Network.AWS.APIGateway.Types.DocumentationPartLocation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A documentation part for a targeted API entity.
--
--
-- A documentation part consists of a content map (@properties@ ) and a target (@location@ ). The target specifies an API entity to which the documentation content applies. The supported API entity types are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Valid @location@ fields depend on the API entity type. All valid fields are not required.
--
-- The content map is a JSON string of API-specific key-value pairs. Although an API can use any shape for the content map, only the OpenAPI-compliant documentation fields will be injected into the associated API entity definition in the exported OpenAPI definition file.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationParts' 
--
-- /See:/ 'documentationPart' smart constructor.
data DocumentationPart = DocumentationPart'{_dpLocation
                                            ::
                                            !(Maybe DocumentationPartLocation),
                                            _dpId :: !(Maybe Text),
                                            _dpProperties :: !(Maybe Text)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentationPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpLocation' - The location of the API entity to which the documentation applies. Valid fields depend on the targeted API entity type. All the valid location fields are not required. If not explicitly specified, a valid location field is treated as a wildcard and associated documentation content may be inherited by matching entities, unless overridden.
--
-- * 'dpId' - The 'DocumentationPart' identifier, generated by API Gateway when the @DocumentationPart@ is created.
--
-- * 'dpProperties' - A content map of API-specific key-value pairs describing the targeted API entity. The map must be encoded as a JSON string, e.g., @"{ \"description\": \"The API does ...\" }"@ . Only OpenAPI-compliant documentation-related fields from the @properties@ map are exported and, hence, published as part of the API entity definitions, while the original documentation parts are exported in a OpenAPI extension of @x-amazon-apigateway-documentation@ .
documentationPart
    :: DocumentationPart
documentationPart
  = DocumentationPart'{_dpLocation = Nothing,
                       _dpId = Nothing, _dpProperties = Nothing}

-- | The location of the API entity to which the documentation applies. Valid fields depend on the targeted API entity type. All the valid location fields are not required. If not explicitly specified, a valid location field is treated as a wildcard and associated documentation content may be inherited by matching entities, unless overridden.
dpLocation :: Lens' DocumentationPart (Maybe DocumentationPartLocation)
dpLocation = lens _dpLocation (\ s a -> s{_dpLocation = a})

-- | The 'DocumentationPart' identifier, generated by API Gateway when the @DocumentationPart@ is created.
dpId :: Lens' DocumentationPart (Maybe Text)
dpId = lens _dpId (\ s a -> s{_dpId = a})

-- | A content map of API-specific key-value pairs describing the targeted API entity. The map must be encoded as a JSON string, e.g., @"{ \"description\": \"The API does ...\" }"@ . Only OpenAPI-compliant documentation-related fields from the @properties@ map are exported and, hence, published as part of the API entity definitions, while the original documentation parts are exported in a OpenAPI extension of @x-amazon-apigateway-documentation@ .
dpProperties :: Lens' DocumentationPart (Maybe Text)
dpProperties = lens _dpProperties (\ s a -> s{_dpProperties = a})

instance FromJSON DocumentationPart where
        parseJSON
          = withObject "DocumentationPart"
              (\ x ->
                 DocumentationPart' <$>
                   (x .:? "location") <*> (x .:? "id") <*>
                     (x .:? "properties"))

instance Hashable DocumentationPart where

instance NFData DocumentationPart where