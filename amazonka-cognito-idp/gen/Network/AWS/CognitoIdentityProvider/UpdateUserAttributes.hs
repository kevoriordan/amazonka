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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to update a specific attribute (one at a time).
--
--
module Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
    (
    -- * Creating a Request
      updateUserAttributes
    , UpdateUserAttributes
    -- * Request Lenses
    , uuaClientMetadata
    , uuaUserAttributes
    , uuaAccessToken

    -- * Destructuring the Response
    , updateUserAttributesResponse
    , UpdateUserAttributesResponse
    -- * Response Lenses
    , uuarsCodeDeliveryDetailsList
    , uuarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update user attributes.
--
--
--
-- /See:/ 'updateUserAttributes' smart constructor.
data UpdateUserAttributes = UpdateUserAttributes'{_uuaClientMetadata
                                                  :: !(Maybe (Map Text Text)),
                                                  _uuaUserAttributes ::
                                                  ![AttributeType],
                                                  _uuaAccessToken ::
                                                  !(Sensitive Text)}
                              deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuaClientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the UpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your UpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- * 'uuaUserAttributes' - An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- * 'uuaAccessToken' - The access token for the request to update user attributes.
updateUserAttributes
    :: Text -- ^ 'uuaAccessToken'
    -> UpdateUserAttributes
updateUserAttributes pAccessToken_
  = UpdateUserAttributes'{_uuaClientMetadata = Nothing,
                          _uuaUserAttributes = mempty,
                          _uuaAccessToken = _Sensitive # pAccessToken_}

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the UpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your UpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
uuaClientMetadata :: Lens' UpdateUserAttributes (HashMap Text Text)
uuaClientMetadata = lens _uuaClientMetadata (\ s a -> s{_uuaClientMetadata = a}) . _Default . _Map

-- | An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
uuaUserAttributes :: Lens' UpdateUserAttributes [AttributeType]
uuaUserAttributes = lens _uuaUserAttributes (\ s a -> s{_uuaUserAttributes = a}) . _Coerce

-- | The access token for the request to update user attributes.
uuaAccessToken :: Lens' UpdateUserAttributes Text
uuaAccessToken = lens _uuaAccessToken (\ s a -> s{_uuaAccessToken = a}) . _Sensitive

instance AWSRequest UpdateUserAttributes where
        type Rs UpdateUserAttributes =
             UpdateUserAttributesResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserAttributesResponse' <$>
                   (x .?> "CodeDeliveryDetailsList" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable UpdateUserAttributes where

instance NFData UpdateUserAttributes where

instance ToHeaders UpdateUserAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserAttributes where
        toJSON UpdateUserAttributes'{..}
          = object
              (catMaybes
                 [("ClientMetadata" .=) <$> _uuaClientMetadata,
                  Just ("UserAttributes" .= _uuaUserAttributes),
                  Just ("AccessToken" .= _uuaAccessToken)])

instance ToPath UpdateUserAttributes where
        toPath = const "/"

instance ToQuery UpdateUserAttributes where
        toQuery = const mempty

-- | Represents the response from the server for the request to update user attributes.
--
--
--
-- /See:/ 'updateUserAttributesResponse' smart constructor.
data UpdateUserAttributesResponse = UpdateUserAttributesResponse'{_uuarsCodeDeliveryDetailsList
                                                                  ::
                                                                  !(Maybe
                                                                      [CodeDeliveryDetailsType]),
                                                                  _uuarsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'UpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuarsCodeDeliveryDetailsList' - The code delivery details list from the server for the request to update user attributes.
--
-- * 'uuarsResponseStatus' - -- | The response status code.
updateUserAttributesResponse
    :: Int -- ^ 'uuarsResponseStatus'
    -> UpdateUserAttributesResponse
updateUserAttributesResponse pResponseStatus_
  = UpdateUserAttributesResponse'{_uuarsCodeDeliveryDetailsList
                                    = Nothing,
                                  _uuarsResponseStatus = pResponseStatus_}

-- | The code delivery details list from the server for the request to update user attributes.
uuarsCodeDeliveryDetailsList :: Lens' UpdateUserAttributesResponse [CodeDeliveryDetailsType]
uuarsCodeDeliveryDetailsList = lens _uuarsCodeDeliveryDetailsList (\ s a -> s{_uuarsCodeDeliveryDetailsList = a}) . _Default . _Coerce

-- | -- | The response status code.
uuarsResponseStatus :: Lens' UpdateUserAttributesResponse Int
uuarsResponseStatus = lens _uuarsResponseStatus (\ s a -> s{_uuarsResponseStatus = a})

instance NFData UpdateUserAttributesResponse where
