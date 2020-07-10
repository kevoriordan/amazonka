{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.AuthorizerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.AuthorizerType (
  AuthorizerType (
    ..
    , AuthorizerCognitoUserPools
    , AuthorizerRequest
    , AuthorizerToken
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
--
data AuthorizerType = AuthorizerType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern AuthorizerCognitoUserPools :: AuthorizerType
pattern AuthorizerCognitoUserPools = AuthorizerType' "COGNITO_USER_POOLS"

pattern AuthorizerRequest :: AuthorizerType
pattern AuthorizerRequest = AuthorizerType' "REQUEST"

pattern AuthorizerToken :: AuthorizerType
pattern AuthorizerToken = AuthorizerType' "TOKEN"

{-# COMPLETE
  AuthorizerCognitoUserPools,
  AuthorizerRequest,
  AuthorizerToken,
  AuthorizerType' #-}

instance FromText AuthorizerType where
    parser = (AuthorizerType' . mk) <$> takeText

instance ToText AuthorizerType where
    toText (AuthorizerType' ci) = original ci

-- | Represents an enum of /known/ $AuthorizerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthorizerType where
    toEnum i = case i of
        0 -> AuthorizerCognitoUserPools
        1 -> AuthorizerRequest
        2 -> AuthorizerToken
        _ -> (error . showText) $ "Unknown index for AuthorizerType: " <> toText i
    fromEnum x = case x of
        AuthorizerCognitoUserPools -> 0
        AuthorizerRequest -> 1
        AuthorizerToken -> 2
        AuthorizerType' name -> (error . showText) $ "Unknown AuthorizerType: " <> original name

-- | Represents the bounds of /known/ $AuthorizerType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthorizerType where
    minBound = AuthorizerCognitoUserPools
    maxBound = AuthorizerToken

instance Hashable     AuthorizerType
instance NFData       AuthorizerType
instance ToByteString AuthorizerType
instance ToQuery      AuthorizerType
instance ToHeader     AuthorizerType

instance ToJSON AuthorizerType where
    toJSON = toJSONText

instance FromJSON AuthorizerType where
    parseJSON = parseJSONText "AuthorizerType"
