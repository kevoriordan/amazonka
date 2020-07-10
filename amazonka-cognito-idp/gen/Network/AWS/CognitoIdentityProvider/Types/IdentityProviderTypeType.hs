{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType (
  IdentityProviderTypeType (
    ..
    , Facebook
    , Google
    , LoginWithAmazon
    , Oidc
    , Saml
    , SignInWithApple
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IdentityProviderTypeType = IdentityProviderTypeType' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Facebook :: IdentityProviderTypeType
pattern Facebook = IdentityProviderTypeType' "Facebook"

pattern Google :: IdentityProviderTypeType
pattern Google = IdentityProviderTypeType' "Google"

pattern LoginWithAmazon :: IdentityProviderTypeType
pattern LoginWithAmazon = IdentityProviderTypeType' "LoginWithAmazon"

pattern Oidc :: IdentityProviderTypeType
pattern Oidc = IdentityProviderTypeType' "OIDC"

pattern Saml :: IdentityProviderTypeType
pattern Saml = IdentityProviderTypeType' "SAML"

pattern SignInWithApple :: IdentityProviderTypeType
pattern SignInWithApple = IdentityProviderTypeType' "SignInWithApple"

{-# COMPLETE
  Facebook,
  Google,
  LoginWithAmazon,
  Oidc,
  Saml,
  SignInWithApple,
  IdentityProviderTypeType' #-}

instance FromText IdentityProviderTypeType where
    parser = (IdentityProviderTypeType' . mk) <$> takeText

instance ToText IdentityProviderTypeType where
    toText (IdentityProviderTypeType' ci) = original ci

-- | Represents an enum of /known/ $IdentityProviderTypeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IdentityProviderTypeType where
    toEnum i = case i of
        0 -> Facebook
        1 -> Google
        2 -> LoginWithAmazon
        3 -> Oidc
        4 -> Saml
        5 -> SignInWithApple
        _ -> (error . showText) $ "Unknown index for IdentityProviderTypeType: " <> toText i
    fromEnum x = case x of
        Facebook -> 0
        Google -> 1
        LoginWithAmazon -> 2
        Oidc -> 3
        Saml -> 4
        SignInWithApple -> 5
        IdentityProviderTypeType' name -> (error . showText) $ "Unknown IdentityProviderTypeType: " <> original name

-- | Represents the bounds of /known/ $IdentityProviderTypeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IdentityProviderTypeType where
    minBound = Facebook
    maxBound = SignInWithApple

instance Hashable     IdentityProviderTypeType
instance NFData       IdentityProviderTypeType
instance ToByteString IdentityProviderTypeType
instance ToQuery      IdentityProviderTypeType
instance ToHeader     IdentityProviderTypeType

instance ToJSON IdentityProviderTypeType where
    toJSON = toJSONText

instance FromJSON IdentityProviderTypeType where
    parseJSON = parseJSONText "IdentityProviderTypeType"
