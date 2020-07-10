{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AuthFlowType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.AuthFlowType (
  AuthFlowType (
    ..
    , AdminNoSrpAuth
    , AdminUserPasswordAuth
    , CustomAuth
    , RefreshToken
    , RefreshTokenAuth
    , UserPasswordAuth
    , UserSrpAuth
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthFlowType = AuthFlowType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AdminNoSrpAuth :: AuthFlowType
pattern AdminNoSrpAuth = AuthFlowType' "ADMIN_NO_SRP_AUTH"

pattern AdminUserPasswordAuth :: AuthFlowType
pattern AdminUserPasswordAuth = AuthFlowType' "ADMIN_USER_PASSWORD_AUTH"

pattern CustomAuth :: AuthFlowType
pattern CustomAuth = AuthFlowType' "CUSTOM_AUTH"

pattern RefreshToken :: AuthFlowType
pattern RefreshToken = AuthFlowType' "REFRESH_TOKEN"

pattern RefreshTokenAuth :: AuthFlowType
pattern RefreshTokenAuth = AuthFlowType' "REFRESH_TOKEN_AUTH"

pattern UserPasswordAuth :: AuthFlowType
pattern UserPasswordAuth = AuthFlowType' "USER_PASSWORD_AUTH"

pattern UserSrpAuth :: AuthFlowType
pattern UserSrpAuth = AuthFlowType' "USER_SRP_AUTH"

{-# COMPLETE
  AdminNoSrpAuth,
  AdminUserPasswordAuth,
  CustomAuth,
  RefreshToken,
  RefreshTokenAuth,
  UserPasswordAuth,
  UserSrpAuth,
  AuthFlowType' #-}

instance FromText AuthFlowType where
    parser = (AuthFlowType' . mk) <$> takeText

instance ToText AuthFlowType where
    toText (AuthFlowType' ci) = original ci

-- | Represents an enum of /known/ $AuthFlowType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthFlowType where
    toEnum i = case i of
        0 -> AdminNoSrpAuth
        1 -> AdminUserPasswordAuth
        2 -> CustomAuth
        3 -> RefreshToken
        4 -> RefreshTokenAuth
        5 -> UserPasswordAuth
        6 -> UserSrpAuth
        _ -> (error . showText) $ "Unknown index for AuthFlowType: " <> toText i
    fromEnum x = case x of
        AdminNoSrpAuth -> 0
        AdminUserPasswordAuth -> 1
        CustomAuth -> 2
        RefreshToken -> 3
        RefreshTokenAuth -> 4
        UserPasswordAuth -> 5
        UserSrpAuth -> 6
        AuthFlowType' name -> (error . showText) $ "Unknown AuthFlowType: " <> original name

-- | Represents the bounds of /known/ $AuthFlowType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthFlowType where
    minBound = AdminNoSrpAuth
    maxBound = UserSrpAuth

instance Hashable     AuthFlowType
instance NFData       AuthFlowType
instance ToByteString AuthFlowType
instance ToQuery      AuthFlowType
instance ToHeader     AuthFlowType

instance ToJSON AuthFlowType where
    toJSON = toJSONText
