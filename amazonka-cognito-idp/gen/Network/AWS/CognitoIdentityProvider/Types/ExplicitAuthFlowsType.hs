{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType (
  ExplicitAuthFlowsType (
    ..
    , EAFTAdminNoSrpAuth
    , EAFTAllowAdminUserPasswordAuth
    , EAFTAllowCustomAuth
    , EAFTAllowRefreshTokenAuth
    , EAFTAllowUserPasswordAuth
    , EAFTAllowUserSrpAuth
    , EAFTCustomAuthFlowOnly
    , EAFTUserPasswordAuth
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ExplicitAuthFlowsType = ExplicitAuthFlowsType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern EAFTAdminNoSrpAuth :: ExplicitAuthFlowsType
pattern EAFTAdminNoSrpAuth = ExplicitAuthFlowsType' "ADMIN_NO_SRP_AUTH"

pattern EAFTAllowAdminUserPasswordAuth :: ExplicitAuthFlowsType
pattern EAFTAllowAdminUserPasswordAuth = ExplicitAuthFlowsType' "ALLOW_ADMIN_USER_PASSWORD_AUTH"

pattern EAFTAllowCustomAuth :: ExplicitAuthFlowsType
pattern EAFTAllowCustomAuth = ExplicitAuthFlowsType' "ALLOW_CUSTOM_AUTH"

pattern EAFTAllowRefreshTokenAuth :: ExplicitAuthFlowsType
pattern EAFTAllowRefreshTokenAuth = ExplicitAuthFlowsType' "ALLOW_REFRESH_TOKEN_AUTH"

pattern EAFTAllowUserPasswordAuth :: ExplicitAuthFlowsType
pattern EAFTAllowUserPasswordAuth = ExplicitAuthFlowsType' "ALLOW_USER_PASSWORD_AUTH"

pattern EAFTAllowUserSrpAuth :: ExplicitAuthFlowsType
pattern EAFTAllowUserSrpAuth = ExplicitAuthFlowsType' "ALLOW_USER_SRP_AUTH"

pattern EAFTCustomAuthFlowOnly :: ExplicitAuthFlowsType
pattern EAFTCustomAuthFlowOnly = ExplicitAuthFlowsType' "CUSTOM_AUTH_FLOW_ONLY"

pattern EAFTUserPasswordAuth :: ExplicitAuthFlowsType
pattern EAFTUserPasswordAuth = ExplicitAuthFlowsType' "USER_PASSWORD_AUTH"

{-# COMPLETE
  EAFTAdminNoSrpAuth,
  EAFTAllowAdminUserPasswordAuth,
  EAFTAllowCustomAuth,
  EAFTAllowRefreshTokenAuth,
  EAFTAllowUserPasswordAuth,
  EAFTAllowUserSrpAuth,
  EAFTCustomAuthFlowOnly,
  EAFTUserPasswordAuth,
  ExplicitAuthFlowsType' #-}

instance FromText ExplicitAuthFlowsType where
    parser = (ExplicitAuthFlowsType' . mk) <$> takeText

instance ToText ExplicitAuthFlowsType where
    toText (ExplicitAuthFlowsType' ci) = original ci

-- | Represents an enum of /known/ $ExplicitAuthFlowsType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExplicitAuthFlowsType where
    toEnum i = case i of
        0 -> EAFTAdminNoSrpAuth
        1 -> EAFTAllowAdminUserPasswordAuth
        2 -> EAFTAllowCustomAuth
        3 -> EAFTAllowRefreshTokenAuth
        4 -> EAFTAllowUserPasswordAuth
        5 -> EAFTAllowUserSrpAuth
        6 -> EAFTCustomAuthFlowOnly
        7 -> EAFTUserPasswordAuth
        _ -> (error . showText) $ "Unknown index for ExplicitAuthFlowsType: " <> toText i
    fromEnum x = case x of
        EAFTAdminNoSrpAuth -> 0
        EAFTAllowAdminUserPasswordAuth -> 1
        EAFTAllowCustomAuth -> 2
        EAFTAllowRefreshTokenAuth -> 3
        EAFTAllowUserPasswordAuth -> 4
        EAFTAllowUserSrpAuth -> 5
        EAFTCustomAuthFlowOnly -> 6
        EAFTUserPasswordAuth -> 7
        ExplicitAuthFlowsType' name -> (error . showText) $ "Unknown ExplicitAuthFlowsType: " <> original name

-- | Represents the bounds of /known/ $ExplicitAuthFlowsType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExplicitAuthFlowsType where
    minBound = EAFTAdminNoSrpAuth
    maxBound = EAFTUserPasswordAuth

instance Hashable     ExplicitAuthFlowsType
instance NFData       ExplicitAuthFlowsType
instance ToByteString ExplicitAuthFlowsType
instance ToQuery      ExplicitAuthFlowsType
instance ToHeader     ExplicitAuthFlowsType

instance ToJSON ExplicitAuthFlowsType where
    toJSON = toJSONText

instance FromJSON ExplicitAuthFlowsType where
    parseJSON = parseJSONText "ExplicitAuthFlowsType"
