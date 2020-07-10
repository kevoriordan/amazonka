{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.AuthenticationType (
  AuthenticationType (
    ..
    , ATAPIKey
    , ATAWSIAM
    , ATAmazonCognitoUserPools
    , ATOpenidConnect
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthenticationType = AuthenticationType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern ATAPIKey :: AuthenticationType
pattern ATAPIKey = AuthenticationType' "API_KEY"

pattern ATAWSIAM :: AuthenticationType
pattern ATAWSIAM = AuthenticationType' "AWS_IAM"

pattern ATAmazonCognitoUserPools :: AuthenticationType
pattern ATAmazonCognitoUserPools = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern ATOpenidConnect :: AuthenticationType
pattern ATOpenidConnect = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE
  ATAPIKey,
  ATAWSIAM,
  ATAmazonCognitoUserPools,
  ATOpenidConnect,
  AuthenticationType' #-}

instance FromText AuthenticationType where
    parser = (AuthenticationType' . mk) <$> takeText

instance ToText AuthenticationType where
    toText (AuthenticationType' ci) = original ci

-- | Represents an enum of /known/ $AuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthenticationType where
    toEnum i = case i of
        0 -> ATAPIKey
        1 -> ATAWSIAM
        2 -> ATAmazonCognitoUserPools
        3 -> ATOpenidConnect
        _ -> (error . showText) $ "Unknown index for AuthenticationType: " <> toText i
    fromEnum x = case x of
        ATAPIKey -> 0
        ATAWSIAM -> 1
        ATAmazonCognitoUserPools -> 2
        ATOpenidConnect -> 3
        AuthenticationType' name -> (error . showText) $ "Unknown AuthenticationType: " <> original name

-- | Represents the bounds of /known/ $AuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthenticationType where
    minBound = ATAPIKey
    maxBound = ATOpenidConnect

instance Hashable     AuthenticationType
instance NFData       AuthenticationType
instance ToByteString AuthenticationType
instance ToQuery      AuthenticationType
instance ToHeader     AuthenticationType

instance ToJSON AuthenticationType where
    toJSON = toJSONText

instance FromJSON AuthenticationType where
    parseJSON = parseJSONText "AuthenticationType"
