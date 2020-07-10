{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.AuthType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.AuthType (
  AuthType (
    ..
    , ATBasicAuth
    , ATOauth
    , ATPersonalAccessToken
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthType = AuthType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern ATBasicAuth :: AuthType
pattern ATBasicAuth = AuthType' "BASIC_AUTH"

pattern ATOauth :: AuthType
pattern ATOauth = AuthType' "OAUTH"

pattern ATPersonalAccessToken :: AuthType
pattern ATPersonalAccessToken = AuthType' "PERSONAL_ACCESS_TOKEN"

{-# COMPLETE
  ATBasicAuth,
  ATOauth,
  ATPersonalAccessToken,
  AuthType' #-}

instance FromText AuthType where
    parser = (AuthType' . mk) <$> takeText

instance ToText AuthType where
    toText (AuthType' ci) = original ci

-- | Represents an enum of /known/ $AuthType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthType where
    toEnum i = case i of
        0 -> ATBasicAuth
        1 -> ATOauth
        2 -> ATPersonalAccessToken
        _ -> (error . showText) $ "Unknown index for AuthType: " <> toText i
    fromEnum x = case x of
        ATBasicAuth -> 0
        ATOauth -> 1
        ATPersonalAccessToken -> 2
        AuthType' name -> (error . showText) $ "Unknown AuthType: " <> original name

-- | Represents the bounds of /known/ $AuthType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthType where
    minBound = ATBasicAuth
    maxBound = ATPersonalAccessToken

instance Hashable     AuthType
instance NFData       AuthType
instance ToByteString AuthType
instance ToQuery      AuthType
instance ToHeader     AuthType

instance ToJSON AuthType where
    toJSON = toJSONText

instance FromJSON AuthType where
    parseJSON = parseJSONText "AuthType"
