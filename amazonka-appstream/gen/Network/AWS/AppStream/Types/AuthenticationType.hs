{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.AuthenticationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.AuthenticationType (
  AuthenticationType (
    ..
    , API
    , Saml
    , Userpool
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthenticationType = AuthenticationType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern API :: AuthenticationType
pattern API = AuthenticationType' "API"

pattern Saml :: AuthenticationType
pattern Saml = AuthenticationType' "SAML"

pattern Userpool :: AuthenticationType
pattern Userpool = AuthenticationType' "USERPOOL"

{-# COMPLETE
  API,
  Saml,
  Userpool,
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
        0 -> API
        1 -> Saml
        2 -> Userpool
        _ -> (error . showText) $ "Unknown index for AuthenticationType: " <> toText i
    fromEnum x = case x of
        API -> 0
        Saml -> 1
        Userpool -> 2
        AuthenticationType' name -> (error . showText) $ "Unknown AuthenticationType: " <> original name

-- | Represents the bounds of /known/ $AuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthenticationType where
    minBound = API
    maxBound = Userpool

instance Hashable     AuthenticationType
instance NFData       AuthenticationType
instance ToByteString AuthenticationType
instance ToQuery      AuthenticationType
instance ToHeader     AuthenticationType

instance ToJSON AuthenticationType where
    toJSON = toJSONText

instance FromJSON AuthenticationType where
    parseJSON = parseJSONText "AuthenticationType"
