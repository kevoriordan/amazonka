{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum (
  AuthenticateOidcActionConditionalBehaviorEnum (
    ..
    , AOACBEAllow
    , AOACBEAuthenticate
    , AOACBEDeny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AuthenticateOidcActionConditionalBehaviorEnum = AuthenticateOidcActionConditionalBehaviorEnum' (CI
                                                                                                       Text)
                                                       deriving (Eq, Ord, Read,
                                                                 Show, Data,
                                                                 Typeable,
                                                                 Generic)

pattern AOACBEAllow :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AOACBEAllow = AuthenticateOidcActionConditionalBehaviorEnum' "allow"

pattern AOACBEAuthenticate :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AOACBEAuthenticate = AuthenticateOidcActionConditionalBehaviorEnum' "authenticate"

pattern AOACBEDeny :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AOACBEDeny = AuthenticateOidcActionConditionalBehaviorEnum' "deny"

{-# COMPLETE
  AOACBEAllow,
  AOACBEAuthenticate,
  AOACBEDeny,
  AuthenticateOidcActionConditionalBehaviorEnum' #-}

instance FromText AuthenticateOidcActionConditionalBehaviorEnum where
    parser = (AuthenticateOidcActionConditionalBehaviorEnum' . mk) <$> takeText

instance ToText AuthenticateOidcActionConditionalBehaviorEnum where
    toText (AuthenticateOidcActionConditionalBehaviorEnum' ci) = original ci

-- | Represents an enum of /known/ $AuthenticateOidcActionConditionalBehaviorEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AuthenticateOidcActionConditionalBehaviorEnum where
    toEnum i = case i of
        0 -> AOACBEAllow
        1 -> AOACBEAuthenticate
        2 -> AOACBEDeny
        _ -> (error . showText) $ "Unknown index for AuthenticateOidcActionConditionalBehaviorEnum: " <> toText i
    fromEnum x = case x of
        AOACBEAllow -> 0
        AOACBEAuthenticate -> 1
        AOACBEDeny -> 2
        AuthenticateOidcActionConditionalBehaviorEnum' name -> (error . showText) $ "Unknown AuthenticateOidcActionConditionalBehaviorEnum: " <> original name

-- | Represents the bounds of /known/ $AuthenticateOidcActionConditionalBehaviorEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AuthenticateOidcActionConditionalBehaviorEnum where
    minBound = AOACBEAllow
    maxBound = AOACBEDeny

instance Hashable     AuthenticateOidcActionConditionalBehaviorEnum
instance NFData       AuthenticateOidcActionConditionalBehaviorEnum
instance ToByteString AuthenticateOidcActionConditionalBehaviorEnum
instance ToQuery      AuthenticateOidcActionConditionalBehaviorEnum
instance ToHeader     AuthenticateOidcActionConditionalBehaviorEnum

instance FromXML AuthenticateOidcActionConditionalBehaviorEnum where
    parseXML = parseXMLText "AuthenticateOidcActionConditionalBehaviorEnum"
