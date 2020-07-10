{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType (
  AmbiguousRoleResolutionType (
    ..
    , AuthenticatedRole
    , Deny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AmbiguousRoleResolutionType = AmbiguousRoleResolutionType' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern AuthenticatedRole :: AmbiguousRoleResolutionType
pattern AuthenticatedRole = AmbiguousRoleResolutionType' "AuthenticatedRole"

pattern Deny :: AmbiguousRoleResolutionType
pattern Deny = AmbiguousRoleResolutionType' "Deny"

{-# COMPLETE
  AuthenticatedRole,
  Deny,
  AmbiguousRoleResolutionType' #-}

instance FromText AmbiguousRoleResolutionType where
    parser = (AmbiguousRoleResolutionType' . mk) <$> takeText

instance ToText AmbiguousRoleResolutionType where
    toText (AmbiguousRoleResolutionType' ci) = original ci

-- | Represents an enum of /known/ $AmbiguousRoleResolutionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AmbiguousRoleResolutionType where
    toEnum i = case i of
        0 -> AuthenticatedRole
        1 -> Deny
        _ -> (error . showText) $ "Unknown index for AmbiguousRoleResolutionType: " <> toText i
    fromEnum x = case x of
        AuthenticatedRole -> 0
        Deny -> 1
        AmbiguousRoleResolutionType' name -> (error . showText) $ "Unknown AmbiguousRoleResolutionType: " <> original name

-- | Represents the bounds of /known/ $AmbiguousRoleResolutionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AmbiguousRoleResolutionType where
    minBound = AuthenticatedRole
    maxBound = Deny

instance Hashable     AmbiguousRoleResolutionType
instance NFData       AmbiguousRoleResolutionType
instance ToByteString AmbiguousRoleResolutionType
instance ToQuery      AmbiguousRoleResolutionType
instance ToHeader     AmbiguousRoleResolutionType

instance ToJSON AmbiguousRoleResolutionType where
    toJSON = toJSONText

instance FromJSON AmbiguousRoleResolutionType where
    parseJSON = parseJSONText "AmbiguousRoleResolutionType"
