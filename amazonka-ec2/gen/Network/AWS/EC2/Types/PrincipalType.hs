{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrincipalType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PrincipalType (
  PrincipalType (
    ..
    , PTAccount
    , PTAll
    , PTOrganizationUnit
    , PTRole
    , PTService
    , PTUser
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PrincipalType = PrincipalType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PTAccount :: PrincipalType
pattern PTAccount = PrincipalType' "Account"

pattern PTAll :: PrincipalType
pattern PTAll = PrincipalType' "All"

pattern PTOrganizationUnit :: PrincipalType
pattern PTOrganizationUnit = PrincipalType' "OrganizationUnit"

pattern PTRole :: PrincipalType
pattern PTRole = PrincipalType' "Role"

pattern PTService :: PrincipalType
pattern PTService = PrincipalType' "Service"

pattern PTUser :: PrincipalType
pattern PTUser = PrincipalType' "User"

{-# COMPLETE
  PTAccount,
  PTAll,
  PTOrganizationUnit,
  PTRole,
  PTService,
  PTUser,
  PrincipalType' #-}

instance FromText PrincipalType where
    parser = (PrincipalType' . mk) <$> takeText

instance ToText PrincipalType where
    toText (PrincipalType' ci) = original ci

-- | Represents an enum of /known/ $PrincipalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PrincipalType where
    toEnum i = case i of
        0 -> PTAccount
        1 -> PTAll
        2 -> PTOrganizationUnit
        3 -> PTRole
        4 -> PTService
        5 -> PTUser
        _ -> (error . showText) $ "Unknown index for PrincipalType: " <> toText i
    fromEnum x = case x of
        PTAccount -> 0
        PTAll -> 1
        PTOrganizationUnit -> 2
        PTRole -> 3
        PTService -> 4
        PTUser -> 5
        PrincipalType' name -> (error . showText) $ "Unknown PrincipalType: " <> original name

-- | Represents the bounds of /known/ $PrincipalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PrincipalType where
    minBound = PTAccount
    maxBound = PTUser

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance FromXML PrincipalType where
    parseXML = parseXMLText "PrincipalType"
