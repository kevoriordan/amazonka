{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.PrincipalType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.PrincipalType (
  PrincipalType (
    ..
    , Anonymous
    , Group
    , Invite
    , Organization
    , User
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PrincipalType = PrincipalType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Anonymous :: PrincipalType
pattern Anonymous = PrincipalType' "ANONYMOUS"

pattern Group :: PrincipalType
pattern Group = PrincipalType' "GROUP"

pattern Invite :: PrincipalType
pattern Invite = PrincipalType' "INVITE"

pattern Organization :: PrincipalType
pattern Organization = PrincipalType' "ORGANIZATION"

pattern User :: PrincipalType
pattern User = PrincipalType' "USER"

{-# COMPLETE
  Anonymous,
  Group,
  Invite,
  Organization,
  User,
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
        0 -> Anonymous
        1 -> Group
        2 -> Invite
        3 -> Organization
        4 -> User
        _ -> (error . showText) $ "Unknown index for PrincipalType: " <> toText i
    fromEnum x = case x of
        Anonymous -> 0
        Group -> 1
        Invite -> 2
        Organization -> 3
        User -> 4
        PrincipalType' name -> (error . showText) $ "Unknown PrincipalType: " <> original name

-- | Represents the bounds of /known/ $PrincipalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PrincipalType where
    minBound = Anonymous
    maxBound = User

instance Hashable     PrincipalType
instance NFData       PrincipalType
instance ToByteString PrincipalType
instance ToQuery      PrincipalType
instance ToHeader     PrincipalType

instance ToJSON PrincipalType where
    toJSON = toJSONText

instance FromJSON PrincipalType where
    parseJSON = parseJSONText "PrincipalType"
