{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserStatusType (
  UserStatusType (
    ..
    , Archived
    , Compromised
    , Confirmed
    , ForceChangePassword
    , ResetRequired
    , Unconfirmed
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserStatusType = UserStatusType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Archived :: UserStatusType
pattern Archived = UserStatusType' "ARCHIVED"

pattern Compromised :: UserStatusType
pattern Compromised = UserStatusType' "COMPROMISED"

pattern Confirmed :: UserStatusType
pattern Confirmed = UserStatusType' "CONFIRMED"

pattern ForceChangePassword :: UserStatusType
pattern ForceChangePassword = UserStatusType' "FORCE_CHANGE_PASSWORD"

pattern ResetRequired :: UserStatusType
pattern ResetRequired = UserStatusType' "RESET_REQUIRED"

pattern Unconfirmed :: UserStatusType
pattern Unconfirmed = UserStatusType' "UNCONFIRMED"

pattern Unknown :: UserStatusType
pattern Unknown = UserStatusType' "UNKNOWN"

{-# COMPLETE
  Archived,
  Compromised,
  Confirmed,
  ForceChangePassword,
  ResetRequired,
  Unconfirmed,
  Unknown,
  UserStatusType' #-}

instance FromText UserStatusType where
    parser = (UserStatusType' . mk) <$> takeText

instance ToText UserStatusType where
    toText (UserStatusType' ci) = original ci

-- | Represents an enum of /known/ $UserStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserStatusType where
    toEnum i = case i of
        0 -> Archived
        1 -> Compromised
        2 -> Confirmed
        3 -> ForceChangePassword
        4 -> ResetRequired
        5 -> Unconfirmed
        6 -> Unknown
        _ -> (error . showText) $ "Unknown index for UserStatusType: " <> toText i
    fromEnum x = case x of
        Archived -> 0
        Compromised -> 1
        Confirmed -> 2
        ForceChangePassword -> 3
        ResetRequired -> 4
        Unconfirmed -> 5
        Unknown -> 6
        UserStatusType' name -> (error . showText) $ "Unknown UserStatusType: " <> original name

-- | Represents the bounds of /known/ $UserStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserStatusType where
    minBound = Archived
    maxBound = Unknown

instance Hashable     UserStatusType
instance NFData       UserStatusType
instance ToByteString UserStatusType
instance ToQuery      UserStatusType
instance ToHeader     UserStatusType

instance FromJSON UserStatusType where
    parseJSON = parseJSONText "UserStatusType"
