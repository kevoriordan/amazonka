{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UserType (
  UserType (
    ..
    , UTAdmin
    , UTMinimaluser
    , UTPoweruser
    , UTUser
    , UTWorkspacesuser
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserType = UserType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern UTAdmin :: UserType
pattern UTAdmin = UserType' "ADMIN"

pattern UTMinimaluser :: UserType
pattern UTMinimaluser = UserType' "MINIMALUSER"

pattern UTPoweruser :: UserType
pattern UTPoweruser = UserType' "POWERUSER"

pattern UTUser :: UserType
pattern UTUser = UserType' "USER"

pattern UTWorkspacesuser :: UserType
pattern UTWorkspacesuser = UserType' "WORKSPACESUSER"

{-# COMPLETE
  UTAdmin,
  UTMinimaluser,
  UTPoweruser,
  UTUser,
  UTWorkspacesuser,
  UserType' #-}

instance FromText UserType where
    parser = (UserType' . mk) <$> takeText

instance ToText UserType where
    toText (UserType' ci) = original ci

-- | Represents an enum of /known/ $UserType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserType where
    toEnum i = case i of
        0 -> UTAdmin
        1 -> UTMinimaluser
        2 -> UTPoweruser
        3 -> UTUser
        4 -> UTWorkspacesuser
        _ -> (error . showText) $ "Unknown index for UserType: " <> toText i
    fromEnum x = case x of
        UTAdmin -> 0
        UTMinimaluser -> 1
        UTPoweruser -> 2
        UTUser -> 3
        UTWorkspacesuser -> 4
        UserType' name -> (error . showText) $ "Unknown UserType: " <> original name

-- | Represents the bounds of /known/ $UserType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserType where
    minBound = UTAdmin
    maxBound = UTWorkspacesuser

instance Hashable     UserType
instance NFData       UserType
instance ToByteString UserType
instance ToQuery      UserType
instance ToHeader     UserType

instance ToJSON UserType where
    toJSON = toJSONText

instance FromJSON UserType where
    parseJSON = parseJSONText "UserType"
