{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey (
  AccessLevelFilterKey (
    ..
    , Account
    , Role
    , User
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccessLevelFilterKey = AccessLevelFilterKey' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Account :: AccessLevelFilterKey
pattern Account = AccessLevelFilterKey' "Account"

pattern Role :: AccessLevelFilterKey
pattern Role = AccessLevelFilterKey' "Role"

pattern User :: AccessLevelFilterKey
pattern User = AccessLevelFilterKey' "User"

{-# COMPLETE
  Account,
  Role,
  User,
  AccessLevelFilterKey' #-}

instance FromText AccessLevelFilterKey where
    parser = (AccessLevelFilterKey' . mk) <$> takeText

instance ToText AccessLevelFilterKey where
    toText (AccessLevelFilterKey' ci) = original ci

-- | Represents an enum of /known/ $AccessLevelFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccessLevelFilterKey where
    toEnum i = case i of
        0 -> Account
        1 -> Role
        2 -> User
        _ -> (error . showText) $ "Unknown index for AccessLevelFilterKey: " <> toText i
    fromEnum x = case x of
        Account -> 0
        Role -> 1
        User -> 2
        AccessLevelFilterKey' name -> (error . showText) $ "Unknown AccessLevelFilterKey: " <> original name

-- | Represents the bounds of /known/ $AccessLevelFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccessLevelFilterKey where
    minBound = Account
    maxBound = User

instance Hashable     AccessLevelFilterKey
instance NFData       AccessLevelFilterKey
instance ToByteString AccessLevelFilterKey
instance ToQuery      AccessLevelFilterKey
instance ToHeader     AccessLevelFilterKey

instance ToJSON AccessLevelFilterKey where
    toJSON = toJSONText
