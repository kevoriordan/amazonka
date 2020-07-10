{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AccountRoleStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types.AccountRoleStatus (
  AccountRoleStatus (
    ..
    , Creating
    , Deleted
    , Deleting
    , PendingDeletion
    , Ready
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AccountRoleStatus = AccountRoleStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Creating :: AccountRoleStatus
pattern Creating = AccountRoleStatus' "CREATING"

pattern Deleted :: AccountRoleStatus
pattern Deleted = AccountRoleStatus' "DELETED"

pattern Deleting :: AccountRoleStatus
pattern Deleting = AccountRoleStatus' "DELETING"

pattern PendingDeletion :: AccountRoleStatus
pattern PendingDeletion = AccountRoleStatus' "PENDING_DELETION"

pattern Ready :: AccountRoleStatus
pattern Ready = AccountRoleStatus' "READY"

{-# COMPLETE
  Creating,
  Deleted,
  Deleting,
  PendingDeletion,
  Ready,
  AccountRoleStatus' #-}

instance FromText AccountRoleStatus where
    parser = (AccountRoleStatus' . mk) <$> takeText

instance ToText AccountRoleStatus where
    toText (AccountRoleStatus' ci) = original ci

-- | Represents an enum of /known/ $AccountRoleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AccountRoleStatus where
    toEnum i = case i of
        0 -> Creating
        1 -> Deleted
        2 -> Deleting
        3 -> PendingDeletion
        4 -> Ready
        _ -> (error . showText) $ "Unknown index for AccountRoleStatus: " <> toText i
    fromEnum x = case x of
        Creating -> 0
        Deleted -> 1
        Deleting -> 2
        PendingDeletion -> 3
        Ready -> 4
        AccountRoleStatus' name -> (error . showText) $ "Unknown AccountRoleStatus: " <> original name

-- | Represents the bounds of /known/ $AccountRoleStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AccountRoleStatus where
    minBound = Creating
    maxBound = Ready

instance Hashable     AccountRoleStatus
instance NFData       AccountRoleStatus
instance ToByteString AccountRoleStatus
instance ToQuery      AccountRoleStatus
instance ToHeader     AccountRoleStatus

instance FromJSON AccountRoleStatus where
    parseJSON = parseJSONText "AccountRoleStatus"
