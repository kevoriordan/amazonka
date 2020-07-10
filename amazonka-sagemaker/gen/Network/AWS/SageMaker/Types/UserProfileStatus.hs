{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.UserProfileStatus (
  UserProfileStatus (
    ..
    , UPSDeleting
    , UPSFailed
    , UPSInService
    , UPSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserProfileStatus = UserProfileStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern UPSDeleting :: UserProfileStatus
pattern UPSDeleting = UserProfileStatus' "Deleting"

pattern UPSFailed :: UserProfileStatus
pattern UPSFailed = UserProfileStatus' "Failed"

pattern UPSInService :: UserProfileStatus
pattern UPSInService = UserProfileStatus' "InService"

pattern UPSPending :: UserProfileStatus
pattern UPSPending = UserProfileStatus' "Pending"

{-# COMPLETE
  UPSDeleting,
  UPSFailed,
  UPSInService,
  UPSPending,
  UserProfileStatus' #-}

instance FromText UserProfileStatus where
    parser = (UserProfileStatus' . mk) <$> takeText

instance ToText UserProfileStatus where
    toText (UserProfileStatus' ci) = original ci

-- | Represents an enum of /known/ $UserProfileStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserProfileStatus where
    toEnum i = case i of
        0 -> UPSDeleting
        1 -> UPSFailed
        2 -> UPSInService
        3 -> UPSPending
        _ -> (error . showText) $ "Unknown index for UserProfileStatus: " <> toText i
    fromEnum x = case x of
        UPSDeleting -> 0
        UPSFailed -> 1
        UPSInService -> 2
        UPSPending -> 3
        UserProfileStatus' name -> (error . showText) $ "Unknown UserProfileStatus: " <> original name

-- | Represents the bounds of /known/ $UserProfileStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserProfileStatus where
    minBound = UPSDeleting
    maxBound = UPSPending

instance Hashable     UserProfileStatus
instance NFData       UserProfileStatus
instance ToByteString UserProfileStatus
instance ToQuery      UserProfileStatus
instance ToHeader     UserProfileStatus

instance FromJSON UserProfileStatus where
    parseJSON = parseJSONText "UserProfileStatus"
