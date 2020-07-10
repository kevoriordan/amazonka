{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserImportJobStatusType (
  UserImportJobStatusType (
    ..
    , Created
    , Expired
    , Failed
    , InProgress
    , Pending
    , Stopped
    , Stopping
    , Succeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserImportJobStatusType = UserImportJobStatusType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Created :: UserImportJobStatusType
pattern Created = UserImportJobStatusType' "Created"

pattern Expired :: UserImportJobStatusType
pattern Expired = UserImportJobStatusType' "Expired"

pattern Failed :: UserImportJobStatusType
pattern Failed = UserImportJobStatusType' "Failed"

pattern InProgress :: UserImportJobStatusType
pattern InProgress = UserImportJobStatusType' "InProgress"

pattern Pending :: UserImportJobStatusType
pattern Pending = UserImportJobStatusType' "Pending"

pattern Stopped :: UserImportJobStatusType
pattern Stopped = UserImportJobStatusType' "Stopped"

pattern Stopping :: UserImportJobStatusType
pattern Stopping = UserImportJobStatusType' "Stopping"

pattern Succeeded :: UserImportJobStatusType
pattern Succeeded = UserImportJobStatusType' "Succeeded"

{-# COMPLETE
  Created,
  Expired,
  Failed,
  InProgress,
  Pending,
  Stopped,
  Stopping,
  Succeeded,
  UserImportJobStatusType' #-}

instance FromText UserImportJobStatusType where
    parser = (UserImportJobStatusType' . mk) <$> takeText

instance ToText UserImportJobStatusType where
    toText (UserImportJobStatusType' ci) = original ci

-- | Represents an enum of /known/ $UserImportJobStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserImportJobStatusType where
    toEnum i = case i of
        0 -> Created
        1 -> Expired
        2 -> Failed
        3 -> InProgress
        4 -> Pending
        5 -> Stopped
        6 -> Stopping
        7 -> Succeeded
        _ -> (error . showText) $ "Unknown index for UserImportJobStatusType: " <> toText i
    fromEnum x = case x of
        Created -> 0
        Expired -> 1
        Failed -> 2
        InProgress -> 3
        Pending -> 4
        Stopped -> 5
        Stopping -> 6
        Succeeded -> 7
        UserImportJobStatusType' name -> (error . showText) $ "Unknown UserImportJobStatusType: " <> original name

-- | Represents the bounds of /known/ $UserImportJobStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserImportJobStatusType where
    minBound = Created
    maxBound = Succeeded

instance Hashable     UserImportJobStatusType
instance NFData       UserImportJobStatusType
instance ToByteString UserImportJobStatusType
instance ToQuery      UserImportJobStatusType
instance ToHeader     UserImportJobStatusType

instance FromJSON UserImportJobStatusType where
    parseJSON = parseJSONText "UserImportJobStatusType"
