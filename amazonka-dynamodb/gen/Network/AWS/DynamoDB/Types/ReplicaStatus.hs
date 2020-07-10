{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ReplicaStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ReplicaStatus (
  ReplicaStatus (
    ..
    , RSActive
    , RSCreating
    , RSCreationFailed
    , RSDeleting
    , RSUpdating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReplicaStatus = ReplicaStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern RSActive :: ReplicaStatus
pattern RSActive = ReplicaStatus' "ACTIVE"

pattern RSCreating :: ReplicaStatus
pattern RSCreating = ReplicaStatus' "CREATING"

pattern RSCreationFailed :: ReplicaStatus
pattern RSCreationFailed = ReplicaStatus' "CREATION_FAILED"

pattern RSDeleting :: ReplicaStatus
pattern RSDeleting = ReplicaStatus' "DELETING"

pattern RSUpdating :: ReplicaStatus
pattern RSUpdating = ReplicaStatus' "UPDATING"

{-# COMPLETE
  RSActive,
  RSCreating,
  RSCreationFailed,
  RSDeleting,
  RSUpdating,
  ReplicaStatus' #-}

instance FromText ReplicaStatus where
    parser = (ReplicaStatus' . mk) <$> takeText

instance ToText ReplicaStatus where
    toText (ReplicaStatus' ci) = original ci

-- | Represents an enum of /known/ $ReplicaStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicaStatus where
    toEnum i = case i of
        0 -> RSActive
        1 -> RSCreating
        2 -> RSCreationFailed
        3 -> RSDeleting
        4 -> RSUpdating
        _ -> (error . showText) $ "Unknown index for ReplicaStatus: " <> toText i
    fromEnum x = case x of
        RSActive -> 0
        RSCreating -> 1
        RSCreationFailed -> 2
        RSDeleting -> 3
        RSUpdating -> 4
        ReplicaStatus' name -> (error . showText) $ "Unknown ReplicaStatus: " <> original name

-- | Represents the bounds of /known/ $ReplicaStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicaStatus where
    minBound = RSActive
    maxBound = RSUpdating

instance Hashable     ReplicaStatus
instance NFData       ReplicaStatus
instance ToByteString ReplicaStatus
instance ToQuery      ReplicaStatus
instance ToHeader     ReplicaStatus

instance FromJSON ReplicaStatus where
    parseJSON = parseJSONText "ReplicaStatus"
