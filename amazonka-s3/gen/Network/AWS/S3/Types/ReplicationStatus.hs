{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationStatus (
  ReplicationStatus (
    ..
    , Completed
    , Failed
    , Pending
    , Replica
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ReplicationStatus = ReplicationStatus' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Completed :: ReplicationStatus
pattern Completed = ReplicationStatus' "COMPLETED"

pattern Failed :: ReplicationStatus
pattern Failed = ReplicationStatus' "FAILED"

pattern Pending :: ReplicationStatus
pattern Pending = ReplicationStatus' "PENDING"

pattern Replica :: ReplicationStatus
pattern Replica = ReplicationStatus' "REPLICA"

{-# COMPLETE
  Completed,
  Failed,
  Pending,
  Replica,
  ReplicationStatus' #-}

instance FromText ReplicationStatus where
    parser = (ReplicationStatus' . mk) <$> takeText

instance ToText ReplicationStatus where
    toText (ReplicationStatus' ci) = original ci

-- | Represents an enum of /known/ $ReplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReplicationStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> Failed
        2 -> Pending
        3 -> Replica
        _ -> (error . showText) $ "Unknown index for ReplicationStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Failed -> 1
        Pending -> 2
        Replica -> 3
        ReplicationStatus' name -> (error . showText) $ "Unknown ReplicationStatus: " <> original name

-- | Represents the bounds of /known/ $ReplicationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReplicationStatus where
    minBound = Completed
    maxBound = Replica

instance Hashable     ReplicationStatus
instance NFData       ReplicationStatus
instance ToByteString ReplicationStatus
instance ToQuery      ReplicationStatus
instance ToHeader     ReplicationStatus

instance FromXML ReplicationStatus where
    parseXML = parseXMLText "ReplicationStatus"
