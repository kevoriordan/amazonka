{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Event
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Event (
  Event (
    ..
    , S3ObjectCreated
    , S3ObjectCreatedCompleteMultipartUpload
    , S3ObjectCreatedCopy
    , S3ObjectCreatedPost
    , S3ObjectCreatedPut
    , S3ObjectRemoved
    , S3ObjectRemovedDelete
    , S3ObjectRemovedDeleteMarkerCreated
    , S3ObjectRestore
    , S3ObjectRestoreCompleted
    , S3ObjectRestorePost
    , S3ReducedRedundancyLostObject
    , S3Replication
    , S3ReplicationOperationFailedReplication
    , S3ReplicationOperationMissedThreshold
    , S3ReplicationOperationNotTracked
    , S3ReplicationOperationReplicatedAfterThreshold
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The bucket event for which to send notifications.
--
--
data Event = Event' (CI Text)
               deriving (Eq, Ord, Read, Show, Data, Typeable,
                         Generic)

pattern S3ObjectCreated :: Event
pattern S3ObjectCreated = Event' "s3:ObjectCreated:*"

pattern S3ObjectCreatedCompleteMultipartUpload :: Event
pattern S3ObjectCreatedCompleteMultipartUpload = Event' "s3:ObjectCreated:CompleteMultipartUpload"

pattern S3ObjectCreatedCopy :: Event
pattern S3ObjectCreatedCopy = Event' "s3:ObjectCreated:Copy"

pattern S3ObjectCreatedPost :: Event
pattern S3ObjectCreatedPost = Event' "s3:ObjectCreated:Post"

pattern S3ObjectCreatedPut :: Event
pattern S3ObjectCreatedPut = Event' "s3:ObjectCreated:Put"

pattern S3ObjectRemoved :: Event
pattern S3ObjectRemoved = Event' "s3:ObjectRemoved:*"

pattern S3ObjectRemovedDelete :: Event
pattern S3ObjectRemovedDelete = Event' "s3:ObjectRemoved:Delete"

pattern S3ObjectRemovedDeleteMarkerCreated :: Event
pattern S3ObjectRemovedDeleteMarkerCreated = Event' "s3:ObjectRemoved:DeleteMarkerCreated"

pattern S3ObjectRestore :: Event
pattern S3ObjectRestore = Event' "s3:ObjectRestore:*"

pattern S3ObjectRestoreCompleted :: Event
pattern S3ObjectRestoreCompleted = Event' "s3:ObjectRestore:Completed"

pattern S3ObjectRestorePost :: Event
pattern S3ObjectRestorePost = Event' "s3:ObjectRestore:Post"

pattern S3ReducedRedundancyLostObject :: Event
pattern S3ReducedRedundancyLostObject = Event' "s3:ReducedRedundancyLostObject"

pattern S3Replication :: Event
pattern S3Replication = Event' "s3:Replication:*"

pattern S3ReplicationOperationFailedReplication :: Event
pattern S3ReplicationOperationFailedReplication = Event' "s3:Replication:OperationFailedReplication"

pattern S3ReplicationOperationMissedThreshold :: Event
pattern S3ReplicationOperationMissedThreshold = Event' "s3:Replication:OperationMissedThreshold"

pattern S3ReplicationOperationNotTracked :: Event
pattern S3ReplicationOperationNotTracked = Event' "s3:Replication:OperationNotTracked"

pattern S3ReplicationOperationReplicatedAfterThreshold :: Event
pattern S3ReplicationOperationReplicatedAfterThreshold = Event' "s3:Replication:OperationReplicatedAfterThreshold"

{-# COMPLETE
  S3ObjectCreated,
  S3ObjectCreatedCompleteMultipartUpload,
  S3ObjectCreatedCopy,
  S3ObjectCreatedPost,
  S3ObjectCreatedPut,
  S3ObjectRemoved,
  S3ObjectRemovedDelete,
  S3ObjectRemovedDeleteMarkerCreated,
  S3ObjectRestore,
  S3ObjectRestoreCompleted,
  S3ObjectRestorePost,
  S3ReducedRedundancyLostObject,
  S3Replication,
  S3ReplicationOperationFailedReplication,
  S3ReplicationOperationMissedThreshold,
  S3ReplicationOperationNotTracked,
  S3ReplicationOperationReplicatedAfterThreshold,
  Event' #-}

instance FromText Event where
    parser = (Event' . mk) <$> takeText

instance ToText Event where
    toText (Event' ci) = original ci

-- | Represents an enum of /known/ $Event.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Event where
    toEnum i = case i of
        0 -> S3ObjectCreated
        1 -> S3ObjectCreatedCompleteMultipartUpload
        2 -> S3ObjectCreatedCopy
        3 -> S3ObjectCreatedPost
        4 -> S3ObjectCreatedPut
        5 -> S3ObjectRemoved
        6 -> S3ObjectRemovedDelete
        7 -> S3ObjectRemovedDeleteMarkerCreated
        8 -> S3ObjectRestore
        9 -> S3ObjectRestoreCompleted
        10 -> S3ObjectRestorePost
        11 -> S3ReducedRedundancyLostObject
        12 -> S3Replication
        13 -> S3ReplicationOperationFailedReplication
        14 -> S3ReplicationOperationMissedThreshold
        15 -> S3ReplicationOperationNotTracked
        16 -> S3ReplicationOperationReplicatedAfterThreshold
        _ -> (error . showText) $ "Unknown index for Event: " <> toText i
    fromEnum x = case x of
        S3ObjectCreated -> 0
        S3ObjectCreatedCompleteMultipartUpload -> 1
        S3ObjectCreatedCopy -> 2
        S3ObjectCreatedPost -> 3
        S3ObjectCreatedPut -> 4
        S3ObjectRemoved -> 5
        S3ObjectRemovedDelete -> 6
        S3ObjectRemovedDeleteMarkerCreated -> 7
        S3ObjectRestore -> 8
        S3ObjectRestoreCompleted -> 9
        S3ObjectRestorePost -> 10
        S3ReducedRedundancyLostObject -> 11
        S3Replication -> 12
        S3ReplicationOperationFailedReplication -> 13
        S3ReplicationOperationMissedThreshold -> 14
        S3ReplicationOperationNotTracked -> 15
        S3ReplicationOperationReplicatedAfterThreshold -> 16
        Event' name -> (error . showText) $ "Unknown Event: " <> original name

-- | Represents the bounds of /known/ $Event.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Event where
    minBound = S3ObjectCreated
    maxBound = S3ReplicationOperationReplicatedAfterThreshold

instance Hashable     Event
instance NFData       Event
instance ToByteString Event
instance ToQuery      Event
instance ToHeader     Event

instance FromXML Event where
    parseXML = parseXMLText "Event"

instance ToXML Event where
    toXML = toXMLText
