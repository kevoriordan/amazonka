{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.QueueAttributeName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.QueueAttributeName (
  QueueAttributeName (
    ..
    , QANAll
    , QANApproximateNumberOfMessages
    , QANApproximateNumberOfMessagesDelayed
    , QANApproximateNumberOfMessagesNotVisible
    , QANContentBasedDeduplication
    , QANCreatedTimestamp
    , QANDelaySeconds
    , QANFifoQueue
    , QANKMSDataKeyReusePeriodSeconds
    , QANKMSMasterKeyId
    , QANLastModifiedTimestamp
    , QANMaximumMessageSize
    , QANMessageRetentionPeriod
    , QANPolicy
    , QANQueueARN
    , QANReceiveMessageWaitTimeSeconds
    , QANRedrivePolicy
    , QANVisibilityTimeout
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QueueAttributeName = QueueAttributeName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern QANAll :: QueueAttributeName
pattern QANAll = QueueAttributeName' "All"

pattern QANApproximateNumberOfMessages :: QueueAttributeName
pattern QANApproximateNumberOfMessages = QueueAttributeName' "ApproximateNumberOfMessages"

pattern QANApproximateNumberOfMessagesDelayed :: QueueAttributeName
pattern QANApproximateNumberOfMessagesDelayed = QueueAttributeName' "ApproximateNumberOfMessagesDelayed"

pattern QANApproximateNumberOfMessagesNotVisible :: QueueAttributeName
pattern QANApproximateNumberOfMessagesNotVisible = QueueAttributeName' "ApproximateNumberOfMessagesNotVisible"

pattern QANContentBasedDeduplication :: QueueAttributeName
pattern QANContentBasedDeduplication = QueueAttributeName' "ContentBasedDeduplication"

pattern QANCreatedTimestamp :: QueueAttributeName
pattern QANCreatedTimestamp = QueueAttributeName' "CreatedTimestamp"

pattern QANDelaySeconds :: QueueAttributeName
pattern QANDelaySeconds = QueueAttributeName' "DelaySeconds"

pattern QANFifoQueue :: QueueAttributeName
pattern QANFifoQueue = QueueAttributeName' "FifoQueue"

pattern QANKMSDataKeyReusePeriodSeconds :: QueueAttributeName
pattern QANKMSDataKeyReusePeriodSeconds = QueueAttributeName' "KmsDataKeyReusePeriodSeconds"

pattern QANKMSMasterKeyId :: QueueAttributeName
pattern QANKMSMasterKeyId = QueueAttributeName' "KmsMasterKeyId"

pattern QANLastModifiedTimestamp :: QueueAttributeName
pattern QANLastModifiedTimestamp = QueueAttributeName' "LastModifiedTimestamp"

pattern QANMaximumMessageSize :: QueueAttributeName
pattern QANMaximumMessageSize = QueueAttributeName' "MaximumMessageSize"

pattern QANMessageRetentionPeriod :: QueueAttributeName
pattern QANMessageRetentionPeriod = QueueAttributeName' "MessageRetentionPeriod"

pattern QANPolicy :: QueueAttributeName
pattern QANPolicy = QueueAttributeName' "Policy"

pattern QANQueueARN :: QueueAttributeName
pattern QANQueueARN = QueueAttributeName' "QueueArn"

pattern QANReceiveMessageWaitTimeSeconds :: QueueAttributeName
pattern QANReceiveMessageWaitTimeSeconds = QueueAttributeName' "ReceiveMessageWaitTimeSeconds"

pattern QANRedrivePolicy :: QueueAttributeName
pattern QANRedrivePolicy = QueueAttributeName' "RedrivePolicy"

pattern QANVisibilityTimeout :: QueueAttributeName
pattern QANVisibilityTimeout = QueueAttributeName' "VisibilityTimeout"

{-# COMPLETE
  QANAll,
  QANApproximateNumberOfMessages,
  QANApproximateNumberOfMessagesDelayed,
  QANApproximateNumberOfMessagesNotVisible,
  QANContentBasedDeduplication,
  QANCreatedTimestamp,
  QANDelaySeconds,
  QANFifoQueue,
  QANKMSDataKeyReusePeriodSeconds,
  QANKMSMasterKeyId,
  QANLastModifiedTimestamp,
  QANMaximumMessageSize,
  QANMessageRetentionPeriod,
  QANPolicy,
  QANQueueARN,
  QANReceiveMessageWaitTimeSeconds,
  QANRedrivePolicy,
  QANVisibilityTimeout,
  QueueAttributeName' #-}

instance FromText QueueAttributeName where
    parser = (QueueAttributeName' . mk) <$> takeText

instance ToText QueueAttributeName where
    toText (QueueAttributeName' ci) = original ci

-- | Represents an enum of /known/ $QueueAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QueueAttributeName where
    toEnum i = case i of
        0 -> QANAll
        1 -> QANApproximateNumberOfMessages
        2 -> QANApproximateNumberOfMessagesDelayed
        3 -> QANApproximateNumberOfMessagesNotVisible
        4 -> QANContentBasedDeduplication
        5 -> QANCreatedTimestamp
        6 -> QANDelaySeconds
        7 -> QANFifoQueue
        8 -> QANKMSDataKeyReusePeriodSeconds
        9 -> QANKMSMasterKeyId
        10 -> QANLastModifiedTimestamp
        11 -> QANMaximumMessageSize
        12 -> QANMessageRetentionPeriod
        13 -> QANPolicy
        14 -> QANQueueARN
        15 -> QANReceiveMessageWaitTimeSeconds
        16 -> QANRedrivePolicy
        17 -> QANVisibilityTimeout
        _ -> (error . showText) $ "Unknown index for QueueAttributeName: " <> toText i
    fromEnum x = case x of
        QANAll -> 0
        QANApproximateNumberOfMessages -> 1
        QANApproximateNumberOfMessagesDelayed -> 2
        QANApproximateNumberOfMessagesNotVisible -> 3
        QANContentBasedDeduplication -> 4
        QANCreatedTimestamp -> 5
        QANDelaySeconds -> 6
        QANFifoQueue -> 7
        QANKMSDataKeyReusePeriodSeconds -> 8
        QANKMSMasterKeyId -> 9
        QANLastModifiedTimestamp -> 10
        QANMaximumMessageSize -> 11
        QANMessageRetentionPeriod -> 12
        QANPolicy -> 13
        QANQueueARN -> 14
        QANReceiveMessageWaitTimeSeconds -> 15
        QANRedrivePolicy -> 16
        QANVisibilityTimeout -> 17
        QueueAttributeName' name -> (error . showText) $ "Unknown QueueAttributeName: " <> original name

-- | Represents the bounds of /known/ $QueueAttributeName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QueueAttributeName where
    minBound = QANAll
    maxBound = QANVisibilityTimeout

instance Hashable     QueueAttributeName
instance NFData       QueueAttributeName
instance ToByteString QueueAttributeName
instance ToQuery      QueueAttributeName
instance ToHeader     QueueAttributeName

instance FromXML QueueAttributeName where
    parseXML = parseXMLText "QueueAttributeName"
