{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.Product (
    module Network.AWS.Kinesis.Types.Consumer,
    module Network.AWS.Kinesis.Types.ConsumerDescription,
    module Network.AWS.Kinesis.Types.EnhancedMetrics,
    module Network.AWS.Kinesis.Types.EnhancedMonitoringOutput,
    module Network.AWS.Kinesis.Types.HashKeyRange,
    module Network.AWS.Kinesis.Types.InternalFailureException,
    module Network.AWS.Kinesis.Types.KMSAccessDeniedException,
    module Network.AWS.Kinesis.Types.KMSDisabledException,
    module Network.AWS.Kinesis.Types.KMSInvalidStateException,
    module Network.AWS.Kinesis.Types.KMSNotFoundException,
    module Network.AWS.Kinesis.Types.KMSOptInRequired,
    module Network.AWS.Kinesis.Types.KMSThrottlingException,
    module Network.AWS.Kinesis.Types.PutRecordsRequestEntry,
    module Network.AWS.Kinesis.Types.PutRecordsResultEntry,
    module Network.AWS.Kinesis.Types.Record,
    module Network.AWS.Kinesis.Types.ResourceInUseException,
    module Network.AWS.Kinesis.Types.ResourceNotFoundException,
    module Network.AWS.Kinesis.Types.SequenceNumberRange,
    module Network.AWS.Kinesis.Types.Shard,
    module Network.AWS.Kinesis.Types.StartingPosition,
    module Network.AWS.Kinesis.Types.StreamDescription,
    module Network.AWS.Kinesis.Types.StreamDescriptionSummary,
    module Network.AWS.Kinesis.Types.SubscribeToShardEvent,
    module Network.AWS.Kinesis.Types.SubscribeToShardEventStream,
    module Network.AWS.Kinesis.Types.Tag
  ) where

import Network.AWS.Kinesis.Types.Consumer
import Network.AWS.Kinesis.Types.ConsumerDescription
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.InternalFailureException
import Network.AWS.Kinesis.Types.KMSAccessDeniedException
import Network.AWS.Kinesis.Types.KMSDisabledException
import Network.AWS.Kinesis.Types.KMSInvalidStateException
import Network.AWS.Kinesis.Types.KMSNotFoundException
import Network.AWS.Kinesis.Types.KMSOptInRequired
import Network.AWS.Kinesis.Types.KMSThrottlingException
import Network.AWS.Kinesis.Types.PutRecordsRequestEntry
import Network.AWS.Kinesis.Types.PutRecordsResultEntry
import Network.AWS.Kinesis.Types.Record
import Network.AWS.Kinesis.Types.ResourceInUseException
import Network.AWS.Kinesis.Types.ResourceNotFoundException
import Network.AWS.Kinesis.Types.SequenceNumberRange
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.StartingPosition
import Network.AWS.Kinesis.Types.StreamDescription
import Network.AWS.Kinesis.Types.StreamDescriptionSummary
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import Network.AWS.Kinesis.Types.SubscribeToShardEventStream
import Network.AWS.Kinesis.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude
