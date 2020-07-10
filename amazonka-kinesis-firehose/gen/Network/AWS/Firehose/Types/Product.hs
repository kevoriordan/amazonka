{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Product (
    module Network.AWS.Firehose.Types.BufferingHints,
    module Network.AWS.Firehose.Types.CloudWatchLoggingOptions,
    module Network.AWS.Firehose.Types.CopyCommand,
    module Network.AWS.Firehose.Types.DataFormatConversionConfiguration,
    module Network.AWS.Firehose.Types.DeliveryStreamDescription,
    module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration,
    module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput,
    module Network.AWS.Firehose.Types.Deserializer,
    module Network.AWS.Firehose.Types.DestinationDescription,
    module Network.AWS.Firehose.Types.ElasticsearchBufferingHints,
    module Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration,
    module Network.AWS.Firehose.Types.ElasticsearchDestinationDescription,
    module Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate,
    module Network.AWS.Firehose.Types.ElasticsearchRetryOptions,
    module Network.AWS.Firehose.Types.EncryptionConfiguration,
    module Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration,
    module Network.AWS.Firehose.Types.ExtendedS3DestinationDescription,
    module Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate,
    module Network.AWS.Firehose.Types.FailureDescription,
    module Network.AWS.Firehose.Types.HiveJSONSerDe,
    module Network.AWS.Firehose.Types.InputFormatConfiguration,
    module Network.AWS.Firehose.Types.KMSEncryptionConfig,
    module Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration,
    module Network.AWS.Firehose.Types.KinesisStreamSourceDescription,
    module Network.AWS.Firehose.Types.OpenXJSONSerDe,
    module Network.AWS.Firehose.Types.OrcSerDe,
    module Network.AWS.Firehose.Types.OutputFormatConfiguration,
    module Network.AWS.Firehose.Types.ParquetSerDe,
    module Network.AWS.Firehose.Types.ProcessingConfiguration,
    module Network.AWS.Firehose.Types.Processor,
    module Network.AWS.Firehose.Types.ProcessorParameter,
    module Network.AWS.Firehose.Types.PutRecordBatchResponseEntry,
    module Network.AWS.Firehose.Types.Record,
    module Network.AWS.Firehose.Types.RedshiftDestinationConfiguration,
    module Network.AWS.Firehose.Types.RedshiftDestinationDescription,
    module Network.AWS.Firehose.Types.RedshiftDestinationUpdate,
    module Network.AWS.Firehose.Types.RedshiftRetryOptions,
    module Network.AWS.Firehose.Types.S3DestinationConfiguration,
    module Network.AWS.Firehose.Types.S3DestinationDescription,
    module Network.AWS.Firehose.Types.S3DestinationUpdate,
    module Network.AWS.Firehose.Types.SchemaConfiguration,
    module Network.AWS.Firehose.Types.Serializer,
    module Network.AWS.Firehose.Types.SourceDescription,
    module Network.AWS.Firehose.Types.SplunkDestinationConfiguration,
    module Network.AWS.Firehose.Types.SplunkDestinationDescription,
    module Network.AWS.Firehose.Types.SplunkDestinationUpdate,
    module Network.AWS.Firehose.Types.SplunkRetryOptions,
    module Network.AWS.Firehose.Types.Tag,
    module Network.AWS.Firehose.Types.VPCConfiguration,
    module Network.AWS.Firehose.Types.VPCConfigurationDescription
  ) where

import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamDescription
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Network.AWS.Firehose.Types.Deserializer
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.HiveJSONSerDe
import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Firehose.Types.OpenXJSONSerDe
import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.OutputFormatConfiguration
import Network.AWS.Firehose.Types.ParquetSerDe
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.Processor
import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
import Network.AWS.Firehose.Types.Record
import Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationUpdate
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Firehose.Types.SchemaConfiguration
import Network.AWS.Firehose.Types.Serializer
import Network.AWS.Firehose.Types.SourceDescription
import Network.AWS.Firehose.Types.SplunkDestinationConfiguration
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationUpdate
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.Tag
import Network.AWS.Firehose.Types.VPCConfiguration
import Network.AWS.Firehose.Types.VPCConfigurationDescription
import Network.AWS.Lens
import Network.AWS.Prelude
