{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.Product (
    module Network.AWS.KinesisAnalytics.Types.ApplicationDetail,
    module Network.AWS.KinesisAnalytics.Types.ApplicationSummary,
    module Network.AWS.KinesisAnalytics.Types.ApplicationUpdate,
    module Network.AWS.KinesisAnalytics.Types.CSVMappingParameters,
    module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption,
    module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription,
    module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate,
    module Network.AWS.KinesisAnalytics.Types.DestinationSchema,
    module Network.AWS.KinesisAnalytics.Types.Input,
    module Network.AWS.KinesisAnalytics.Types.InputConfiguration,
    module Network.AWS.KinesisAnalytics.Types.InputDescription,
    module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor,
    module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription,
    module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate,
    module Network.AWS.KinesisAnalytics.Types.InputParallelism,
    module Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate,
    module Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration,
    module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription,
    module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate,
    module Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate,
    module Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration,
    module Network.AWS.KinesisAnalytics.Types.InputUpdate,
    module Network.AWS.KinesisAnalytics.Types.JSONMappingParameters,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription,
    module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription,
    module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate,
    module Network.AWS.KinesisAnalytics.Types.LambdaOutput,
    module Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription,
    module Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate,
    module Network.AWS.KinesisAnalytics.Types.MappingParameters,
    module Network.AWS.KinesisAnalytics.Types.Output,
    module Network.AWS.KinesisAnalytics.Types.OutputDescription,
    module Network.AWS.KinesisAnalytics.Types.OutputUpdate,
    module Network.AWS.KinesisAnalytics.Types.RecordColumn,
    module Network.AWS.KinesisAnalytics.Types.RecordFormat,
    module Network.AWS.KinesisAnalytics.Types.ReferenceDataSource,
    module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription,
    module Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate,
    module Network.AWS.KinesisAnalytics.Types.S3Configuration,
    module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource,
    module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription,
    module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate,
    module Network.AWS.KinesisAnalytics.Types.SourceSchema,
    module Network.AWS.KinesisAnalytics.Types.Tag
  ) where

import Network.AWS.KinesisAnalytics.Types.ApplicationDetail
import Network.AWS.KinesisAnalytics.Types.ApplicationSummary
import Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.Input
import Network.AWS.KinesisAnalytics.Types.InputConfiguration
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.Output
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.S3Configuration
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.KinesisAnalytics.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude
