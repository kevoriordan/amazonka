{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MachineLearning.Types.Product (
    module Network.AWS.MachineLearning.Types.BatchPrediction,
    module Network.AWS.MachineLearning.Types.DataSource,
    module Network.AWS.MachineLearning.Types.Evaluation,
    module Network.AWS.MachineLearning.Types.MLModel,
    module Network.AWS.MachineLearning.Types.PerformanceMetrics,
    module Network.AWS.MachineLearning.Types.Prediction,
    module Network.AWS.MachineLearning.Types.RDSDataSpec,
    module Network.AWS.MachineLearning.Types.RDSDatabase,
    module Network.AWS.MachineLearning.Types.RDSDatabaseCredentials,
    module Network.AWS.MachineLearning.Types.RDSMetadata,
    module Network.AWS.MachineLearning.Types.RealtimeEndpointInfo,
    module Network.AWS.MachineLearning.Types.RedshiftDataSpec,
    module Network.AWS.MachineLearning.Types.RedshiftDatabase,
    module Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials,
    module Network.AWS.MachineLearning.Types.RedshiftMetadata,
    module Network.AWS.MachineLearning.Types.S3DataSpec,
    module Network.AWS.MachineLearning.Types.Tag
  ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types.BatchPrediction
import Network.AWS.MachineLearning.Types.DataSource
import Network.AWS.MachineLearning.Types.Evaluation
import Network.AWS.MachineLearning.Types.MLModel
import Network.AWS.MachineLearning.Types.PerformanceMetrics
import Network.AWS.MachineLearning.Types.Prediction
import Network.AWS.MachineLearning.Types.RDSDataSpec
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
import Network.AWS.MachineLearning.Types.RedshiftDataSpec
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import Network.AWS.MachineLearning.Types.S3DataSpec
import Network.AWS.MachineLearning.Types.Tag
import Network.AWS.Prelude
