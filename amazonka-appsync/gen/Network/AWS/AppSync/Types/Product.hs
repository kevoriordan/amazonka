{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.Product (
    module Network.AWS.AppSync.Types.APICache,
    module Network.AWS.AppSync.Types.APIKey,
    module Network.AWS.AppSync.Types.AWSIAMConfig,
    module Network.AWS.AppSync.Types.AdditionalAuthenticationProvider,
    module Network.AWS.AppSync.Types.AuthorizationConfig,
    module Network.AWS.AppSync.Types.CachingConfig,
    module Network.AWS.AppSync.Types.CognitoUserPoolConfig,
    module Network.AWS.AppSync.Types.DataSource,
    module Network.AWS.AppSync.Types.DeltaSyncConfig,
    module Network.AWS.AppSync.Types.DynamodbDataSourceConfig,
    module Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig,
    module Network.AWS.AppSync.Types.FunctionConfiguration,
    module Network.AWS.AppSync.Types.GraphqlAPI,
    module Network.AWS.AppSync.Types.HTTPDataSourceConfig,
    module Network.AWS.AppSync.Types.LambdaConflictHandlerConfig,
    module Network.AWS.AppSync.Types.LambdaDataSourceConfig,
    module Network.AWS.AppSync.Types.LogConfig,
    module Network.AWS.AppSync.Types.OpenIdConnectConfig,
    module Network.AWS.AppSync.Types.PipelineConfig,
    module Network.AWS.AppSync.Types.RDSHTTPEndpointConfig,
    module Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig,
    module Network.AWS.AppSync.Types.Resolver,
    module Network.AWS.AppSync.Types.SyncConfig,
    module Network.AWS.AppSync.Types.Type,
    module Network.AWS.AppSync.Types.UserPoolConfig
  ) where

import Network.AWS.AppSync.Types.APICache
import Network.AWS.AppSync.Types.APIKey
import Network.AWS.AppSync.Types.AWSIAMConfig
import Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
import Network.AWS.AppSync.Types.AuthorizationConfig
import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.DataSource
import Network.AWS.AppSync.Types.DeltaSyncConfig
import Network.AWS.AppSync.Types.DynamodbDataSourceConfig
import Network.AWS.AppSync.Types.ElasticsearchDataSourceConfig
import Network.AWS.AppSync.Types.FunctionConfiguration
import Network.AWS.AppSync.Types.GraphqlAPI
import Network.AWS.AppSync.Types.HTTPDataSourceConfig
import Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
import Network.AWS.AppSync.Types.LambdaDataSourceConfig
import Network.AWS.AppSync.Types.LogConfig
import Network.AWS.AppSync.Types.OpenIdConnectConfig
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
import Network.AWS.AppSync.Types.RelationalDatabaseDataSourceConfig
import Network.AWS.AppSync.Types.Resolver
import Network.AWS.AppSync.Types.SyncConfig
import Network.AWS.AppSync.Types.Type
import Network.AWS.AppSync.Types.UserPoolConfig
import Network.AWS.Lens
import Network.AWS.Prelude
