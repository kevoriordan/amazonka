{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.Product (
    module Network.AWS.Route53AutoNaming.Types.DNSConfig,
    module Network.AWS.Route53AutoNaming.Types.DNSConfigChange,
    module Network.AWS.Route53AutoNaming.Types.DNSProperties,
    module Network.AWS.Route53AutoNaming.Types.DNSRecord,
    module Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary,
    module Network.AWS.Route53AutoNaming.Types.HTTPProperties,
    module Network.AWS.Route53AutoNaming.Types.HealthCheckConfig,
    module Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig,
    module Network.AWS.Route53AutoNaming.Types.Instance,
    module Network.AWS.Route53AutoNaming.Types.InstanceSummary,
    module Network.AWS.Route53AutoNaming.Types.Namespace,
    module Network.AWS.Route53AutoNaming.Types.NamespaceFilter,
    module Network.AWS.Route53AutoNaming.Types.NamespaceProperties,
    module Network.AWS.Route53AutoNaming.Types.NamespaceSummary,
    module Network.AWS.Route53AutoNaming.Types.Operation,
    module Network.AWS.Route53AutoNaming.Types.OperationFilter,
    module Network.AWS.Route53AutoNaming.Types.OperationSummary,
    module Network.AWS.Route53AutoNaming.Types.ServiceChange,
    module Network.AWS.Route53AutoNaming.Types.ServiceFilter,
    module Network.AWS.Route53AutoNaming.Types.ServiceInfo,
    module Network.AWS.Route53AutoNaming.Types.ServiceSummary
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53AutoNaming.Types.DNSConfig
import Network.AWS.Route53AutoNaming.Types.DNSConfigChange
import Network.AWS.Route53AutoNaming.Types.DNSProperties
import Network.AWS.Route53AutoNaming.Types.DNSRecord
import Network.AWS.Route53AutoNaming.Types.HTTPInstanceSummary
import Network.AWS.Route53AutoNaming.Types.HTTPProperties
import Network.AWS.Route53AutoNaming.Types.HealthCheckConfig
import Network.AWS.Route53AutoNaming.Types.HealthCheckCustomConfig
import Network.AWS.Route53AutoNaming.Types.Instance
import Network.AWS.Route53AutoNaming.Types.InstanceSummary
import Network.AWS.Route53AutoNaming.Types.Namespace
import Network.AWS.Route53AutoNaming.Types.NamespaceFilter
import Network.AWS.Route53AutoNaming.Types.NamespaceProperties
import Network.AWS.Route53AutoNaming.Types.NamespaceSummary
import Network.AWS.Route53AutoNaming.Types.Operation
import Network.AWS.Route53AutoNaming.Types.OperationFilter
import Network.AWS.Route53AutoNaming.Types.OperationSummary
import Network.AWS.Route53AutoNaming.Types.ServiceChange
import Network.AWS.Route53AutoNaming.Types.ServiceFilter
import Network.AWS.Route53AutoNaming.Types.ServiceInfo
import Network.AWS.Route53AutoNaming.Types.ServiceSummary
