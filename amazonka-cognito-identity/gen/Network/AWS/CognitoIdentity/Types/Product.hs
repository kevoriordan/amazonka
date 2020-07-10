{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Product (
    module Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider,
    module Network.AWS.CognitoIdentity.Types.Credentials,
    module Network.AWS.CognitoIdentity.Types.IdentityDescription,
    module Network.AWS.CognitoIdentity.Types.IdentityPool,
    module Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription,
    module Network.AWS.CognitoIdentity.Types.MappingRule,
    module Network.AWS.CognitoIdentity.Types.RoleMapping,
    module Network.AWS.CognitoIdentity.Types.RulesConfigurationType,
    module Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
  ) where

import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import Network.AWS.CognitoIdentity.Types.Credentials
import Network.AWS.CognitoIdentity.Types.IdentityDescription
import Network.AWS.CognitoIdentity.Types.IdentityPool
import Network.AWS.CognitoIdentity.Types.IdentityPoolShortDescription
import Network.AWS.CognitoIdentity.Types.MappingRule
import Network.AWS.CognitoIdentity.Types.RoleMapping
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import Network.AWS.CognitoIdentity.Types.UnprocessedIdentityId
import Network.AWS.Lens
import Network.AWS.Prelude
