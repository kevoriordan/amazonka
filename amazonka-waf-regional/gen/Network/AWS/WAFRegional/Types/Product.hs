{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.Product (
    module Network.AWS.WAFRegional.Types.ActivatedRule,
    module Network.AWS.WAFRegional.Types.ByteMatchSet,
    module Network.AWS.WAFRegional.Types.ByteMatchSetSummary,
    module Network.AWS.WAFRegional.Types.ByteMatchSetUpdate,
    module Network.AWS.WAFRegional.Types.ByteMatchTuple,
    module Network.AWS.WAFRegional.Types.ExcludedRule,
    module Network.AWS.WAFRegional.Types.FieldToMatch,
    module Network.AWS.WAFRegional.Types.GeoMatchConstraint,
    module Network.AWS.WAFRegional.Types.GeoMatchSet,
    module Network.AWS.WAFRegional.Types.GeoMatchSetSummary,
    module Network.AWS.WAFRegional.Types.GeoMatchSetUpdate,
    module Network.AWS.WAFRegional.Types.HTTPHeader,
    module Network.AWS.WAFRegional.Types.HTTPRequest,
    module Network.AWS.WAFRegional.Types.IPSet,
    module Network.AWS.WAFRegional.Types.IPSetDescriptor,
    module Network.AWS.WAFRegional.Types.IPSetSummary,
    module Network.AWS.WAFRegional.Types.IPSetUpdate,
    module Network.AWS.WAFRegional.Types.LoggingConfiguration,
    module Network.AWS.WAFRegional.Types.Predicate,
    module Network.AWS.WAFRegional.Types.RateBasedRule,
    module Network.AWS.WAFRegional.Types.RegexMatchSet,
    module Network.AWS.WAFRegional.Types.RegexMatchSetSummary,
    module Network.AWS.WAFRegional.Types.RegexMatchSetUpdate,
    module Network.AWS.WAFRegional.Types.RegexMatchTuple,
    module Network.AWS.WAFRegional.Types.RegexPatternSet,
    module Network.AWS.WAFRegional.Types.RegexPatternSetSummary,
    module Network.AWS.WAFRegional.Types.RegexPatternSetUpdate,
    module Network.AWS.WAFRegional.Types.Rule,
    module Network.AWS.WAFRegional.Types.RuleGroup,
    module Network.AWS.WAFRegional.Types.RuleGroupSummary,
    module Network.AWS.WAFRegional.Types.RuleGroupUpdate,
    module Network.AWS.WAFRegional.Types.RuleSummary,
    module Network.AWS.WAFRegional.Types.RuleUpdate,
    module Network.AWS.WAFRegional.Types.SampledHTTPRequest,
    module Network.AWS.WAFRegional.Types.SizeConstraint,
    module Network.AWS.WAFRegional.Types.SizeConstraintSet,
    module Network.AWS.WAFRegional.Types.SizeConstraintSetSummary,
    module Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate,
    module Network.AWS.WAFRegional.Types.SqlInjectionMatchSet,
    module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary,
    module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate,
    module Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple,
    module Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary,
    module Network.AWS.WAFRegional.Types.Tag,
    module Network.AWS.WAFRegional.Types.TagInfoForResource,
    module Network.AWS.WAFRegional.Types.TimeWindow,
    module Network.AWS.WAFRegional.Types.WafAction,
    module Network.AWS.WAFRegional.Types.WafOverrideAction,
    module Network.AWS.WAFRegional.Types.WebACL,
    module Network.AWS.WAFRegional.Types.WebACLSummary,
    module Network.AWS.WAFRegional.Types.WebACLUpdate,
    module Network.AWS.WAFRegional.Types.XSSMatchSet,
    module Network.AWS.WAFRegional.Types.XSSMatchSetSummary,
    module Network.AWS.WAFRegional.Types.XSSMatchSetUpdate,
    module Network.AWS.WAFRegional.Types.XSSMatchTuple
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.ByteMatchSet
import Network.AWS.WAFRegional.Types.ByteMatchSetSummary
import Network.AWS.WAFRegional.Types.ByteMatchSetUpdate
import Network.AWS.WAFRegional.Types.ByteMatchTuple
import Network.AWS.WAFRegional.Types.ExcludedRule
import Network.AWS.WAFRegional.Types.FieldToMatch
import Network.AWS.WAFRegional.Types.GeoMatchConstraint
import Network.AWS.WAFRegional.Types.GeoMatchSet
import Network.AWS.WAFRegional.Types.GeoMatchSetSummary
import Network.AWS.WAFRegional.Types.GeoMatchSetUpdate
import Network.AWS.WAFRegional.Types.HTTPHeader
import Network.AWS.WAFRegional.Types.HTTPRequest
import Network.AWS.WAFRegional.Types.IPSet
import Network.AWS.WAFRegional.Types.IPSetDescriptor
import Network.AWS.WAFRegional.Types.IPSetSummary
import Network.AWS.WAFRegional.Types.IPSetUpdate
import Network.AWS.WAFRegional.Types.LoggingConfiguration
import Network.AWS.WAFRegional.Types.Predicate
import Network.AWS.WAFRegional.Types.RateBasedRule
import Network.AWS.WAFRegional.Types.RegexMatchSet
import Network.AWS.WAFRegional.Types.RegexMatchSetSummary
import Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
import Network.AWS.WAFRegional.Types.RegexMatchTuple
import Network.AWS.WAFRegional.Types.RegexPatternSet
import Network.AWS.WAFRegional.Types.RegexPatternSetSummary
import Network.AWS.WAFRegional.Types.RegexPatternSetUpdate
import Network.AWS.WAFRegional.Types.Rule
import Network.AWS.WAFRegional.Types.RuleGroup
import Network.AWS.WAFRegional.Types.RuleGroupSummary
import Network.AWS.WAFRegional.Types.RuleGroupUpdate
import Network.AWS.WAFRegional.Types.RuleSummary
import Network.AWS.WAFRegional.Types.RuleUpdate
import Network.AWS.WAFRegional.Types.SampledHTTPRequest
import Network.AWS.WAFRegional.Types.SizeConstraint
import Network.AWS.WAFRegional.Types.SizeConstraintSet
import Network.AWS.WAFRegional.Types.SizeConstraintSetSummary
import Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple
import Network.AWS.WAFRegional.Types.SubscribedRuleGroupSummary
import Network.AWS.WAFRegional.Types.Tag
import Network.AWS.WAFRegional.Types.TagInfoForResource
import Network.AWS.WAFRegional.Types.TimeWindow
import Network.AWS.WAFRegional.Types.WafAction
import Network.AWS.WAFRegional.Types.WafOverrideAction
import Network.AWS.WAFRegional.Types.WebACL
import Network.AWS.WAFRegional.Types.WebACLSummary
import Network.AWS.WAFRegional.Types.WebACLUpdate
import Network.AWS.WAFRegional.Types.XSSMatchSet
import Network.AWS.WAFRegional.Types.XSSMatchSetSummary
import Network.AWS.WAFRegional.Types.XSSMatchSetUpdate
import Network.AWS.WAFRegional.Types.XSSMatchTuple
