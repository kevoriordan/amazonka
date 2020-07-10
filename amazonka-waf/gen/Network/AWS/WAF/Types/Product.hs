{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.Product (
    module Network.AWS.WAF.Types.ActivatedRule,
    module Network.AWS.WAF.Types.ByteMatchSet,
    module Network.AWS.WAF.Types.ByteMatchSetSummary,
    module Network.AWS.WAF.Types.ByteMatchSetUpdate,
    module Network.AWS.WAF.Types.ByteMatchTuple,
    module Network.AWS.WAF.Types.ExcludedRule,
    module Network.AWS.WAF.Types.FieldToMatch,
    module Network.AWS.WAF.Types.GeoMatchConstraint,
    module Network.AWS.WAF.Types.GeoMatchSet,
    module Network.AWS.WAF.Types.GeoMatchSetSummary,
    module Network.AWS.WAF.Types.GeoMatchSetUpdate,
    module Network.AWS.WAF.Types.HTTPHeader,
    module Network.AWS.WAF.Types.HTTPRequest,
    module Network.AWS.WAF.Types.IPSet,
    module Network.AWS.WAF.Types.IPSetDescriptor,
    module Network.AWS.WAF.Types.IPSetSummary,
    module Network.AWS.WAF.Types.IPSetUpdate,
    module Network.AWS.WAF.Types.LoggingConfiguration,
    module Network.AWS.WAF.Types.Predicate,
    module Network.AWS.WAF.Types.RateBasedRule,
    module Network.AWS.WAF.Types.RegexMatchSet,
    module Network.AWS.WAF.Types.RegexMatchSetSummary,
    module Network.AWS.WAF.Types.RegexMatchSetUpdate,
    module Network.AWS.WAF.Types.RegexMatchTuple,
    module Network.AWS.WAF.Types.RegexPatternSet,
    module Network.AWS.WAF.Types.RegexPatternSetSummary,
    module Network.AWS.WAF.Types.RegexPatternSetUpdate,
    module Network.AWS.WAF.Types.Rule,
    module Network.AWS.WAF.Types.RuleGroup,
    module Network.AWS.WAF.Types.RuleGroupSummary,
    module Network.AWS.WAF.Types.RuleGroupUpdate,
    module Network.AWS.WAF.Types.RuleSummary,
    module Network.AWS.WAF.Types.RuleUpdate,
    module Network.AWS.WAF.Types.SampledHTTPRequest,
    module Network.AWS.WAF.Types.SizeConstraint,
    module Network.AWS.WAF.Types.SizeConstraintSet,
    module Network.AWS.WAF.Types.SizeConstraintSetSummary,
    module Network.AWS.WAF.Types.SizeConstraintSetUpdate,
    module Network.AWS.WAF.Types.SqlInjectionMatchSet,
    module Network.AWS.WAF.Types.SqlInjectionMatchSetSummary,
    module Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate,
    module Network.AWS.WAF.Types.SqlInjectionMatchTuple,
    module Network.AWS.WAF.Types.SubscribedRuleGroupSummary,
    module Network.AWS.WAF.Types.Tag,
    module Network.AWS.WAF.Types.TagInfoForResource,
    module Network.AWS.WAF.Types.TimeWindow,
    module Network.AWS.WAF.Types.WafAction,
    module Network.AWS.WAF.Types.WafOverrideAction,
    module Network.AWS.WAF.Types.WebACL,
    module Network.AWS.WAF.Types.WebACLSummary,
    module Network.AWS.WAF.Types.WebACLUpdate,
    module Network.AWS.WAF.Types.XSSMatchSet,
    module Network.AWS.WAF.Types.XSSMatchSetSummary,
    module Network.AWS.WAF.Types.XSSMatchSetUpdate,
    module Network.AWS.WAF.Types.XSSMatchTuple
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ByteMatchSet
import Network.AWS.WAF.Types.ByteMatchSetSummary
import Network.AWS.WAF.Types.ByteMatchSetUpdate
import Network.AWS.WAF.Types.ByteMatchTuple
import Network.AWS.WAF.Types.ExcludedRule
import Network.AWS.WAF.Types.FieldToMatch
import Network.AWS.WAF.Types.GeoMatchConstraint
import Network.AWS.WAF.Types.GeoMatchSet
import Network.AWS.WAF.Types.GeoMatchSetSummary
import Network.AWS.WAF.Types.GeoMatchSetUpdate
import Network.AWS.WAF.Types.HTTPHeader
import Network.AWS.WAF.Types.HTTPRequest
import Network.AWS.WAF.Types.IPSet
import Network.AWS.WAF.Types.IPSetDescriptor
import Network.AWS.WAF.Types.IPSetSummary
import Network.AWS.WAF.Types.IPSetUpdate
import Network.AWS.WAF.Types.LoggingConfiguration
import Network.AWS.WAF.Types.Predicate
import Network.AWS.WAF.Types.RateBasedRule
import Network.AWS.WAF.Types.RegexMatchSet
import Network.AWS.WAF.Types.RegexMatchSetSummary
import Network.AWS.WAF.Types.RegexMatchSetUpdate
import Network.AWS.WAF.Types.RegexMatchTuple
import Network.AWS.WAF.Types.RegexPatternSet
import Network.AWS.WAF.Types.RegexPatternSetSummary
import Network.AWS.WAF.Types.RegexPatternSetUpdate
import Network.AWS.WAF.Types.Rule
import Network.AWS.WAF.Types.RuleGroup
import Network.AWS.WAF.Types.RuleGroupSummary
import Network.AWS.WAF.Types.RuleGroupUpdate
import Network.AWS.WAF.Types.RuleSummary
import Network.AWS.WAF.Types.RuleUpdate
import Network.AWS.WAF.Types.SampledHTTPRequest
import Network.AWS.WAF.Types.SizeConstraint
import Network.AWS.WAF.Types.SizeConstraintSet
import Network.AWS.WAF.Types.SizeConstraintSetSummary
import Network.AWS.WAF.Types.SizeConstraintSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchSet
import Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
import Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
import Network.AWS.WAF.Types.SqlInjectionMatchTuple
import Network.AWS.WAF.Types.SubscribedRuleGroupSummary
import Network.AWS.WAF.Types.Tag
import Network.AWS.WAF.Types.TagInfoForResource
import Network.AWS.WAF.Types.TimeWindow
import Network.AWS.WAF.Types.WafAction
import Network.AWS.WAF.Types.WafOverrideAction
import Network.AWS.WAF.Types.WebACL
import Network.AWS.WAF.Types.WebACLSummary
import Network.AWS.WAF.Types.WebACLUpdate
import Network.AWS.WAF.Types.XSSMatchSet
import Network.AWS.WAF.Types.XSSMatchSetSummary
import Network.AWS.WAF.Types.XSSMatchSetUpdate
import Network.AWS.WAF.Types.XSSMatchTuple
