{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyEvaluationDecisionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.PolicyEvaluationDecisionType (
  PolicyEvaluationDecisionType (
    ..
    , Allowed
    , ExplicitDeny
    , ImplicitDeny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyEvaluationDecisionType = PolicyEvaluationDecisionType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Allowed :: PolicyEvaluationDecisionType
pattern Allowed = PolicyEvaluationDecisionType' "allowed"

pattern ExplicitDeny :: PolicyEvaluationDecisionType
pattern ExplicitDeny = PolicyEvaluationDecisionType' "explicitDeny"

pattern ImplicitDeny :: PolicyEvaluationDecisionType
pattern ImplicitDeny = PolicyEvaluationDecisionType' "implicitDeny"

{-# COMPLETE
  Allowed,
  ExplicitDeny,
  ImplicitDeny,
  PolicyEvaluationDecisionType' #-}

instance FromText PolicyEvaluationDecisionType where
    parser = (PolicyEvaluationDecisionType' . mk) <$> takeText

instance ToText PolicyEvaluationDecisionType where
    toText (PolicyEvaluationDecisionType' ci) = original ci

-- | Represents an enum of /known/ $PolicyEvaluationDecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyEvaluationDecisionType where
    toEnum i = case i of
        0 -> Allowed
        1 -> ExplicitDeny
        2 -> ImplicitDeny
        _ -> (error . showText) $ "Unknown index for PolicyEvaluationDecisionType: " <> toText i
    fromEnum x = case x of
        Allowed -> 0
        ExplicitDeny -> 1
        ImplicitDeny -> 2
        PolicyEvaluationDecisionType' name -> (error . showText) $ "Unknown PolicyEvaluationDecisionType: " <> original name

-- | Represents the bounds of /known/ $PolicyEvaluationDecisionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyEvaluationDecisionType where
    minBound = Allowed
    maxBound = ImplicitDeny

instance Hashable     PolicyEvaluationDecisionType
instance NFData       PolicyEvaluationDecisionType
instance ToByteString PolicyEvaluationDecisionType
instance ToQuery      PolicyEvaluationDecisionType
instance ToHeader     PolicyEvaluationDecisionType

instance FromXML PolicyEvaluationDecisionType where
    parseXML = parseXMLText "PolicyEvaluationDecisionType"
