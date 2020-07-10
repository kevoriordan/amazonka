{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingPolicyUpdateBehavior (
  ScalingPolicyUpdateBehavior (
    ..
    , KeepExternalPolicies
    , ReplaceExternalPolicies
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ScalingPolicyUpdateBehavior = ScalingPolicyUpdateBehavior' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern KeepExternalPolicies :: ScalingPolicyUpdateBehavior
pattern KeepExternalPolicies = ScalingPolicyUpdateBehavior' "KeepExternalPolicies"

pattern ReplaceExternalPolicies :: ScalingPolicyUpdateBehavior
pattern ReplaceExternalPolicies = ScalingPolicyUpdateBehavior' "ReplaceExternalPolicies"

{-# COMPLETE
  KeepExternalPolicies,
  ReplaceExternalPolicies,
  ScalingPolicyUpdateBehavior' #-}

instance FromText ScalingPolicyUpdateBehavior where
    parser = (ScalingPolicyUpdateBehavior' . mk) <$> takeText

instance ToText ScalingPolicyUpdateBehavior where
    toText (ScalingPolicyUpdateBehavior' ci) = original ci

-- | Represents an enum of /known/ $ScalingPolicyUpdateBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ScalingPolicyUpdateBehavior where
    toEnum i = case i of
        0 -> KeepExternalPolicies
        1 -> ReplaceExternalPolicies
        _ -> (error . showText) $ "Unknown index for ScalingPolicyUpdateBehavior: " <> toText i
    fromEnum x = case x of
        KeepExternalPolicies -> 0
        ReplaceExternalPolicies -> 1
        ScalingPolicyUpdateBehavior' name -> (error . showText) $ "Unknown ScalingPolicyUpdateBehavior: " <> original name

-- | Represents the bounds of /known/ $ScalingPolicyUpdateBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ScalingPolicyUpdateBehavior where
    minBound = KeepExternalPolicies
    maxBound = ReplaceExternalPolicies

instance Hashable     ScalingPolicyUpdateBehavior
instance NFData       ScalingPolicyUpdateBehavior
instance ToByteString ScalingPolicyUpdateBehavior
instance ToQuery      ScalingPolicyUpdateBehavior
instance ToHeader     ScalingPolicyUpdateBehavior

instance ToJSON ScalingPolicyUpdateBehavior where
    toJSON = toJSONText

instance FromJSON ScalingPolicyUpdateBehavior where
    parseJSON = parseJSONText "ScalingPolicyUpdateBehavior"
