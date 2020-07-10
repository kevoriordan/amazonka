{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PolicyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.PolicyType (
  PolicyType (
    ..
    , RuleBased
    , TargetBased
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyType = PolicyType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern RuleBased :: PolicyType
pattern RuleBased = PolicyType' "RuleBased"

pattern TargetBased :: PolicyType
pattern TargetBased = PolicyType' "TargetBased"

{-# COMPLETE
  RuleBased,
  TargetBased,
  PolicyType' #-}

instance FromText PolicyType where
    parser = (PolicyType' . mk) <$> takeText

instance ToText PolicyType where
    toText (PolicyType' ci) = original ci

-- | Represents an enum of /known/ $PolicyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PolicyType where
    toEnum i = case i of
        0 -> RuleBased
        1 -> TargetBased
        _ -> (error . showText) $ "Unknown index for PolicyType: " <> toText i
    fromEnum x = case x of
        RuleBased -> 0
        TargetBased -> 1
        PolicyType' name -> (error . showText) $ "Unknown PolicyType: " <> original name

-- | Represents the bounds of /known/ $PolicyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyType where
    minBound = RuleBased
    maxBound = TargetBased

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance ToJSON PolicyType where
    toJSON = toJSONText

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"
