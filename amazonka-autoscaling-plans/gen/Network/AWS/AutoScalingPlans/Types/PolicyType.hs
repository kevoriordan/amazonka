{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.PolicyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.PolicyType (
  PolicyType (
    ..
    , TargetTrackingScaling
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PolicyType = PolicyType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern TargetTrackingScaling :: PolicyType
pattern TargetTrackingScaling = PolicyType' "TargetTrackingScaling"

{-# COMPLETE
  TargetTrackingScaling,
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
        0 -> TargetTrackingScaling
        _ -> (error . showText) $ "Unknown index for PolicyType: " <> toText i
    fromEnum x = case x of
        TargetTrackingScaling -> 0
        PolicyType' name -> (error . showText) $ "Unknown PolicyType: " <> original name

-- | Represents the bounds of /known/ $PolicyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PolicyType where
    minBound = TargetTrackingScaling
    maxBound = TargetTrackingScaling

instance Hashable     PolicyType
instance NFData       PolicyType
instance ToByteString PolicyType
instance ToQuery      PolicyType
instance ToHeader     PolicyType

instance FromJSON PolicyType where
    parseJSON = parseJSONText "PolicyType"
