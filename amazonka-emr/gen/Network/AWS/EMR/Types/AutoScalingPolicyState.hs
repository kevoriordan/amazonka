{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.AutoScalingPolicyState (
  AutoScalingPolicyState (
    ..
    , Attached
    , Attaching
    , Detached
    , Detaching
    , Failed
    , Pending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoScalingPolicyState = AutoScalingPolicyState' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Attached :: AutoScalingPolicyState
pattern Attached = AutoScalingPolicyState' "ATTACHED"

pattern Attaching :: AutoScalingPolicyState
pattern Attaching = AutoScalingPolicyState' "ATTACHING"

pattern Detached :: AutoScalingPolicyState
pattern Detached = AutoScalingPolicyState' "DETACHED"

pattern Detaching :: AutoScalingPolicyState
pattern Detaching = AutoScalingPolicyState' "DETACHING"

pattern Failed :: AutoScalingPolicyState
pattern Failed = AutoScalingPolicyState' "FAILED"

pattern Pending :: AutoScalingPolicyState
pattern Pending = AutoScalingPolicyState' "PENDING"

{-# COMPLETE
  Attached,
  Attaching,
  Detached,
  Detaching,
  Failed,
  Pending,
  AutoScalingPolicyState' #-}

instance FromText AutoScalingPolicyState where
    parser = (AutoScalingPolicyState' . mk) <$> takeText

instance ToText AutoScalingPolicyState where
    toText (AutoScalingPolicyState' ci) = original ci

-- | Represents an enum of /known/ $AutoScalingPolicyState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoScalingPolicyState where
    toEnum i = case i of
        0 -> Attached
        1 -> Attaching
        2 -> Detached
        3 -> Detaching
        4 -> Failed
        5 -> Pending
        _ -> (error . showText) $ "Unknown index for AutoScalingPolicyState: " <> toText i
    fromEnum x = case x of
        Attached -> 0
        Attaching -> 1
        Detached -> 2
        Detaching -> 3
        Failed -> 4
        Pending -> 5
        AutoScalingPolicyState' name -> (error . showText) $ "Unknown AutoScalingPolicyState: " <> original name

-- | Represents the bounds of /known/ $AutoScalingPolicyState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoScalingPolicyState where
    minBound = Attached
    maxBound = Pending

instance Hashable     AutoScalingPolicyState
instance NFData       AutoScalingPolicyState
instance ToByteString AutoScalingPolicyState
instance ToQuery      AutoScalingPolicyState
instance ToHeader     AutoScalingPolicyState

instance FromJSON AutoScalingPolicyState where
    parseJSON = parseJSONText "AutoScalingPolicyState"
