{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ActionOnFailure (
  ActionOnFailure (
    ..
    , CancelAndWait
    , Continue
    , TerminateCluster
    , TerminateJobFlow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ActionOnFailure = ActionOnFailure' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern CancelAndWait :: ActionOnFailure
pattern CancelAndWait = ActionOnFailure' "CANCEL_AND_WAIT"

pattern Continue :: ActionOnFailure
pattern Continue = ActionOnFailure' "CONTINUE"

pattern TerminateCluster :: ActionOnFailure
pattern TerminateCluster = ActionOnFailure' "TERMINATE_CLUSTER"

pattern TerminateJobFlow :: ActionOnFailure
pattern TerminateJobFlow = ActionOnFailure' "TERMINATE_JOB_FLOW"

{-# COMPLETE
  CancelAndWait,
  Continue,
  TerminateCluster,
  TerminateJobFlow,
  ActionOnFailure' #-}

instance FromText ActionOnFailure where
    parser = (ActionOnFailure' . mk) <$> takeText

instance ToText ActionOnFailure where
    toText (ActionOnFailure' ci) = original ci

-- | Represents an enum of /known/ $ActionOnFailure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionOnFailure where
    toEnum i = case i of
        0 -> CancelAndWait
        1 -> Continue
        2 -> TerminateCluster
        3 -> TerminateJobFlow
        _ -> (error . showText) $ "Unknown index for ActionOnFailure: " <> toText i
    fromEnum x = case x of
        CancelAndWait -> 0
        Continue -> 1
        TerminateCluster -> 2
        TerminateJobFlow -> 3
        ActionOnFailure' name -> (error . showText) $ "Unknown ActionOnFailure: " <> original name

-- | Represents the bounds of /known/ $ActionOnFailure.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionOnFailure where
    minBound = CancelAndWait
    maxBound = TerminateJobFlow

instance Hashable     ActionOnFailure
instance NFData       ActionOnFailure
instance ToByteString ActionOnFailure
instance ToQuery      ActionOnFailure
instance ToHeader     ActionOnFailure

instance ToJSON ActionOnFailure where
    toJSON = toJSONText

instance FromJSON ActionOnFailure where
    parseJSON = parseJSONText "ActionOnFailure"
