{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupState (
  InstanceGroupState (
    ..
    , Arrested
    , Bootstrapping
    , Ended
    , Provisioning
    , Reconfiguring
    , Resizing
    , Running
    , ShuttingDown
    , Suspended
    , Terminated
    , Terminating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceGroupState = InstanceGroupState' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Arrested :: InstanceGroupState
pattern Arrested = InstanceGroupState' "ARRESTED"

pattern Bootstrapping :: InstanceGroupState
pattern Bootstrapping = InstanceGroupState' "BOOTSTRAPPING"

pattern Ended :: InstanceGroupState
pattern Ended = InstanceGroupState' "ENDED"

pattern Provisioning :: InstanceGroupState
pattern Provisioning = InstanceGroupState' "PROVISIONING"

pattern Reconfiguring :: InstanceGroupState
pattern Reconfiguring = InstanceGroupState' "RECONFIGURING"

pattern Resizing :: InstanceGroupState
pattern Resizing = InstanceGroupState' "RESIZING"

pattern Running :: InstanceGroupState
pattern Running = InstanceGroupState' "RUNNING"

pattern ShuttingDown :: InstanceGroupState
pattern ShuttingDown = InstanceGroupState' "SHUTTING_DOWN"

pattern Suspended :: InstanceGroupState
pattern Suspended = InstanceGroupState' "SUSPENDED"

pattern Terminated :: InstanceGroupState
pattern Terminated = InstanceGroupState' "TERMINATED"

pattern Terminating :: InstanceGroupState
pattern Terminating = InstanceGroupState' "TERMINATING"

{-# COMPLETE
  Arrested,
  Bootstrapping,
  Ended,
  Provisioning,
  Reconfiguring,
  Resizing,
  Running,
  ShuttingDown,
  Suspended,
  Terminated,
  Terminating,
  InstanceGroupState' #-}

instance FromText InstanceGroupState where
    parser = (InstanceGroupState' . mk) <$> takeText

instance ToText InstanceGroupState where
    toText (InstanceGroupState' ci) = original ci

-- | Represents an enum of /known/ $InstanceGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceGroupState where
    toEnum i = case i of
        0 -> Arrested
        1 -> Bootstrapping
        2 -> Ended
        3 -> Provisioning
        4 -> Reconfiguring
        5 -> Resizing
        6 -> Running
        7 -> ShuttingDown
        8 -> Suspended
        9 -> Terminated
        10 -> Terminating
        _ -> (error . showText) $ "Unknown index for InstanceGroupState: " <> toText i
    fromEnum x = case x of
        Arrested -> 0
        Bootstrapping -> 1
        Ended -> 2
        Provisioning -> 3
        Reconfiguring -> 4
        Resizing -> 5
        Running -> 6
        ShuttingDown -> 7
        Suspended -> 8
        Terminated -> 9
        Terminating -> 10
        InstanceGroupState' name -> (error . showText) $ "Unknown InstanceGroupState: " <> original name

-- | Represents the bounds of /known/ $InstanceGroupState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceGroupState where
    minBound = Arrested
    maxBound = Terminating

instance Hashable     InstanceGroupState
instance NFData       InstanceGroupState
instance ToByteString InstanceGroupState
instance ToQuery      InstanceGroupState
instance ToHeader     InstanceGroupState

instance FromJSON InstanceGroupState where
    parseJSON = parseJSONText "InstanceGroupState"
