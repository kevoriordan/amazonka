{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ClusterState (
  ClusterState (
    ..
    , CSBootstrapping
    , CSRunning
    , CSStarting
    , CSTerminated
    , CSTerminatedWithErrors
    , CSTerminating
    , CSWaiting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClusterState = ClusterState' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern CSBootstrapping :: ClusterState
pattern CSBootstrapping = ClusterState' "BOOTSTRAPPING"

pattern CSRunning :: ClusterState
pattern CSRunning = ClusterState' "RUNNING"

pattern CSStarting :: ClusterState
pattern CSStarting = ClusterState' "STARTING"

pattern CSTerminated :: ClusterState
pattern CSTerminated = ClusterState' "TERMINATED"

pattern CSTerminatedWithErrors :: ClusterState
pattern CSTerminatedWithErrors = ClusterState' "TERMINATED_WITH_ERRORS"

pattern CSTerminating :: ClusterState
pattern CSTerminating = ClusterState' "TERMINATING"

pattern CSWaiting :: ClusterState
pattern CSWaiting = ClusterState' "WAITING"

{-# COMPLETE
  CSBootstrapping,
  CSRunning,
  CSStarting,
  CSTerminated,
  CSTerminatedWithErrors,
  CSTerminating,
  CSWaiting,
  ClusterState' #-}

instance FromText ClusterState where
    parser = (ClusterState' . mk) <$> takeText

instance ToText ClusterState where
    toText (ClusterState' ci) = original ci

-- | Represents an enum of /known/ $ClusterState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClusterState where
    toEnum i = case i of
        0 -> CSBootstrapping
        1 -> CSRunning
        2 -> CSStarting
        3 -> CSTerminated
        4 -> CSTerminatedWithErrors
        5 -> CSTerminating
        6 -> CSWaiting
        _ -> (error . showText) $ "Unknown index for ClusterState: " <> toText i
    fromEnum x = case x of
        CSBootstrapping -> 0
        CSRunning -> 1
        CSStarting -> 2
        CSTerminated -> 3
        CSTerminatedWithErrors -> 4
        CSTerminating -> 5
        CSWaiting -> 6
        ClusterState' name -> (error . showText) $ "Unknown ClusterState: " <> original name

-- | Represents the bounds of /known/ $ClusterState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClusterState where
    minBound = CSBootstrapping
    maxBound = CSWaiting

instance Hashable     ClusterState
instance NFData       ClusterState
instance ToByteString ClusterState
instance ToQuery      ClusterState
instance ToHeader     ClusterState

instance ToJSON ClusterState where
    toJSON = toJSONText

instance FromJSON ClusterState where
    parseJSON = parseJSONText "ClusterState"
