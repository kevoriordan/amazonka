{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.AgentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.AgentStatus (
  AgentStatus (
    ..
    , Blacklisted
    , Healthy
    , Running
    , Shutdown
    , Unhealthy
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AgentStatus = AgentStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Blacklisted :: AgentStatus
pattern Blacklisted = AgentStatus' "BLACKLISTED"

pattern Healthy :: AgentStatus
pattern Healthy = AgentStatus' "HEALTHY"

pattern Running :: AgentStatus
pattern Running = AgentStatus' "RUNNING"

pattern Shutdown :: AgentStatus
pattern Shutdown = AgentStatus' "SHUTDOWN"

pattern Unhealthy :: AgentStatus
pattern Unhealthy = AgentStatus' "UNHEALTHY"

pattern Unknown :: AgentStatus
pattern Unknown = AgentStatus' "UNKNOWN"

{-# COMPLETE
  Blacklisted,
  Healthy,
  Running,
  Shutdown,
  Unhealthy,
  Unknown,
  AgentStatus' #-}

instance FromText AgentStatus where
    parser = (AgentStatus' . mk) <$> takeText

instance ToText AgentStatus where
    toText (AgentStatus' ci) = original ci

-- | Represents an enum of /known/ $AgentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AgentStatus where
    toEnum i = case i of
        0 -> Blacklisted
        1 -> Healthy
        2 -> Running
        3 -> Shutdown
        4 -> Unhealthy
        5 -> Unknown
        _ -> (error . showText) $ "Unknown index for AgentStatus: " <> toText i
    fromEnum x = case x of
        Blacklisted -> 0
        Healthy -> 1
        Running -> 2
        Shutdown -> 3
        Unhealthy -> 4
        Unknown -> 5
        AgentStatus' name -> (error . showText) $ "Unknown AgentStatus: " <> original name

-- | Represents the bounds of /known/ $AgentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AgentStatus where
    minBound = Blacklisted
    maxBound = Unknown

instance Hashable     AgentStatus
instance NFData       AgentStatus
instance ToByteString AgentStatus
instance ToQuery      AgentStatus
instance ToHeader     AgentStatus

instance FromJSON AgentStatus where
    parseJSON = parseJSONText "AgentStatus"
