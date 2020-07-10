{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AgentHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AgentHealth (
  AgentHealth (
    ..
    , AHHealthy
    , AHUnhealthy
    , AHUnknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AgentHealth = AgentHealth' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern AHHealthy :: AgentHealth
pattern AHHealthy = AgentHealth' "HEALTHY"

pattern AHUnhealthy :: AgentHealth
pattern AHUnhealthy = AgentHealth' "UNHEALTHY"

pattern AHUnknown :: AgentHealth
pattern AHUnknown = AgentHealth' "UNKNOWN"

{-# COMPLETE
  AHHealthy,
  AHUnhealthy,
  AHUnknown,
  AgentHealth' #-}

instance FromText AgentHealth where
    parser = (AgentHealth' . mk) <$> takeText

instance ToText AgentHealth where
    toText (AgentHealth' ci) = original ci

-- | Represents an enum of /known/ $AgentHealth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AgentHealth where
    toEnum i = case i of
        0 -> AHHealthy
        1 -> AHUnhealthy
        2 -> AHUnknown
        _ -> (error . showText) $ "Unknown index for AgentHealth: " <> toText i
    fromEnum x = case x of
        AHHealthy -> 0
        AHUnhealthy -> 1
        AHUnknown -> 2
        AgentHealth' name -> (error . showText) $ "Unknown AgentHealth: " <> original name

-- | Represents the bounds of /known/ $AgentHealth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AgentHealth where
    minBound = AHHealthy
    maxBound = AHUnknown

instance Hashable     AgentHealth
instance NFData       AgentHealth
instance ToByteString AgentHealth
instance ToQuery      AgentHealth
instance ToHeader     AgentHealth

instance ToJSON AgentHealth where
    toJSON = toJSONText

instance FromJSON AgentHealth where
    parseJSON = parseJSONText "AgentHealth"
