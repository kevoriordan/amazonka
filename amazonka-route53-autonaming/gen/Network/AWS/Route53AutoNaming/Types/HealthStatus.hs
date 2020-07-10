{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.HealthStatus (
  HealthStatus (
    ..
    , Healthy
    , Unhealthy
    , Unknown
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HealthStatus = HealthStatus' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Healthy :: HealthStatus
pattern Healthy = HealthStatus' "HEALTHY"

pattern Unhealthy :: HealthStatus
pattern Unhealthy = HealthStatus' "UNHEALTHY"

pattern Unknown :: HealthStatus
pattern Unknown = HealthStatus' "UNKNOWN"

{-# COMPLETE
  Healthy,
  Unhealthy,
  Unknown,
  HealthStatus' #-}

instance FromText HealthStatus where
    parser = (HealthStatus' . mk) <$> takeText

instance ToText HealthStatus where
    toText (HealthStatus' ci) = original ci

-- | Represents an enum of /known/ $HealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HealthStatus where
    toEnum i = case i of
        0 -> Healthy
        1 -> Unhealthy
        2 -> Unknown
        _ -> (error . showText) $ "Unknown index for HealthStatus: " <> toText i
    fromEnum x = case x of
        Healthy -> 0
        Unhealthy -> 1
        Unknown -> 2
        HealthStatus' name -> (error . showText) $ "Unknown HealthStatus: " <> original name

-- | Represents the bounds of /known/ $HealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HealthStatus where
    minBound = Healthy
    maxBound = Unknown

instance Hashable     HealthStatus
instance NFData       HealthStatus
instance ToByteString HealthStatus
instance ToQuery      HealthStatus
instance ToHeader     HealthStatus

instance FromJSON HealthStatus where
    parseJSON = parseJSONText "HealthStatus"
