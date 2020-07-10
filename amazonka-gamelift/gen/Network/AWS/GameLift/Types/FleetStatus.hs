{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.FleetStatus (
  FleetStatus (
    ..
    , FSActivating
    , FSActive
    , FSBuilding
    , FSDeleting
    , FSDownloading
    , FSError'
    , FSNew
    , FSTerminated
    , FSValidating
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FleetStatus = FleetStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern FSActivating :: FleetStatus
pattern FSActivating = FleetStatus' "ACTIVATING"

pattern FSActive :: FleetStatus
pattern FSActive = FleetStatus' "ACTIVE"

pattern FSBuilding :: FleetStatus
pattern FSBuilding = FleetStatus' "BUILDING"

pattern FSDeleting :: FleetStatus
pattern FSDeleting = FleetStatus' "DELETING"

pattern FSDownloading :: FleetStatus
pattern FSDownloading = FleetStatus' "DOWNLOADING"

pattern FSError' :: FleetStatus
pattern FSError' = FleetStatus' "ERROR"

pattern FSNew :: FleetStatus
pattern FSNew = FleetStatus' "NEW"

pattern FSTerminated :: FleetStatus
pattern FSTerminated = FleetStatus' "TERMINATED"

pattern FSValidating :: FleetStatus
pattern FSValidating = FleetStatus' "VALIDATING"

{-# COMPLETE
  FSActivating,
  FSActive,
  FSBuilding,
  FSDeleting,
  FSDownloading,
  FSError',
  FSNew,
  FSTerminated,
  FSValidating,
  FleetStatus' #-}

instance FromText FleetStatus where
    parser = (FleetStatus' . mk) <$> takeText

instance ToText FleetStatus where
    toText (FleetStatus' ci) = original ci

-- | Represents an enum of /known/ $FleetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetStatus where
    toEnum i = case i of
        0 -> FSActivating
        1 -> FSActive
        2 -> FSBuilding
        3 -> FSDeleting
        4 -> FSDownloading
        5 -> FSError'
        6 -> FSNew
        7 -> FSTerminated
        8 -> FSValidating
        _ -> (error . showText) $ "Unknown index for FleetStatus: " <> toText i
    fromEnum x = case x of
        FSActivating -> 0
        FSActive -> 1
        FSBuilding -> 2
        FSDeleting -> 3
        FSDownloading -> 4
        FSError' -> 5
        FSNew -> 6
        FSTerminated -> 7
        FSValidating -> 8
        FleetStatus' name -> (error . showText) $ "Unknown FleetStatus: " <> original name

-- | Represents the bounds of /known/ $FleetStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetStatus where
    minBound = FSActivating
    maxBound = FSValidating

instance Hashable     FleetStatus
instance NFData       FleetStatus
instance ToByteString FleetStatus
instance ToQuery      FleetStatus
instance ToHeader     FleetStatus

instance FromJSON FleetStatus where
    parseJSON = parseJSONText "FleetStatus"
