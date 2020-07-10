{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneOptInStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AvailabilityZoneOptInStatus (
  AvailabilityZoneOptInStatus (
    ..
    , AZOISNotOptedIn
    , AZOISOptInNotRequired
    , AZOISOptedIn
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AvailabilityZoneOptInStatus = AvailabilityZoneOptInStatus' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern AZOISNotOptedIn :: AvailabilityZoneOptInStatus
pattern AZOISNotOptedIn = AvailabilityZoneOptInStatus' "not-opted-in"

pattern AZOISOptInNotRequired :: AvailabilityZoneOptInStatus
pattern AZOISOptInNotRequired = AvailabilityZoneOptInStatus' "opt-in-not-required"

pattern AZOISOptedIn :: AvailabilityZoneOptInStatus
pattern AZOISOptedIn = AvailabilityZoneOptInStatus' "opted-in"

{-# COMPLETE
  AZOISNotOptedIn,
  AZOISOptInNotRequired,
  AZOISOptedIn,
  AvailabilityZoneOptInStatus' #-}

instance FromText AvailabilityZoneOptInStatus where
    parser = (AvailabilityZoneOptInStatus' . mk) <$> takeText

instance ToText AvailabilityZoneOptInStatus where
    toText (AvailabilityZoneOptInStatus' ci) = original ci

-- | Represents an enum of /known/ $AvailabilityZoneOptInStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AvailabilityZoneOptInStatus where
    toEnum i = case i of
        0 -> AZOISNotOptedIn
        1 -> AZOISOptInNotRequired
        2 -> AZOISOptedIn
        _ -> (error . showText) $ "Unknown index for AvailabilityZoneOptInStatus: " <> toText i
    fromEnum x = case x of
        AZOISNotOptedIn -> 0
        AZOISOptInNotRequired -> 1
        AZOISOptedIn -> 2
        AvailabilityZoneOptInStatus' name -> (error . showText) $ "Unknown AvailabilityZoneOptInStatus: " <> original name

-- | Represents the bounds of /known/ $AvailabilityZoneOptInStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AvailabilityZoneOptInStatus where
    minBound = AZOISNotOptedIn
    maxBound = AZOISOptedIn

instance Hashable     AvailabilityZoneOptInStatus
instance NFData       AvailabilityZoneOptInStatus
instance ToByteString AvailabilityZoneOptInStatus
instance ToQuery      AvailabilityZoneOptInStatus
instance ToHeader     AvailabilityZoneOptInStatus

instance FromXML AvailabilityZoneOptInStatus where
    parseXML = parseXMLText "AvailabilityZoneOptInStatus"
