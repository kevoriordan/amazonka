{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailabilityZoneState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.AvailabilityZoneState (
  AvailabilityZoneState (
    ..
    , AZSAvailable
    , AZSImpaired
    , AZSInformation
    , AZSUnavailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AvailabilityZoneState = AvailabilityZoneState' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern AZSAvailable :: AvailabilityZoneState
pattern AZSAvailable = AvailabilityZoneState' "available"

pattern AZSImpaired :: AvailabilityZoneState
pattern AZSImpaired = AvailabilityZoneState' "impaired"

pattern AZSInformation :: AvailabilityZoneState
pattern AZSInformation = AvailabilityZoneState' "information"

pattern AZSUnavailable :: AvailabilityZoneState
pattern AZSUnavailable = AvailabilityZoneState' "unavailable"

{-# COMPLETE
  AZSAvailable,
  AZSImpaired,
  AZSInformation,
  AZSUnavailable,
  AvailabilityZoneState' #-}

instance FromText AvailabilityZoneState where
    parser = (AvailabilityZoneState' . mk) <$> takeText

instance ToText AvailabilityZoneState where
    toText (AvailabilityZoneState' ci) = original ci

-- | Represents an enum of /known/ $AvailabilityZoneState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AvailabilityZoneState where
    toEnum i = case i of
        0 -> AZSAvailable
        1 -> AZSImpaired
        2 -> AZSInformation
        3 -> AZSUnavailable
        _ -> (error . showText) $ "Unknown index for AvailabilityZoneState: " <> toText i
    fromEnum x = case x of
        AZSAvailable -> 0
        AZSImpaired -> 1
        AZSInformation -> 2
        AZSUnavailable -> 3
        AvailabilityZoneState' name -> (error . showText) $ "Unknown AvailabilityZoneState: " <> original name

-- | Represents the bounds of /known/ $AvailabilityZoneState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AvailabilityZoneState where
    minBound = AZSAvailable
    maxBound = AZSUnavailable

instance Hashable     AvailabilityZoneState
instance NFData       AvailabilityZoneState
instance ToByteString AvailabilityZoneState
instance ToQuery      AvailabilityZoneState
instance ToHeader     AvailabilityZoneState

instance FromXML AvailabilityZoneState where
    parseXML = parseXMLText "AvailabilityZoneState"
