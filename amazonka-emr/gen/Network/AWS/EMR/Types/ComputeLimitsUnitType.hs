{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimitsUnitType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.ComputeLimitsUnitType (
  ComputeLimitsUnitType (
    ..
    , InstanceFleetUnits
    , Instances
    , Vcpu
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ComputeLimitsUnitType = ComputeLimitsUnitType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern InstanceFleetUnits :: ComputeLimitsUnitType
pattern InstanceFleetUnits = ComputeLimitsUnitType' "InstanceFleetUnits"

pattern Instances :: ComputeLimitsUnitType
pattern Instances = ComputeLimitsUnitType' "Instances"

pattern Vcpu :: ComputeLimitsUnitType
pattern Vcpu = ComputeLimitsUnitType' "VCPU"

{-# COMPLETE
  InstanceFleetUnits,
  Instances,
  Vcpu,
  ComputeLimitsUnitType' #-}

instance FromText ComputeLimitsUnitType where
    parser = (ComputeLimitsUnitType' . mk) <$> takeText

instance ToText ComputeLimitsUnitType where
    toText (ComputeLimitsUnitType' ci) = original ci

-- | Represents an enum of /known/ $ComputeLimitsUnitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ComputeLimitsUnitType where
    toEnum i = case i of
        0 -> InstanceFleetUnits
        1 -> Instances
        2 -> Vcpu
        _ -> (error . showText) $ "Unknown index for ComputeLimitsUnitType: " <> toText i
    fromEnum x = case x of
        InstanceFleetUnits -> 0
        Instances -> 1
        Vcpu -> 2
        ComputeLimitsUnitType' name -> (error . showText) $ "Unknown ComputeLimitsUnitType: " <> original name

-- | Represents the bounds of /known/ $ComputeLimitsUnitType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ComputeLimitsUnitType where
    minBound = InstanceFleetUnits
    maxBound = Vcpu

instance Hashable     ComputeLimitsUnitType
instance NFData       ComputeLimitsUnitType
instance ToByteString ComputeLimitsUnitType
instance ToQuery      ComputeLimitsUnitType
instance ToHeader     ComputeLimitsUnitType

instance ToJSON ComputeLimitsUnitType where
    toJSON = toJSONText

instance FromJSON ComputeLimitsUnitType where
    parseJSON = parseJSONText "ComputeLimitsUnitType"
