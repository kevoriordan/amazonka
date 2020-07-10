{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceFilterAttribute (
  DeviceFilterAttribute (
    ..
    , DFAARN
    , DFAAvailability
    , DFAFleetType
    , DFAFormFactor
    , DFAInstanceARN
    , DFAInstanceLabels
    , DFAManufacturer
    , DFAModel
    , DFAOSVersion
    , DFAPlatform
    , DFARemoteAccessEnabled
    , DFARemoteDebugEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceFilterAttribute = DeviceFilterAttribute' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern DFAARN :: DeviceFilterAttribute
pattern DFAARN = DeviceFilterAttribute' "ARN"

pattern DFAAvailability :: DeviceFilterAttribute
pattern DFAAvailability = DeviceFilterAttribute' "AVAILABILITY"

pattern DFAFleetType :: DeviceFilterAttribute
pattern DFAFleetType = DeviceFilterAttribute' "FLEET_TYPE"

pattern DFAFormFactor :: DeviceFilterAttribute
pattern DFAFormFactor = DeviceFilterAttribute' "FORM_FACTOR"

pattern DFAInstanceARN :: DeviceFilterAttribute
pattern DFAInstanceARN = DeviceFilterAttribute' "INSTANCE_ARN"

pattern DFAInstanceLabels :: DeviceFilterAttribute
pattern DFAInstanceLabels = DeviceFilterAttribute' "INSTANCE_LABELS"

pattern DFAManufacturer :: DeviceFilterAttribute
pattern DFAManufacturer = DeviceFilterAttribute' "MANUFACTURER"

pattern DFAModel :: DeviceFilterAttribute
pattern DFAModel = DeviceFilterAttribute' "MODEL"

pattern DFAOSVersion :: DeviceFilterAttribute
pattern DFAOSVersion = DeviceFilterAttribute' "OS_VERSION"

pattern DFAPlatform :: DeviceFilterAttribute
pattern DFAPlatform = DeviceFilterAttribute' "PLATFORM"

pattern DFARemoteAccessEnabled :: DeviceFilterAttribute
pattern DFARemoteAccessEnabled = DeviceFilterAttribute' "REMOTE_ACCESS_ENABLED"

pattern DFARemoteDebugEnabled :: DeviceFilterAttribute
pattern DFARemoteDebugEnabled = DeviceFilterAttribute' "REMOTE_DEBUG_ENABLED"

{-# COMPLETE
  DFAARN,
  DFAAvailability,
  DFAFleetType,
  DFAFormFactor,
  DFAInstanceARN,
  DFAInstanceLabels,
  DFAManufacturer,
  DFAModel,
  DFAOSVersion,
  DFAPlatform,
  DFARemoteAccessEnabled,
  DFARemoteDebugEnabled,
  DeviceFilterAttribute' #-}

instance FromText DeviceFilterAttribute where
    parser = (DeviceFilterAttribute' . mk) <$> takeText

instance ToText DeviceFilterAttribute where
    toText (DeviceFilterAttribute' ci) = original ci

-- | Represents an enum of /known/ $DeviceFilterAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceFilterAttribute where
    toEnum i = case i of
        0 -> DFAARN
        1 -> DFAAvailability
        2 -> DFAFleetType
        3 -> DFAFormFactor
        4 -> DFAInstanceARN
        5 -> DFAInstanceLabels
        6 -> DFAManufacturer
        7 -> DFAModel
        8 -> DFAOSVersion
        9 -> DFAPlatform
        10 -> DFARemoteAccessEnabled
        11 -> DFARemoteDebugEnabled
        _ -> (error . showText) $ "Unknown index for DeviceFilterAttribute: " <> toText i
    fromEnum x = case x of
        DFAARN -> 0
        DFAAvailability -> 1
        DFAFleetType -> 2
        DFAFormFactor -> 3
        DFAInstanceARN -> 4
        DFAInstanceLabels -> 5
        DFAManufacturer -> 6
        DFAModel -> 7
        DFAOSVersion -> 8
        DFAPlatform -> 9
        DFARemoteAccessEnabled -> 10
        DFARemoteDebugEnabled -> 11
        DeviceFilterAttribute' name -> (error . showText) $ "Unknown DeviceFilterAttribute: " <> original name

-- | Represents the bounds of /known/ $DeviceFilterAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceFilterAttribute where
    minBound = DFAARN
    maxBound = DFARemoteDebugEnabled

instance Hashable     DeviceFilterAttribute
instance NFData       DeviceFilterAttribute
instance ToByteString DeviceFilterAttribute
instance ToQuery      DeviceFilterAttribute
instance ToHeader     DeviceFilterAttribute

instance ToJSON DeviceFilterAttribute where
    toJSON = toJSONText

instance FromJSON DeviceFilterAttribute where
    parseJSON = parseJSONText "DeviceFilterAttribute"
