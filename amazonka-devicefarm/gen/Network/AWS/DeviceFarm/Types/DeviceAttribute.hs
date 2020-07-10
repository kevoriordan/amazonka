{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceAttribute (
  DeviceAttribute (
    ..
    , ARN
    , AppiumVersion
    , Availability
    , FleetType
    , FormFactor
    , InstanceARN
    , InstanceLabels
    , Manufacturer
    , Model
    , OSVersion
    , Platform
    , RemoteAccessEnabled
    , RemoteDebugEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceAttribute = DeviceAttribute' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ARN :: DeviceAttribute
pattern ARN = DeviceAttribute' "ARN"

pattern AppiumVersion :: DeviceAttribute
pattern AppiumVersion = DeviceAttribute' "APPIUM_VERSION"

pattern Availability :: DeviceAttribute
pattern Availability = DeviceAttribute' "AVAILABILITY"

pattern FleetType :: DeviceAttribute
pattern FleetType = DeviceAttribute' "FLEET_TYPE"

pattern FormFactor :: DeviceAttribute
pattern FormFactor = DeviceAttribute' "FORM_FACTOR"

pattern InstanceARN :: DeviceAttribute
pattern InstanceARN = DeviceAttribute' "INSTANCE_ARN"

pattern InstanceLabels :: DeviceAttribute
pattern InstanceLabels = DeviceAttribute' "INSTANCE_LABELS"

pattern Manufacturer :: DeviceAttribute
pattern Manufacturer = DeviceAttribute' "MANUFACTURER"

pattern Model :: DeviceAttribute
pattern Model = DeviceAttribute' "MODEL"

pattern OSVersion :: DeviceAttribute
pattern OSVersion = DeviceAttribute' "OS_VERSION"

pattern Platform :: DeviceAttribute
pattern Platform = DeviceAttribute' "PLATFORM"

pattern RemoteAccessEnabled :: DeviceAttribute
pattern RemoteAccessEnabled = DeviceAttribute' "REMOTE_ACCESS_ENABLED"

pattern RemoteDebugEnabled :: DeviceAttribute
pattern RemoteDebugEnabled = DeviceAttribute' "REMOTE_DEBUG_ENABLED"

{-# COMPLETE
  ARN,
  AppiumVersion,
  Availability,
  FleetType,
  FormFactor,
  InstanceARN,
  InstanceLabels,
  Manufacturer,
  Model,
  OSVersion,
  Platform,
  RemoteAccessEnabled,
  RemoteDebugEnabled,
  DeviceAttribute' #-}

instance FromText DeviceAttribute where
    parser = (DeviceAttribute' . mk) <$> takeText

instance ToText DeviceAttribute where
    toText (DeviceAttribute' ci) = original ci

-- | Represents an enum of /known/ $DeviceAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceAttribute where
    toEnum i = case i of
        0 -> ARN
        1 -> AppiumVersion
        2 -> Availability
        3 -> FleetType
        4 -> FormFactor
        5 -> InstanceARN
        6 -> InstanceLabels
        7 -> Manufacturer
        8 -> Model
        9 -> OSVersion
        10 -> Platform
        11 -> RemoteAccessEnabled
        12 -> RemoteDebugEnabled
        _ -> (error . showText) $ "Unknown index for DeviceAttribute: " <> toText i
    fromEnum x = case x of
        ARN -> 0
        AppiumVersion -> 1
        Availability -> 2
        FleetType -> 3
        FormFactor -> 4
        InstanceARN -> 5
        InstanceLabels -> 6
        Manufacturer -> 7
        Model -> 8
        OSVersion -> 9
        Platform -> 10
        RemoteAccessEnabled -> 11
        RemoteDebugEnabled -> 12
        DeviceAttribute' name -> (error . showText) $ "Unknown DeviceAttribute: " <> original name

-- | Represents the bounds of /known/ $DeviceAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceAttribute where
    minBound = ARN
    maxBound = RemoteDebugEnabled

instance Hashable     DeviceAttribute
instance NFData       DeviceAttribute
instance ToByteString DeviceAttribute
instance ToQuery      DeviceAttribute
instance ToHeader     DeviceAttribute

instance ToJSON DeviceAttribute where
    toJSON = toJSONText

instance FromJSON DeviceAttribute where
    parseJSON = parseJSONText "DeviceAttribute"
