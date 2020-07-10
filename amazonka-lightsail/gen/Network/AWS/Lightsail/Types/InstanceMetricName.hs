{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceMetricName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceMetricName (
  InstanceMetricName (
    ..
    , IMNCPUUtilization
    , IMNNetworkIn
    , IMNNetworkOut
    , IMNStatusCheckFailed
    , IMNStatusCheckFailedInstance
    , IMNStatusCheckFailedSystem
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceMetricName = InstanceMetricName' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern IMNCPUUtilization :: InstanceMetricName
pattern IMNCPUUtilization = InstanceMetricName' "CPUUtilization"

pattern IMNNetworkIn :: InstanceMetricName
pattern IMNNetworkIn = InstanceMetricName' "NetworkIn"

pattern IMNNetworkOut :: InstanceMetricName
pattern IMNNetworkOut = InstanceMetricName' "NetworkOut"

pattern IMNStatusCheckFailed :: InstanceMetricName
pattern IMNStatusCheckFailed = InstanceMetricName' "StatusCheckFailed"

pattern IMNStatusCheckFailedInstance :: InstanceMetricName
pattern IMNStatusCheckFailedInstance = InstanceMetricName' "StatusCheckFailed_Instance"

pattern IMNStatusCheckFailedSystem :: InstanceMetricName
pattern IMNStatusCheckFailedSystem = InstanceMetricName' "StatusCheckFailed_System"

{-# COMPLETE
  IMNCPUUtilization,
  IMNNetworkIn,
  IMNNetworkOut,
  IMNStatusCheckFailed,
  IMNStatusCheckFailedInstance,
  IMNStatusCheckFailedSystem,
  InstanceMetricName' #-}

instance FromText InstanceMetricName where
    parser = (InstanceMetricName' . mk) <$> takeText

instance ToText InstanceMetricName where
    toText (InstanceMetricName' ci) = original ci

-- | Represents an enum of /known/ $InstanceMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceMetricName where
    toEnum i = case i of
        0 -> IMNCPUUtilization
        1 -> IMNNetworkIn
        2 -> IMNNetworkOut
        3 -> IMNStatusCheckFailed
        4 -> IMNStatusCheckFailedInstance
        5 -> IMNStatusCheckFailedSystem
        _ -> (error . showText) $ "Unknown index for InstanceMetricName: " <> toText i
    fromEnum x = case x of
        IMNCPUUtilization -> 0
        IMNNetworkIn -> 1
        IMNNetworkOut -> 2
        IMNStatusCheckFailed -> 3
        IMNStatusCheckFailedInstance -> 4
        IMNStatusCheckFailedSystem -> 5
        InstanceMetricName' name -> (error . showText) $ "Unknown InstanceMetricName: " <> original name

-- | Represents the bounds of /known/ $InstanceMetricName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceMetricName where
    minBound = IMNCPUUtilization
    maxBound = IMNStatusCheckFailedSystem

instance Hashable     InstanceMetricName
instance NFData       InstanceMetricName
instance ToByteString InstanceMetricName
instance ToQuery      InstanceMetricName
instance ToHeader     InstanceMetricName

instance ToJSON InstanceMetricName where
    toJSON = toJSONText

instance FromJSON InstanceMetricName where
    parseJSON = parseJSONText "InstanceMetricName"
