{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EventCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EventCode (
  EventCode (
    ..
    , InstanceReboot
    , InstanceRetirement
    , InstanceStop
    , SystemMaintenance
    , SystemReboot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EventCode = EventCode' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern InstanceReboot :: EventCode
pattern InstanceReboot = EventCode' "instance-reboot"

pattern InstanceRetirement :: EventCode
pattern InstanceRetirement = EventCode' "instance-retirement"

pattern InstanceStop :: EventCode
pattern InstanceStop = EventCode' "instance-stop"

pattern SystemMaintenance :: EventCode
pattern SystemMaintenance = EventCode' "system-maintenance"

pattern SystemReboot :: EventCode
pattern SystemReboot = EventCode' "system-reboot"

{-# COMPLETE
  InstanceReboot,
  InstanceRetirement,
  InstanceStop,
  SystemMaintenance,
  SystemReboot,
  EventCode' #-}

instance FromText EventCode where
    parser = (EventCode' . mk) <$> takeText

instance ToText EventCode where
    toText (EventCode' ci) = original ci

-- | Represents an enum of /known/ $EventCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EventCode where
    toEnum i = case i of
        0 -> InstanceReboot
        1 -> InstanceRetirement
        2 -> InstanceStop
        3 -> SystemMaintenance
        4 -> SystemReboot
        _ -> (error . showText) $ "Unknown index for EventCode: " <> toText i
    fromEnum x = case x of
        InstanceReboot -> 0
        InstanceRetirement -> 1
        InstanceStop -> 2
        SystemMaintenance -> 3
        SystemReboot -> 4
        EventCode' name -> (error . showText) $ "Unknown EventCode: " <> original name

-- | Represents the bounds of /known/ $EventCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EventCode where
    minBound = InstanceReboot
    maxBound = SystemReboot

instance Hashable     EventCode
instance NFData       EventCode
instance ToByteString EventCode
instance ToQuery      EventCode
instance ToHeader     EventCode

instance FromXML EventCode where
    parseXML = parseXMLText "EventCode"
