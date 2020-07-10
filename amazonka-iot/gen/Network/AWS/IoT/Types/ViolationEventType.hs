{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ViolationEventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ViolationEventType (
  ViolationEventType (
    ..
    , AlarmCleared
    , AlarmInvalidated
    , InAlarm
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ViolationEventType = ViolationEventType' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern AlarmCleared :: ViolationEventType
pattern AlarmCleared = ViolationEventType' "alarm-cleared"

pattern AlarmInvalidated :: ViolationEventType
pattern AlarmInvalidated = ViolationEventType' "alarm-invalidated"

pattern InAlarm :: ViolationEventType
pattern InAlarm = ViolationEventType' "in-alarm"

{-# COMPLETE
  AlarmCleared,
  AlarmInvalidated,
  InAlarm,
  ViolationEventType' #-}

instance FromText ViolationEventType where
    parser = (ViolationEventType' . mk) <$> takeText

instance ToText ViolationEventType where
    toText (ViolationEventType' ci) = original ci

-- | Represents an enum of /known/ $ViolationEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ViolationEventType where
    toEnum i = case i of
        0 -> AlarmCleared
        1 -> AlarmInvalidated
        2 -> InAlarm
        _ -> (error . showText) $ "Unknown index for ViolationEventType: " <> toText i
    fromEnum x = case x of
        AlarmCleared -> 0
        AlarmInvalidated -> 1
        InAlarm -> 2
        ViolationEventType' name -> (error . showText) $ "Unknown ViolationEventType: " <> original name

-- | Represents the bounds of /known/ $ViolationEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ViolationEventType where
    minBound = AlarmCleared
    maxBound = InAlarm

instance Hashable     ViolationEventType
instance NFData       ViolationEventType
instance ToByteString ViolationEventType
instance ToQuery      ViolationEventType
instance ToHeader     ViolationEventType

instance FromJSON ViolationEventType where
    parseJSON = parseJSONText "ViolationEventType"
