{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceAvailability (
  DeviceAvailability (
    ..
    , Available
    , Busy
    , HighlyAvailable
    , TemporaryNotAvailable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceAvailability = DeviceAvailability' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Available :: DeviceAvailability
pattern Available = DeviceAvailability' "AVAILABLE"

pattern Busy :: DeviceAvailability
pattern Busy = DeviceAvailability' "BUSY"

pattern HighlyAvailable :: DeviceAvailability
pattern HighlyAvailable = DeviceAvailability' "HIGHLY_AVAILABLE"

pattern TemporaryNotAvailable :: DeviceAvailability
pattern TemporaryNotAvailable = DeviceAvailability' "TEMPORARY_NOT_AVAILABLE"

{-# COMPLETE
  Available,
  Busy,
  HighlyAvailable,
  TemporaryNotAvailable,
  DeviceAvailability' #-}

instance FromText DeviceAvailability where
    parser = (DeviceAvailability' . mk) <$> takeText

instance ToText DeviceAvailability where
    toText (DeviceAvailability' ci) = original ci

-- | Represents an enum of /known/ $DeviceAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceAvailability where
    toEnum i = case i of
        0 -> Available
        1 -> Busy
        2 -> HighlyAvailable
        3 -> TemporaryNotAvailable
        _ -> (error . showText) $ "Unknown index for DeviceAvailability: " <> toText i
    fromEnum x = case x of
        Available -> 0
        Busy -> 1
        HighlyAvailable -> 2
        TemporaryNotAvailable -> 3
        DeviceAvailability' name -> (error . showText) $ "Unknown DeviceAvailability: " <> original name

-- | Represents the bounds of /known/ $DeviceAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceAvailability where
    minBound = Available
    maxBound = TemporaryNotAvailable

instance Hashable     DeviceAvailability
instance NFData       DeviceAvailability
instance ToByteString DeviceAvailability
instance ToQuery      DeviceAvailability
instance ToHeader     DeviceAvailability

instance FromJSON DeviceAvailability where
    parseJSON = parseJSONText "DeviceAvailability"
