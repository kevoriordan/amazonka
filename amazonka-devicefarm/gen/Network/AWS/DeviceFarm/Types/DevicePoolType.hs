{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePoolType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DevicePoolType (
  DevicePoolType (
    ..
    , DPTCurated
    , DPTPrivate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DevicePoolType = DevicePoolType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DPTCurated :: DevicePoolType
pattern DPTCurated = DevicePoolType' "CURATED"

pattern DPTPrivate :: DevicePoolType
pattern DPTPrivate = DevicePoolType' "PRIVATE"

{-# COMPLETE
  DPTCurated,
  DPTPrivate,
  DevicePoolType' #-}

instance FromText DevicePoolType where
    parser = (DevicePoolType' . mk) <$> takeText

instance ToText DevicePoolType where
    toText (DevicePoolType' ci) = original ci

-- | Represents an enum of /known/ $DevicePoolType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DevicePoolType where
    toEnum i = case i of
        0 -> DPTCurated
        1 -> DPTPrivate
        _ -> (error . showText) $ "Unknown index for DevicePoolType: " <> toText i
    fromEnum x = case x of
        DPTCurated -> 0
        DPTPrivate -> 1
        DevicePoolType' name -> (error . showText) $ "Unknown DevicePoolType: " <> original name

-- | Represents the bounds of /known/ $DevicePoolType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DevicePoolType where
    minBound = DPTCurated
    maxBound = DPTPrivate

instance Hashable     DevicePoolType
instance NFData       DevicePoolType
instance ToByteString DevicePoolType
instance ToQuery      DevicePoolType
instance ToHeader     DevicePoolType

instance ToJSON DevicePoolType where
    toJSON = toJSONText

instance FromJSON DevicePoolType where
    parseJSON = parseJSONText "DevicePoolType"
