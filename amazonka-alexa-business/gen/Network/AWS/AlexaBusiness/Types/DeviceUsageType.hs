{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceUsageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceUsageType (
  DeviceUsageType (
    ..
    , Voice
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeviceUsageType = DeviceUsageType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Voice :: DeviceUsageType
pattern Voice = DeviceUsageType' "VOICE"

{-# COMPLETE
  Voice,
  DeviceUsageType' #-}

instance FromText DeviceUsageType where
    parser = (DeviceUsageType' . mk) <$> takeText

instance ToText DeviceUsageType where
    toText (DeviceUsageType' ci) = original ci

-- | Represents an enum of /known/ $DeviceUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeviceUsageType where
    toEnum i = case i of
        0 -> Voice
        _ -> (error . showText) $ "Unknown index for DeviceUsageType: " <> toText i
    fromEnum x = case x of
        Voice -> 0
        DeviceUsageType' name -> (error . showText) $ "Unknown DeviceUsageType: " <> original name

-- | Represents the bounds of /known/ $DeviceUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeviceUsageType where
    minBound = Voice
    maxBound = Voice

instance Hashable     DeviceUsageType
instance NFData       DeviceUsageType
instance ToByteString DeviceUsageType
instance ToQuery      DeviceUsageType
instance ToHeader     DeviceUsageType

instance ToJSON DeviceUsageType where
    toJSON = toJSONText
