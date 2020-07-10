{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ThingIndexingMode (
  ThingIndexingMode (
    ..
    , TIMOff
    , TIMRegistry
    , TIMRegistryAndShadow
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ThingIndexingMode = ThingIndexingMode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern TIMOff :: ThingIndexingMode
pattern TIMOff = ThingIndexingMode' "OFF"

pattern TIMRegistry :: ThingIndexingMode
pattern TIMRegistry = ThingIndexingMode' "REGISTRY"

pattern TIMRegistryAndShadow :: ThingIndexingMode
pattern TIMRegistryAndShadow = ThingIndexingMode' "REGISTRY_AND_SHADOW"

{-# COMPLETE
  TIMOff,
  TIMRegistry,
  TIMRegistryAndShadow,
  ThingIndexingMode' #-}

instance FromText ThingIndexingMode where
    parser = (ThingIndexingMode' . mk) <$> takeText

instance ToText ThingIndexingMode where
    toText (ThingIndexingMode' ci) = original ci

-- | Represents an enum of /known/ $ThingIndexingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ThingIndexingMode where
    toEnum i = case i of
        0 -> TIMOff
        1 -> TIMRegistry
        2 -> TIMRegistryAndShadow
        _ -> (error . showText) $ "Unknown index for ThingIndexingMode: " <> toText i
    fromEnum x = case x of
        TIMOff -> 0
        TIMRegistry -> 1
        TIMRegistryAndShadow -> 2
        ThingIndexingMode' name -> (error . showText) $ "Unknown ThingIndexingMode: " <> original name

-- | Represents the bounds of /known/ $ThingIndexingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ThingIndexingMode where
    minBound = TIMOff
    maxBound = TIMRegistryAndShadow

instance Hashable     ThingIndexingMode
instance NFData       ThingIndexingMode
instance ToByteString ThingIndexingMode
instance ToQuery      ThingIndexingMode
instance ToHeader     ThingIndexingMode

instance ToJSON ThingIndexingMode where
    toJSON = toJSONText

instance FromJSON ThingIndexingMode where
    parseJSON = parseJSONText "ThingIndexingMode"
