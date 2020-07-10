{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource (
  GlobalConfigurationOutputTimingSource (
    ..
    , GCOTSInputClock
    , GCOTSSystemClock
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Global Configuration Output Timing Source
data GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern GCOTSInputClock :: GlobalConfigurationOutputTimingSource
pattern GCOTSInputClock = GlobalConfigurationOutputTimingSource' "INPUT_CLOCK"

pattern GCOTSSystemClock :: GlobalConfigurationOutputTimingSource
pattern GCOTSSystemClock = GlobalConfigurationOutputTimingSource' "SYSTEM_CLOCK"

{-# COMPLETE
  GCOTSInputClock,
  GCOTSSystemClock,
  GlobalConfigurationOutputTimingSource' #-}

instance FromText GlobalConfigurationOutputTimingSource where
    parser = (GlobalConfigurationOutputTimingSource' . mk) <$> takeText

instance ToText GlobalConfigurationOutputTimingSource where
    toText (GlobalConfigurationOutputTimingSource' ci) = original ci

-- | Represents an enum of /known/ $GlobalConfigurationOutputTimingSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GlobalConfigurationOutputTimingSource where
    toEnum i = case i of
        0 -> GCOTSInputClock
        1 -> GCOTSSystemClock
        _ -> (error . showText) $ "Unknown index for GlobalConfigurationOutputTimingSource: " <> toText i
    fromEnum x = case x of
        GCOTSInputClock -> 0
        GCOTSSystemClock -> 1
        GlobalConfigurationOutputTimingSource' name -> (error . showText) $ "Unknown GlobalConfigurationOutputTimingSource: " <> original name

-- | Represents the bounds of /known/ $GlobalConfigurationOutputTimingSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GlobalConfigurationOutputTimingSource where
    minBound = GCOTSInputClock
    maxBound = GCOTSSystemClock

instance Hashable     GlobalConfigurationOutputTimingSource
instance NFData       GlobalConfigurationOutputTimingSource
instance ToByteString GlobalConfigurationOutputTimingSource
instance ToQuery      GlobalConfigurationOutputTimingSource
instance ToHeader     GlobalConfigurationOutputTimingSource

instance ToJSON GlobalConfigurationOutputTimingSource where
    toJSON = toJSONText

instance FromJSON GlobalConfigurationOutputTimingSource where
    parseJSON = parseJSONText "GlobalConfigurationOutputTimingSource"
