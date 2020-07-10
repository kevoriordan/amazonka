{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputMaximumBitrate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputMaximumBitrate (
  InputMaximumBitrate (
    ..
    , Max10Mbps
    , Max20Mbps
    , Max50Mbps
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
data InputMaximumBitrate = InputMaximumBitrate' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Max10Mbps :: InputMaximumBitrate
pattern Max10Mbps = InputMaximumBitrate' "MAX_10_MBPS"

pattern Max20Mbps :: InputMaximumBitrate
pattern Max20Mbps = InputMaximumBitrate' "MAX_20_MBPS"

pattern Max50Mbps :: InputMaximumBitrate
pattern Max50Mbps = InputMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  Max10Mbps,
  Max20Mbps,
  Max50Mbps,
  InputMaximumBitrate' #-}

instance FromText InputMaximumBitrate where
    parser = (InputMaximumBitrate' . mk) <$> takeText

instance ToText InputMaximumBitrate where
    toText (InputMaximumBitrate' ci) = original ci

-- | Represents an enum of /known/ $InputMaximumBitrate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputMaximumBitrate where
    toEnum i = case i of
        0 -> Max10Mbps
        1 -> Max20Mbps
        2 -> Max50Mbps
        _ -> (error . showText) $ "Unknown index for InputMaximumBitrate: " <> toText i
    fromEnum x = case x of
        Max10Mbps -> 0
        Max20Mbps -> 1
        Max50Mbps -> 2
        InputMaximumBitrate' name -> (error . showText) $ "Unknown InputMaximumBitrate: " <> original name

-- | Represents the bounds of /known/ $InputMaximumBitrate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputMaximumBitrate where
    minBound = Max10Mbps
    maxBound = Max50Mbps

instance Hashable     InputMaximumBitrate
instance NFData       InputMaximumBitrate
instance ToByteString InputMaximumBitrate
instance ToQuery      InputMaximumBitrate
instance ToHeader     InputMaximumBitrate

instance ToJSON InputMaximumBitrate where
    toJSON = toJSONText

instance FromJSON InputMaximumBitrate where
    parseJSON = parseJSONText "InputMaximumBitrate"
