{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumFramerate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationMaximumFramerate (
  ReservationMaximumFramerate (
    ..
    , Max30Fps
    , Max60Fps
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Maximum framerate in frames per second (Outputs only)
data ReservationMaximumFramerate = ReservationMaximumFramerate' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern Max30Fps :: ReservationMaximumFramerate
pattern Max30Fps = ReservationMaximumFramerate' "MAX_30_FPS"

pattern Max60Fps :: ReservationMaximumFramerate
pattern Max60Fps = ReservationMaximumFramerate' "MAX_60_FPS"

{-# COMPLETE
  Max30Fps,
  Max60Fps,
  ReservationMaximumFramerate' #-}

instance FromText ReservationMaximumFramerate where
    parser = (ReservationMaximumFramerate' . mk) <$> takeText

instance ToText ReservationMaximumFramerate where
    toText (ReservationMaximumFramerate' ci) = original ci

-- | Represents an enum of /known/ $ReservationMaximumFramerate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationMaximumFramerate where
    toEnum i = case i of
        0 -> Max30Fps
        1 -> Max60Fps
        _ -> (error . showText) $ "Unknown index for ReservationMaximumFramerate: " <> toText i
    fromEnum x = case x of
        Max30Fps -> 0
        Max60Fps -> 1
        ReservationMaximumFramerate' name -> (error . showText) $ "Unknown ReservationMaximumFramerate: " <> original name

-- | Represents the bounds of /known/ $ReservationMaximumFramerate.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationMaximumFramerate where
    minBound = Max30Fps
    maxBound = Max60Fps

instance Hashable     ReservationMaximumFramerate
instance NFData       ReservationMaximumFramerate
instance ToByteString ReservationMaximumFramerate
instance ToQuery      ReservationMaximumFramerate
instance ToHeader     ReservationMaximumFramerate

instance FromJSON ReservationMaximumFramerate where
    parseJSON = parseJSONText "ReservationMaximumFramerate"
