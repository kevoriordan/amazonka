{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TimecodeConfigSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.TimecodeConfigSource (
  TimecodeConfigSource (
    ..
    , Embedded
    , Systemclock
    , Zerobased
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Timecode Config Source
data TimecodeConfigSource = TimecodeConfigSource' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Embedded :: TimecodeConfigSource
pattern Embedded = TimecodeConfigSource' "EMBEDDED"

pattern Systemclock :: TimecodeConfigSource
pattern Systemclock = TimecodeConfigSource' "SYSTEMCLOCK"

pattern Zerobased :: TimecodeConfigSource
pattern Zerobased = TimecodeConfigSource' "ZEROBASED"

{-# COMPLETE
  Embedded,
  Systemclock,
  Zerobased,
  TimecodeConfigSource' #-}

instance FromText TimecodeConfigSource where
    parser = (TimecodeConfigSource' . mk) <$> takeText

instance ToText TimecodeConfigSource where
    toText (TimecodeConfigSource' ci) = original ci

-- | Represents an enum of /known/ $TimecodeConfigSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TimecodeConfigSource where
    toEnum i = case i of
        0 -> Embedded
        1 -> Systemclock
        2 -> Zerobased
        _ -> (error . showText) $ "Unknown index for TimecodeConfigSource: " <> toText i
    fromEnum x = case x of
        Embedded -> 0
        Systemclock -> 1
        Zerobased -> 2
        TimecodeConfigSource' name -> (error . showText) $ "Unknown TimecodeConfigSource: " <> original name

-- | Represents the bounds of /known/ $TimecodeConfigSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TimecodeConfigSource where
    minBound = Embedded
    maxBound = Zerobased

instance Hashable     TimecodeConfigSource
instance NFData       TimecodeConfigSource
instance ToByteString TimecodeConfigSource
instance ToQuery      TimecodeConfigSource
instance ToHeader     TimecodeConfigSource

instance ToJSON TimecodeConfigSource where
    toJSON = toJSONText

instance FromJSON TimecodeConfigSource where
    parseJSON = parseJSONText "TimecodeConfigSource"
