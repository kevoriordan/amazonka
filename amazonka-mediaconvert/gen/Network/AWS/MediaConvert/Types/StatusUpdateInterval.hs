{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StatusUpdateInterval
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.StatusUpdateInterval (
  StatusUpdateInterval (
    ..
    , Seconds10
    , Seconds12
    , Seconds120
    , Seconds15
    , Seconds180
    , Seconds20
    , Seconds240
    , Seconds30
    , Seconds300
    , Seconds360
    , Seconds420
    , Seconds480
    , Seconds540
    , Seconds60
    , Seconds600
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
data StatusUpdateInterval = StatusUpdateInterval' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Seconds10 :: StatusUpdateInterval
pattern Seconds10 = StatusUpdateInterval' "SECONDS_10"

pattern Seconds12 :: StatusUpdateInterval
pattern Seconds12 = StatusUpdateInterval' "SECONDS_12"

pattern Seconds120 :: StatusUpdateInterval
pattern Seconds120 = StatusUpdateInterval' "SECONDS_120"

pattern Seconds15 :: StatusUpdateInterval
pattern Seconds15 = StatusUpdateInterval' "SECONDS_15"

pattern Seconds180 :: StatusUpdateInterval
pattern Seconds180 = StatusUpdateInterval' "SECONDS_180"

pattern Seconds20 :: StatusUpdateInterval
pattern Seconds20 = StatusUpdateInterval' "SECONDS_20"

pattern Seconds240 :: StatusUpdateInterval
pattern Seconds240 = StatusUpdateInterval' "SECONDS_240"

pattern Seconds30 :: StatusUpdateInterval
pattern Seconds30 = StatusUpdateInterval' "SECONDS_30"

pattern Seconds300 :: StatusUpdateInterval
pattern Seconds300 = StatusUpdateInterval' "SECONDS_300"

pattern Seconds360 :: StatusUpdateInterval
pattern Seconds360 = StatusUpdateInterval' "SECONDS_360"

pattern Seconds420 :: StatusUpdateInterval
pattern Seconds420 = StatusUpdateInterval' "SECONDS_420"

pattern Seconds480 :: StatusUpdateInterval
pattern Seconds480 = StatusUpdateInterval' "SECONDS_480"

pattern Seconds540 :: StatusUpdateInterval
pattern Seconds540 = StatusUpdateInterval' "SECONDS_540"

pattern Seconds60 :: StatusUpdateInterval
pattern Seconds60 = StatusUpdateInterval' "SECONDS_60"

pattern Seconds600 :: StatusUpdateInterval
pattern Seconds600 = StatusUpdateInterval' "SECONDS_600"

{-# COMPLETE
  Seconds10,
  Seconds12,
  Seconds120,
  Seconds15,
  Seconds180,
  Seconds20,
  Seconds240,
  Seconds30,
  Seconds300,
  Seconds360,
  Seconds420,
  Seconds480,
  Seconds540,
  Seconds60,
  Seconds600,
  StatusUpdateInterval' #-}

instance FromText StatusUpdateInterval where
    parser = (StatusUpdateInterval' . mk) <$> takeText

instance ToText StatusUpdateInterval where
    toText (StatusUpdateInterval' ci) = original ci

-- | Represents an enum of /known/ $StatusUpdateInterval.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatusUpdateInterval where
    toEnum i = case i of
        0 -> Seconds10
        1 -> Seconds12
        2 -> Seconds120
        3 -> Seconds15
        4 -> Seconds180
        5 -> Seconds20
        6 -> Seconds240
        7 -> Seconds30
        8 -> Seconds300
        9 -> Seconds360
        10 -> Seconds420
        11 -> Seconds480
        12 -> Seconds540
        13 -> Seconds60
        14 -> Seconds600
        _ -> (error . showText) $ "Unknown index for StatusUpdateInterval: " <> toText i
    fromEnum x = case x of
        Seconds10 -> 0
        Seconds12 -> 1
        Seconds120 -> 2
        Seconds15 -> 3
        Seconds180 -> 4
        Seconds20 -> 5
        Seconds240 -> 6
        Seconds30 -> 7
        Seconds300 -> 8
        Seconds360 -> 9
        Seconds420 -> 10
        Seconds480 -> 11
        Seconds540 -> 12
        Seconds60 -> 13
        Seconds600 -> 14
        StatusUpdateInterval' name -> (error . showText) $ "Unknown StatusUpdateInterval: " <> original name

-- | Represents the bounds of /known/ $StatusUpdateInterval.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatusUpdateInterval where
    minBound = Seconds10
    maxBound = Seconds600

instance Hashable     StatusUpdateInterval
instance NFData       StatusUpdateInterval
instance ToByteString StatusUpdateInterval
instance ToQuery      StatusUpdateInterval
instance ToHeader     StatusUpdateInterval

instance ToJSON StatusUpdateInterval where
    toJSON = toJSONText

instance FromJSON StatusUpdateInterval where
    parseJSON = parseJSONText "StatusUpdateInterval"
