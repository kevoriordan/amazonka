{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationCodec (
  ReservationCodec (
    ..
    , RCAudio
    , RCAvc
    , RCHevc
    , RCMPEG2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Codec, 'MPEG2', 'AVC', 'HEVC', or 'AUDIO'
data ReservationCodec = ReservationCodec' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern RCAudio :: ReservationCodec
pattern RCAudio = ReservationCodec' "AUDIO"

pattern RCAvc :: ReservationCodec
pattern RCAvc = ReservationCodec' "AVC"

pattern RCHevc :: ReservationCodec
pattern RCHevc = ReservationCodec' "HEVC"

pattern RCMPEG2 :: ReservationCodec
pattern RCMPEG2 = ReservationCodec' "MPEG2"

{-# COMPLETE
  RCAudio,
  RCAvc,
  RCHevc,
  RCMPEG2,
  ReservationCodec' #-}

instance FromText ReservationCodec where
    parser = (ReservationCodec' . mk) <$> takeText

instance ToText ReservationCodec where
    toText (ReservationCodec' ci) = original ci

-- | Represents an enum of /known/ $ReservationCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReservationCodec where
    toEnum i = case i of
        0 -> RCAudio
        1 -> RCAvc
        2 -> RCHevc
        3 -> RCMPEG2
        _ -> (error . showText) $ "Unknown index for ReservationCodec: " <> toText i
    fromEnum x = case x of
        RCAudio -> 0
        RCAvc -> 1
        RCHevc -> 2
        RCMPEG2 -> 3
        ReservationCodec' name -> (error . showText) $ "Unknown ReservationCodec: " <> original name

-- | Represents the bounds of /known/ $ReservationCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReservationCodec where
    minBound = RCAudio
    maxBound = RCMPEG2

instance Hashable     ReservationCodec
instance NFData       ReservationCodec
instance ToByteString ReservationCodec
instance ToQuery      ReservationCodec
instance ToHeader     ReservationCodec

instance FromJSON ReservationCodec where
    parseJSON = parseJSONText "ReservationCodec"
