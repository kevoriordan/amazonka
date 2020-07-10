{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M3u8PcrControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M3u8PcrControl (
  M3u8PcrControl (
    ..
    , MPCConfiguredPcrPeriod
    , MPCPcrEveryPesPacket
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
data M3u8PcrControl = M3u8PcrControl' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern MPCConfiguredPcrPeriod :: M3u8PcrControl
pattern MPCConfiguredPcrPeriod = M3u8PcrControl' "CONFIGURED_PCR_PERIOD"

pattern MPCPcrEveryPesPacket :: M3u8PcrControl
pattern MPCPcrEveryPesPacket = M3u8PcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  MPCConfiguredPcrPeriod,
  MPCPcrEveryPesPacket,
  M3u8PcrControl' #-}

instance FromText M3u8PcrControl where
    parser = (M3u8PcrControl' . mk) <$> takeText

instance ToText M3u8PcrControl where
    toText (M3u8PcrControl' ci) = original ci

-- | Represents an enum of /known/ $M3u8PcrControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M3u8PcrControl where
    toEnum i = case i of
        0 -> MPCConfiguredPcrPeriod
        1 -> MPCPcrEveryPesPacket
        _ -> (error . showText) $ "Unknown index for M3u8PcrControl: " <> toText i
    fromEnum x = case x of
        MPCConfiguredPcrPeriod -> 0
        MPCPcrEveryPesPacket -> 1
        M3u8PcrControl' name -> (error . showText) $ "Unknown M3u8PcrControl: " <> original name

-- | Represents the bounds of /known/ $M3u8PcrControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M3u8PcrControl where
    minBound = MPCConfiguredPcrPeriod
    maxBound = MPCPcrEveryPesPacket

instance Hashable     M3u8PcrControl
instance NFData       M3u8PcrControl
instance ToByteString M3u8PcrControl
instance ToQuery      M3u8PcrControl
instance ToHeader     M3u8PcrControl

instance ToJSON M3u8PcrControl where
    toJSON = toJSONText

instance FromJSON M3u8PcrControl where
    parseJSON = parseJSONText "M3u8PcrControl"
