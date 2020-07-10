{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsPcrControl (
  M2tsPcrControl (
    ..
    , ConfiguredPcrPeriod
    , PcrEveryPesPacket
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
data M2tsPcrControl = M2tsPcrControl' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ConfiguredPcrPeriod :: M2tsPcrControl
pattern ConfiguredPcrPeriod = M2tsPcrControl' "CONFIGURED_PCR_PERIOD"

pattern PcrEveryPesPacket :: M2tsPcrControl
pattern PcrEveryPesPacket = M2tsPcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  ConfiguredPcrPeriod,
  PcrEveryPesPacket,
  M2tsPcrControl' #-}

instance FromText M2tsPcrControl where
    parser = (M2tsPcrControl' . mk) <$> takeText

instance ToText M2tsPcrControl where
    toText (M2tsPcrControl' ci) = original ci

-- | Represents an enum of /known/ $M2tsPcrControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsPcrControl where
    toEnum i = case i of
        0 -> ConfiguredPcrPeriod
        1 -> PcrEveryPesPacket
        _ -> (error . showText) $ "Unknown index for M2tsPcrControl: " <> toText i
    fromEnum x = case x of
        ConfiguredPcrPeriod -> 0
        PcrEveryPesPacket -> 1
        M2tsPcrControl' name -> (error . showText) $ "Unknown M2tsPcrControl: " <> original name

-- | Represents the bounds of /known/ $M2tsPcrControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsPcrControl where
    minBound = ConfiguredPcrPeriod
    maxBound = PcrEveryPesPacket

instance Hashable     M2tsPcrControl
instance NFData       M2tsPcrControl
instance ToByteString M2tsPcrControl
instance ToQuery      M2tsPcrControl
instance ToHeader     M2tsPcrControl

instance ToJSON M2tsPcrControl where
    toJSON = toJSONText

instance FromJSON M2tsPcrControl where
    parseJSON = parseJSONText "M2tsPcrControl"
