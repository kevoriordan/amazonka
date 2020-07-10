{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264RateControlMode (
  H264RateControlMode (
    ..
    , HRCMCbr
    , HRCMMultiplex
    , HRCMQvbr
    , HRCMVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Rate Control Mode
data H264RateControlMode = H264RateControlMode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern HRCMCbr :: H264RateControlMode
pattern HRCMCbr = H264RateControlMode' "CBR"

pattern HRCMMultiplex :: H264RateControlMode
pattern HRCMMultiplex = H264RateControlMode' "MULTIPLEX"

pattern HRCMQvbr :: H264RateControlMode
pattern HRCMQvbr = H264RateControlMode' "QVBR"

pattern HRCMVbr :: H264RateControlMode
pattern HRCMVbr = H264RateControlMode' "VBR"

{-# COMPLETE
  HRCMCbr,
  HRCMMultiplex,
  HRCMQvbr,
  HRCMVbr,
  H264RateControlMode' #-}

instance FromText H264RateControlMode where
    parser = (H264RateControlMode' . mk) <$> takeText

instance ToText H264RateControlMode where
    toText (H264RateControlMode' ci) = original ci

-- | Represents an enum of /known/ $H264RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264RateControlMode where
    toEnum i = case i of
        0 -> HRCMCbr
        1 -> HRCMMultiplex
        2 -> HRCMQvbr
        3 -> HRCMVbr
        _ -> (error . showText) $ "Unknown index for H264RateControlMode: " <> toText i
    fromEnum x = case x of
        HRCMCbr -> 0
        HRCMMultiplex -> 1
        HRCMQvbr -> 2
        HRCMVbr -> 3
        H264RateControlMode' name -> (error . showText) $ "Unknown H264RateControlMode: " <> original name

-- | Represents the bounds of /known/ $H264RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264RateControlMode where
    minBound = HRCMCbr
    maxBound = HRCMVbr

instance Hashable     H264RateControlMode
instance NFData       H264RateControlMode
instance ToByteString H264RateControlMode
instance ToQuery      H264RateControlMode
instance ToHeader     H264RateControlMode

instance ToJSON H264RateControlMode where
    toJSON = toJSONText

instance FromJSON H264RateControlMode where
    parseJSON = parseJSONText "H264RateControlMode"
