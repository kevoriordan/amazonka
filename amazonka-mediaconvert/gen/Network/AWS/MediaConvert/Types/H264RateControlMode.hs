{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264RateControlMode (
  H264RateControlMode (
    ..
    , HCbr
    , HQvbr
    , HVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
data H264RateControlMode = H264RateControlMode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern HCbr :: H264RateControlMode
pattern HCbr = H264RateControlMode' "CBR"

pattern HQvbr :: H264RateControlMode
pattern HQvbr = H264RateControlMode' "QVBR"

pattern HVbr :: H264RateControlMode
pattern HVbr = H264RateControlMode' "VBR"

{-# COMPLETE
  HCbr,
  HQvbr,
  HVbr,
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
        0 -> HCbr
        1 -> HQvbr
        2 -> HVbr
        _ -> (error . showText) $ "Unknown index for H264RateControlMode: " <> toText i
    fromEnum x = case x of
        HCbr -> 0
        HQvbr -> 1
        HVbr -> 2
        H264RateControlMode' name -> (error . showText) $ "Unknown H264RateControlMode: " <> original name

-- | Represents the bounds of /known/ $H264RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264RateControlMode where
    minBound = HCbr
    maxBound = HVbr

instance Hashable     H264RateControlMode
instance NFData       H264RateControlMode
instance ToByteString H264RateControlMode
instance ToQuery      H264RateControlMode
instance ToHeader     H264RateControlMode

instance ToJSON H264RateControlMode where
    toJSON = toJSONText

instance FromJSON H264RateControlMode where
    parseJSON = parseJSONText "H264RateControlMode"
