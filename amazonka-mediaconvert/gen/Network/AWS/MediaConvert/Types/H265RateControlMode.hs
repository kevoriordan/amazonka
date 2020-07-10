{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265RateControlMode (
  H265RateControlMode (
    ..
    , HRCMCbr
    , HRCMQvbr
    , HRCMVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
data H265RateControlMode = H265RateControlMode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern HRCMCbr :: H265RateControlMode
pattern HRCMCbr = H265RateControlMode' "CBR"

pattern HRCMQvbr :: H265RateControlMode
pattern HRCMQvbr = H265RateControlMode' "QVBR"

pattern HRCMVbr :: H265RateControlMode
pattern HRCMVbr = H265RateControlMode' "VBR"

{-# COMPLETE
  HRCMCbr,
  HRCMQvbr,
  HRCMVbr,
  H265RateControlMode' #-}

instance FromText H265RateControlMode where
    parser = (H265RateControlMode' . mk) <$> takeText

instance ToText H265RateControlMode where
    toText (H265RateControlMode' ci) = original ci

-- | Represents an enum of /known/ $H265RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265RateControlMode where
    toEnum i = case i of
        0 -> HRCMCbr
        1 -> HRCMQvbr
        2 -> HRCMVbr
        _ -> (error . showText) $ "Unknown index for H265RateControlMode: " <> toText i
    fromEnum x = case x of
        HRCMCbr -> 0
        HRCMQvbr -> 1
        HRCMVbr -> 2
        H265RateControlMode' name -> (error . showText) $ "Unknown H265RateControlMode: " <> original name

-- | Represents the bounds of /known/ $H265RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265RateControlMode where
    minBound = HRCMCbr
    maxBound = HRCMVbr

instance Hashable     H265RateControlMode
instance NFData       H265RateControlMode
instance ToByteString H265RateControlMode
instance ToQuery      H265RateControlMode
instance ToHeader     H265RateControlMode

instance ToJSON H265RateControlMode where
    toJSON = toJSONText

instance FromJSON H265RateControlMode where
    parseJSON = parseJSONText "H265RateControlMode"
