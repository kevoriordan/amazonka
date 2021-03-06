{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp3RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mp3RateControlMode (
  Mp3RateControlMode (
    ..
    , MRCMCbr
    , MRCMVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify whether the service encodes this MP3 audio output with a constant bitrate (CBR) or a variable bitrate (VBR).
data Mp3RateControlMode = Mp3RateControlMode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern MRCMCbr :: Mp3RateControlMode
pattern MRCMCbr = Mp3RateControlMode' "CBR"

pattern MRCMVbr :: Mp3RateControlMode
pattern MRCMVbr = Mp3RateControlMode' "VBR"

{-# COMPLETE
  MRCMCbr,
  MRCMVbr,
  Mp3RateControlMode' #-}

instance FromText Mp3RateControlMode where
    parser = (Mp3RateControlMode' . mk) <$> takeText

instance ToText Mp3RateControlMode where
    toText (Mp3RateControlMode' ci) = original ci

-- | Represents an enum of /known/ $Mp3RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mp3RateControlMode where
    toEnum i = case i of
        0 -> MRCMCbr
        1 -> MRCMVbr
        _ -> (error . showText) $ "Unknown index for Mp3RateControlMode: " <> toText i
    fromEnum x = case x of
        MRCMCbr -> 0
        MRCMVbr -> 1
        Mp3RateControlMode' name -> (error . showText) $ "Unknown Mp3RateControlMode: " <> original name

-- | Represents the bounds of /known/ $Mp3RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mp3RateControlMode where
    minBound = MRCMCbr
    maxBound = MRCMVbr

instance Hashable     Mp3RateControlMode
instance NFData       Mp3RateControlMode
instance ToByteString Mp3RateControlMode
instance ToQuery      Mp3RateControlMode
instance ToHeader     Mp3RateControlMode

instance ToJSON Mp3RateControlMode where
    toJSON = toJSONText

instance FromJSON Mp3RateControlMode where
    parseJSON = parseJSONText "Mp3RateControlMode"
