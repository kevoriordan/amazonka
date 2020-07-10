{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264Telecine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264Telecine (
  H264Telecine (
    ..
    , HHard
    , HNone
    , HSoft
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
data H264Telecine = H264Telecine' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern HHard :: H264Telecine
pattern HHard = H264Telecine' "HARD"

pattern HNone :: H264Telecine
pattern HNone = H264Telecine' "NONE"

pattern HSoft :: H264Telecine
pattern HSoft = H264Telecine' "SOFT"

{-# COMPLETE
  HHard,
  HNone,
  HSoft,
  H264Telecine' #-}

instance FromText H264Telecine where
    parser = (H264Telecine' . mk) <$> takeText

instance ToText H264Telecine where
    toText (H264Telecine' ci) = original ci

-- | Represents an enum of /known/ $H264Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264Telecine where
    toEnum i = case i of
        0 -> HHard
        1 -> HNone
        2 -> HSoft
        _ -> (error . showText) $ "Unknown index for H264Telecine: " <> toText i
    fromEnum x = case x of
        HHard -> 0
        HNone -> 1
        HSoft -> 2
        H264Telecine' name -> (error . showText) $ "Unknown H264Telecine: " <> original name

-- | Represents the bounds of /known/ $H264Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264Telecine where
    minBound = HHard
    maxBound = HSoft

instance Hashable     H264Telecine
instance NFData       H264Telecine
instance ToByteString H264Telecine
instance ToQuery      H264Telecine
instance ToHeader     H264Telecine

instance ToJSON H264Telecine where
    toJSON = toJSONText

instance FromJSON H264Telecine where
    parseJSON = parseJSONText "H264Telecine"
