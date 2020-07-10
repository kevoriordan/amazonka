{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265Telecine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265Telecine (
  H265Telecine (
    ..
    , HTHard
    , HTNone
    , HTSoft
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
data H265Telecine = H265Telecine' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern HTHard :: H265Telecine
pattern HTHard = H265Telecine' "HARD"

pattern HTNone :: H265Telecine
pattern HTNone = H265Telecine' "NONE"

pattern HTSoft :: H265Telecine
pattern HTSoft = H265Telecine' "SOFT"

{-# COMPLETE
  HTHard,
  HTNone,
  HTSoft,
  H265Telecine' #-}

instance FromText H265Telecine where
    parser = (H265Telecine' . mk) <$> takeText

instance ToText H265Telecine where
    toText (H265Telecine' ci) = original ci

-- | Represents an enum of /known/ $H265Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265Telecine where
    toEnum i = case i of
        0 -> HTHard
        1 -> HTNone
        2 -> HTSoft
        _ -> (error . showText) $ "Unknown index for H265Telecine: " <> toText i
    fromEnum x = case x of
        HTHard -> 0
        HTNone -> 1
        HTSoft -> 2
        H265Telecine' name -> (error . showText) $ "Unknown H265Telecine: " <> original name

-- | Represents the bounds of /known/ $H265Telecine.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265Telecine where
    minBound = HTHard
    maxBound = HTSoft

instance Hashable     H265Telecine
instance NFData       H265Telecine
instance ToByteString H265Telecine
instance ToQuery      H265Telecine
instance ToHeader     H265Telecine

instance ToJSON H265Telecine where
    toJSON = toJSONText

instance FromJSON H265Telecine where
    parseJSON = parseJSONText "H265Telecine"
