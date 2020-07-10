{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264Profile (
  H264Profile (
    ..
    , HPBaseline
    , HPHigh
    , HPHigh10BIT
    , HPHigh422
    , HPHigh42210BIT
    , HPMain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Profile
data H264Profile = H264Profile' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern HPBaseline :: H264Profile
pattern HPBaseline = H264Profile' "BASELINE"

pattern HPHigh :: H264Profile
pattern HPHigh = H264Profile' "HIGH"

pattern HPHigh10BIT :: H264Profile
pattern HPHigh10BIT = H264Profile' "HIGH_10BIT"

pattern HPHigh422 :: H264Profile
pattern HPHigh422 = H264Profile' "HIGH_422"

pattern HPHigh42210BIT :: H264Profile
pattern HPHigh42210BIT = H264Profile' "HIGH_422_10BIT"

pattern HPMain :: H264Profile
pattern HPMain = H264Profile' "MAIN"

{-# COMPLETE
  HPBaseline,
  HPHigh,
  HPHigh10BIT,
  HPHigh422,
  HPHigh42210BIT,
  HPMain,
  H264Profile' #-}

instance FromText H264Profile where
    parser = (H264Profile' . mk) <$> takeText

instance ToText H264Profile where
    toText (H264Profile' ci) = original ci

-- | Represents an enum of /known/ $H264Profile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264Profile where
    toEnum i = case i of
        0 -> HPBaseline
        1 -> HPHigh
        2 -> HPHigh10BIT
        3 -> HPHigh422
        4 -> HPHigh42210BIT
        5 -> HPMain
        _ -> (error . showText) $ "Unknown index for H264Profile: " <> toText i
    fromEnum x = case x of
        HPBaseline -> 0
        HPHigh -> 1
        HPHigh10BIT -> 2
        HPHigh422 -> 3
        HPHigh42210BIT -> 4
        HPMain -> 5
        H264Profile' name -> (error . showText) $ "Unknown H264Profile: " <> original name

-- | Represents the bounds of /known/ $H264Profile.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264Profile where
    minBound = HPBaseline
    maxBound = HPMain

instance Hashable     H264Profile
instance NFData       H264Profile
instance ToByteString H264Profile
instance ToQuery      H264Profile
instance ToHeader     H264Profile

instance ToJSON H264Profile where
    toJSON = toJSONText

instance FromJSON H264Profile where
    parseJSON = parseJSONText "H264Profile"
