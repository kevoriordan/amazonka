{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SlowPal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264SlowPal (
  H264SlowPal (
    ..
    , H264SlwPlDisabled
    , H264SlwPlEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
data H264SlowPal = H264SlowPal' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern H264SlwPlDisabled :: H264SlowPal
pattern H264SlwPlDisabled = H264SlowPal' "DISABLED"

pattern H264SlwPlEnabled :: H264SlowPal
pattern H264SlwPlEnabled = H264SlowPal' "ENABLED"

{-# COMPLETE
  H264SlwPlDisabled,
  H264SlwPlEnabled,
  H264SlowPal' #-}

instance FromText H264SlowPal where
    parser = (H264SlowPal' . mk) <$> takeText

instance ToText H264SlowPal where
    toText (H264SlowPal' ci) = original ci

-- | Represents an enum of /known/ $H264SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264SlowPal where
    toEnum i = case i of
        0 -> H264SlwPlDisabled
        1 -> H264SlwPlEnabled
        _ -> (error . showText) $ "Unknown index for H264SlowPal: " <> toText i
    fromEnum x = case x of
        H264SlwPlDisabled -> 0
        H264SlwPlEnabled -> 1
        H264SlowPal' name -> (error . showText) $ "Unknown H264SlowPal: " <> original name

-- | Represents the bounds of /known/ $H264SlowPal.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264SlowPal where
    minBound = H264SlwPlDisabled
    maxBound = H264SlwPlEnabled

instance Hashable     H264SlowPal
instance NFData       H264SlowPal
instance ToByteString H264SlowPal
instance ToQuery      H264SlowPal
instance ToHeader     H264SlowPal

instance ToJSON H264SlowPal where
    toJSON = toJSONText

instance FromJSON H264SlowPal where
    parseJSON = parseJSONText "H264SlowPal"
