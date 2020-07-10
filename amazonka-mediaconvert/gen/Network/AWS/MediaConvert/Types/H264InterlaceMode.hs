{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264InterlaceMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264InterlaceMode (
  H264InterlaceMode (
    ..
    , HIMBottomField
    , HIMFollowBottomField
    , HIMFollowTopField
    , HIMProgressive
    , HIMTopField
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type, as follows.
--
--   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".
--   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data H264InterlaceMode = H264InterlaceMode' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern HIMBottomField :: H264InterlaceMode
pattern HIMBottomField = H264InterlaceMode' "BOTTOM_FIELD"

pattern HIMFollowBottomField :: H264InterlaceMode
pattern HIMFollowBottomField = H264InterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern HIMFollowTopField :: H264InterlaceMode
pattern HIMFollowTopField = H264InterlaceMode' "FOLLOW_TOP_FIELD"

pattern HIMProgressive :: H264InterlaceMode
pattern HIMProgressive = H264InterlaceMode' "PROGRESSIVE"

pattern HIMTopField :: H264InterlaceMode
pattern HIMTopField = H264InterlaceMode' "TOP_FIELD"

{-# COMPLETE
  HIMBottomField,
  HIMFollowBottomField,
  HIMFollowTopField,
  HIMProgressive,
  HIMTopField,
  H264InterlaceMode' #-}

instance FromText H264InterlaceMode where
    parser = (H264InterlaceMode' . mk) <$> takeText

instance ToText H264InterlaceMode where
    toText (H264InterlaceMode' ci) = original ci

-- | Represents an enum of /known/ $H264InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264InterlaceMode where
    toEnum i = case i of
        0 -> HIMBottomField
        1 -> HIMFollowBottomField
        2 -> HIMFollowTopField
        3 -> HIMProgressive
        4 -> HIMTopField
        _ -> (error . showText) $ "Unknown index for H264InterlaceMode: " <> toText i
    fromEnum x = case x of
        HIMBottomField -> 0
        HIMFollowBottomField -> 1
        HIMFollowTopField -> 2
        HIMProgressive -> 3
        HIMTopField -> 4
        H264InterlaceMode' name -> (error . showText) $ "Unknown H264InterlaceMode: " <> original name

-- | Represents the bounds of /known/ $H264InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264InterlaceMode where
    minBound = HIMBottomField
    maxBound = HIMTopField

instance Hashable     H264InterlaceMode
instance NFData       H264InterlaceMode
instance ToByteString H264InterlaceMode
instance ToQuery      H264InterlaceMode
instance ToHeader     H264InterlaceMode

instance ToJSON H264InterlaceMode where
    toJSON = toJSONText

instance FromJSON H264InterlaceMode where
    parseJSON = parseJSONText "H264InterlaceMode"
