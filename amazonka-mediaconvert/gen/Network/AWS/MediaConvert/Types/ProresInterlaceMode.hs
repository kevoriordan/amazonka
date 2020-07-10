{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresInterlaceMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ProresInterlaceMode (
  ProresInterlaceMode (
    ..
    , PIMBottomField
    , PIMFollowBottomField
    , PIMFollowTopField
    , PIMProgressive
    , PIMTopField
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.
--
--   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".
--   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data ProresInterlaceMode = ProresInterlaceMode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern PIMBottomField :: ProresInterlaceMode
pattern PIMBottomField = ProresInterlaceMode' "BOTTOM_FIELD"

pattern PIMFollowBottomField :: ProresInterlaceMode
pattern PIMFollowBottomField = ProresInterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern PIMFollowTopField :: ProresInterlaceMode
pattern PIMFollowTopField = ProresInterlaceMode' "FOLLOW_TOP_FIELD"

pattern PIMProgressive :: ProresInterlaceMode
pattern PIMProgressive = ProresInterlaceMode' "PROGRESSIVE"

pattern PIMTopField :: ProresInterlaceMode
pattern PIMTopField = ProresInterlaceMode' "TOP_FIELD"

{-# COMPLETE
  PIMBottomField,
  PIMFollowBottomField,
  PIMFollowTopField,
  PIMProgressive,
  PIMTopField,
  ProresInterlaceMode' #-}

instance FromText ProresInterlaceMode where
    parser = (ProresInterlaceMode' . mk) <$> takeText

instance ToText ProresInterlaceMode where
    toText (ProresInterlaceMode' ci) = original ci

-- | Represents an enum of /known/ $ProresInterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProresInterlaceMode where
    toEnum i = case i of
        0 -> PIMBottomField
        1 -> PIMFollowBottomField
        2 -> PIMFollowTopField
        3 -> PIMProgressive
        4 -> PIMTopField
        _ -> (error . showText) $ "Unknown index for ProresInterlaceMode: " <> toText i
    fromEnum x = case x of
        PIMBottomField -> 0
        PIMFollowBottomField -> 1
        PIMFollowTopField -> 2
        PIMProgressive -> 3
        PIMTopField -> 4
        ProresInterlaceMode' name -> (error . showText) $ "Unknown ProresInterlaceMode: " <> original name

-- | Represents the bounds of /known/ $ProresInterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProresInterlaceMode where
    minBound = PIMBottomField
    maxBound = PIMTopField

instance Hashable     ProresInterlaceMode
instance NFData       ProresInterlaceMode
instance ToByteString ProresInterlaceMode
instance ToQuery      ProresInterlaceMode
instance ToHeader     ProresInterlaceMode

instance ToJSON ProresInterlaceMode where
    toJSON = toJSONText

instance FromJSON ProresInterlaceMode where
    parseJSON = parseJSONText "ProresInterlaceMode"
