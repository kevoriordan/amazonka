{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2InterlaceMode (
  Mpeg2InterlaceMode (
    ..
    , MIMBottomField
    , MIMFollowBottomField
    , MIMFollowTopField
    , MIMProgressive
    , MIMTopField
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOW_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same field polarity as the source. Therefore, behavior depends on the input scan type.
--
--   - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first".
--   - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
data Mpeg2InterlaceMode = Mpeg2InterlaceMode' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern MIMBottomField :: Mpeg2InterlaceMode
pattern MIMBottomField = Mpeg2InterlaceMode' "BOTTOM_FIELD"

pattern MIMFollowBottomField :: Mpeg2InterlaceMode
pattern MIMFollowBottomField = Mpeg2InterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern MIMFollowTopField :: Mpeg2InterlaceMode
pattern MIMFollowTopField = Mpeg2InterlaceMode' "FOLLOW_TOP_FIELD"

pattern MIMProgressive :: Mpeg2InterlaceMode
pattern MIMProgressive = Mpeg2InterlaceMode' "PROGRESSIVE"

pattern MIMTopField :: Mpeg2InterlaceMode
pattern MIMTopField = Mpeg2InterlaceMode' "TOP_FIELD"

{-# COMPLETE
  MIMBottomField,
  MIMFollowBottomField,
  MIMFollowTopField,
  MIMProgressive,
  MIMTopField,
  Mpeg2InterlaceMode' #-}

instance FromText Mpeg2InterlaceMode where
    parser = (Mpeg2InterlaceMode' . mk) <$> takeText

instance ToText Mpeg2InterlaceMode where
    toText (Mpeg2InterlaceMode' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2InterlaceMode where
    toEnum i = case i of
        0 -> MIMBottomField
        1 -> MIMFollowBottomField
        2 -> MIMFollowTopField
        3 -> MIMProgressive
        4 -> MIMTopField
        _ -> (error . showText) $ "Unknown index for Mpeg2InterlaceMode: " <> toText i
    fromEnum x = case x of
        MIMBottomField -> 0
        MIMFollowBottomField -> 1
        MIMFollowTopField -> 2
        MIMProgressive -> 3
        MIMTopField -> 4
        Mpeg2InterlaceMode' name -> (error . showText) $ "Unknown Mpeg2InterlaceMode: " <> original name

-- | Represents the bounds of /known/ $Mpeg2InterlaceMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2InterlaceMode where
    minBound = MIMBottomField
    maxBound = MIMTopField

instance Hashable     Mpeg2InterlaceMode
instance NFData       Mpeg2InterlaceMode
instance ToByteString Mpeg2InterlaceMode
instance ToQuery      Mpeg2InterlaceMode
instance ToHeader     Mpeg2InterlaceMode

instance ToJSON Mpeg2InterlaceMode where
    toJSON = toJSONText

instance FromJSON Mpeg2InterlaceMode where
    parseJSON = parseJSONText "Mpeg2InterlaceMode"
