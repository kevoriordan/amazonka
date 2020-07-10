{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2CodecLevel (
  Mpeg2CodecLevel (
    ..
    , MCLAuto
    , MCLHIGH1440
    , MCLHigh
    , MCLLow
    , MCLMain
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
data Mpeg2CodecLevel = Mpeg2CodecLevel' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MCLAuto :: Mpeg2CodecLevel
pattern MCLAuto = Mpeg2CodecLevel' "AUTO"

pattern MCLHIGH1440 :: Mpeg2CodecLevel
pattern MCLHIGH1440 = Mpeg2CodecLevel' "HIGH1440"

pattern MCLHigh :: Mpeg2CodecLevel
pattern MCLHigh = Mpeg2CodecLevel' "HIGH"

pattern MCLLow :: Mpeg2CodecLevel
pattern MCLLow = Mpeg2CodecLevel' "LOW"

pattern MCLMain :: Mpeg2CodecLevel
pattern MCLMain = Mpeg2CodecLevel' "MAIN"

{-# COMPLETE
  MCLAuto,
  MCLHIGH1440,
  MCLHigh,
  MCLLow,
  MCLMain,
  Mpeg2CodecLevel' #-}

instance FromText Mpeg2CodecLevel where
    parser = (Mpeg2CodecLevel' . mk) <$> takeText

instance ToText Mpeg2CodecLevel where
    toText (Mpeg2CodecLevel' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2CodecLevel where
    toEnum i = case i of
        0 -> MCLAuto
        1 -> MCLHIGH1440
        2 -> MCLHigh
        3 -> MCLLow
        4 -> MCLMain
        _ -> (error . showText) $ "Unknown index for Mpeg2CodecLevel: " <> toText i
    fromEnum x = case x of
        MCLAuto -> 0
        MCLHIGH1440 -> 1
        MCLHigh -> 2
        MCLLow -> 3
        MCLMain -> 4
        Mpeg2CodecLevel' name -> (error . showText) $ "Unknown Mpeg2CodecLevel: " <> original name

-- | Represents the bounds of /known/ $Mpeg2CodecLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2CodecLevel where
    minBound = MCLAuto
    maxBound = MCLMain

instance Hashable     Mpeg2CodecLevel
instance NFData       Mpeg2CodecLevel
instance ToByteString Mpeg2CodecLevel
instance ToQuery      Mpeg2CodecLevel
instance ToHeader     Mpeg2CodecLevel

instance ToJSON Mpeg2CodecLevel where
    toJSON = toJSONText

instance FromJSON Mpeg2CodecLevel where
    parseJSON = parseJSONText "Mpeg2CodecLevel"
