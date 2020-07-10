{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputCodec
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputCodec (
  InputCodec (
    ..
    , Avc
    , Hevc
    , MPEG2
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | codec in increasing order of complexity
data InputCodec = InputCodec' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Avc :: InputCodec
pattern Avc = InputCodec' "AVC"

pattern Hevc :: InputCodec
pattern Hevc = InputCodec' "HEVC"

pattern MPEG2 :: InputCodec
pattern MPEG2 = InputCodec' "MPEG2"

{-# COMPLETE
  Avc,
  Hevc,
  MPEG2,
  InputCodec' #-}

instance FromText InputCodec where
    parser = (InputCodec' . mk) <$> takeText

instance ToText InputCodec where
    toText (InputCodec' ci) = original ci

-- | Represents an enum of /known/ $InputCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputCodec where
    toEnum i = case i of
        0 -> Avc
        1 -> Hevc
        2 -> MPEG2
        _ -> (error . showText) $ "Unknown index for InputCodec: " <> toText i
    fromEnum x = case x of
        Avc -> 0
        Hevc -> 1
        MPEG2 -> 2
        InputCodec' name -> (error . showText) $ "Unknown InputCodec: " <> original name

-- | Represents the bounds of /known/ $InputCodec.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputCodec where
    minBound = Avc
    maxBound = MPEG2

instance Hashable     InputCodec
instance NFData       InputCodec
instance ToByteString InputCodec
instance ToQuery      InputCodec
instance ToHeader     InputCodec

instance ToJSON InputCodec where
    toJSON = toJSONText

instance FromJSON InputCodec where
    parseJSON = parseJSONText "InputCodec"
