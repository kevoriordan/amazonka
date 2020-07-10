{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization (
  Mpeg2AdaptiveQuantization (
    ..
    , MAQHigh
    , MAQLow
    , MAQMedium
    , MAQOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern MAQHigh :: Mpeg2AdaptiveQuantization
pattern MAQHigh = Mpeg2AdaptiveQuantization' "HIGH"

pattern MAQLow :: Mpeg2AdaptiveQuantization
pattern MAQLow = Mpeg2AdaptiveQuantization' "LOW"

pattern MAQMedium :: Mpeg2AdaptiveQuantization
pattern MAQMedium = Mpeg2AdaptiveQuantization' "MEDIUM"

pattern MAQOff :: Mpeg2AdaptiveQuantization
pattern MAQOff = Mpeg2AdaptiveQuantization' "OFF"

{-# COMPLETE
  MAQHigh,
  MAQLow,
  MAQMedium,
  MAQOff,
  Mpeg2AdaptiveQuantization' #-}

instance FromText Mpeg2AdaptiveQuantization where
    parser = (Mpeg2AdaptiveQuantization' . mk) <$> takeText

instance ToText Mpeg2AdaptiveQuantization where
    toText (Mpeg2AdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $Mpeg2AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Mpeg2AdaptiveQuantization where
    toEnum i = case i of
        0 -> MAQHigh
        1 -> MAQLow
        2 -> MAQMedium
        3 -> MAQOff
        _ -> (error . showText) $ "Unknown index for Mpeg2AdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        MAQHigh -> 0
        MAQLow -> 1
        MAQMedium -> 2
        MAQOff -> 3
        Mpeg2AdaptiveQuantization' name -> (error . showText) $ "Unknown Mpeg2AdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $Mpeg2AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Mpeg2AdaptiveQuantization where
    minBound = MAQHigh
    maxBound = MAQOff

instance Hashable     Mpeg2AdaptiveQuantization
instance NFData       Mpeg2AdaptiveQuantization
instance ToByteString Mpeg2AdaptiveQuantization
instance ToQuery      Mpeg2AdaptiveQuantization
instance ToHeader     Mpeg2AdaptiveQuantization

instance ToJSON Mpeg2AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2AdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2AdaptiveQuantization"
