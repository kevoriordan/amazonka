{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization (
  Av1AdaptiveQuantization (
    ..
    , High
    , Higher
    , Low
    , Max
    , Medium
    , Off
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data Av1AdaptiveQuantization = Av1AdaptiveQuantization' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern High :: Av1AdaptiveQuantization
pattern High = Av1AdaptiveQuantization' "HIGH"

pattern Higher :: Av1AdaptiveQuantization
pattern Higher = Av1AdaptiveQuantization' "HIGHER"

pattern Low :: Av1AdaptiveQuantization
pattern Low = Av1AdaptiveQuantization' "LOW"

pattern Max :: Av1AdaptiveQuantization
pattern Max = Av1AdaptiveQuantization' "MAX"

pattern Medium :: Av1AdaptiveQuantization
pattern Medium = Av1AdaptiveQuantization' "MEDIUM"

pattern Off :: Av1AdaptiveQuantization
pattern Off = Av1AdaptiveQuantization' "OFF"

{-# COMPLETE
  High,
  Higher,
  Low,
  Max,
  Medium,
  Off,
  Av1AdaptiveQuantization' #-}

instance FromText Av1AdaptiveQuantization where
    parser = (Av1AdaptiveQuantization' . mk) <$> takeText

instance ToText Av1AdaptiveQuantization where
    toText (Av1AdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $Av1AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Av1AdaptiveQuantization where
    toEnum i = case i of
        0 -> High
        1 -> Higher
        2 -> Low
        3 -> Max
        4 -> Medium
        5 -> Off
        _ -> (error . showText) $ "Unknown index for Av1AdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        High -> 0
        Higher -> 1
        Low -> 2
        Max -> 3
        Medium -> 4
        Off -> 5
        Av1AdaptiveQuantization' name -> (error . showText) $ "Unknown Av1AdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $Av1AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Av1AdaptiveQuantization where
    minBound = High
    maxBound = Off

instance Hashable     Av1AdaptiveQuantization
instance NFData       Av1AdaptiveQuantization
instance ToByteString Av1AdaptiveQuantization
instance ToQuery      Av1AdaptiveQuantization
instance ToHeader     Av1AdaptiveQuantization

instance ToJSON Av1AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Av1AdaptiveQuantization where
    parseJSON = parseJSONText "Av1AdaptiveQuantization"
