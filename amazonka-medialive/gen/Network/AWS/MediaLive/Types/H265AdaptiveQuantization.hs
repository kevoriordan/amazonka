{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265AdaptiveQuantization (
  H265AdaptiveQuantization (
    ..
    , HAQHigh
    , HAQHigher
    , HAQLow
    , HAQMax
    , HAQMedium
    , HAQOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Adaptive Quantization
data H265AdaptiveQuantization = H265AdaptiveQuantization' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HAQHigh :: H265AdaptiveQuantization
pattern HAQHigh = H265AdaptiveQuantization' "HIGH"

pattern HAQHigher :: H265AdaptiveQuantization
pattern HAQHigher = H265AdaptiveQuantization' "HIGHER"

pattern HAQLow :: H265AdaptiveQuantization
pattern HAQLow = H265AdaptiveQuantization' "LOW"

pattern HAQMax :: H265AdaptiveQuantization
pattern HAQMax = H265AdaptiveQuantization' "MAX"

pattern HAQMedium :: H265AdaptiveQuantization
pattern HAQMedium = H265AdaptiveQuantization' "MEDIUM"

pattern HAQOff :: H265AdaptiveQuantization
pattern HAQOff = H265AdaptiveQuantization' "OFF"

{-# COMPLETE
  HAQHigh,
  HAQHigher,
  HAQLow,
  HAQMax,
  HAQMedium,
  HAQOff,
  H265AdaptiveQuantization' #-}

instance FromText H265AdaptiveQuantization where
    parser = (H265AdaptiveQuantization' . mk) <$> takeText

instance ToText H265AdaptiveQuantization where
    toText (H265AdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H265AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265AdaptiveQuantization where
    toEnum i = case i of
        0 -> HAQHigh
        1 -> HAQHigher
        2 -> HAQLow
        3 -> HAQMax
        4 -> HAQMedium
        5 -> HAQOff
        _ -> (error . showText) $ "Unknown index for H265AdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HAQHigh -> 0
        HAQHigher -> 1
        HAQLow -> 2
        HAQMax -> 3
        HAQMedium -> 4
        HAQOff -> 5
        H265AdaptiveQuantization' name -> (error . showText) $ "Unknown H265AdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H265AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265AdaptiveQuantization where
    minBound = HAQHigh
    maxBound = HAQOff

instance Hashable     H265AdaptiveQuantization
instance NFData       H265AdaptiveQuantization
instance ToByteString H265AdaptiveQuantization
instance ToQuery      H265AdaptiveQuantization
instance ToHeader     H265AdaptiveQuantization

instance ToJSON H265AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H265AdaptiveQuantization where
    parseJSON = parseJSONText "H265AdaptiveQuantization"
