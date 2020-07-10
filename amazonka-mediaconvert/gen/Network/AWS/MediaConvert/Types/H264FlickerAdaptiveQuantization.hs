{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization (
  H264FlickerAdaptiveQuantization (
    ..
    , H264FlckrAdptvQntztnDisabled
    , H264FlckrAdptvQntztnEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
data H264FlickerAdaptiveQuantization = H264FlickerAdaptiveQuantization' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern H264FlckrAdptvQntztnDisabled :: H264FlickerAdaptiveQuantization
pattern H264FlckrAdptvQntztnDisabled = H264FlickerAdaptiveQuantization' "DISABLED"

pattern H264FlckrAdptvQntztnEnabled :: H264FlickerAdaptiveQuantization
pattern H264FlckrAdptvQntztnEnabled = H264FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H264FlckrAdptvQntztnDisabled,
  H264FlckrAdptvQntztnEnabled,
  H264FlickerAdaptiveQuantization' #-}

instance FromText H264FlickerAdaptiveQuantization where
    parser = (H264FlickerAdaptiveQuantization' . mk) <$> takeText

instance ToText H264FlickerAdaptiveQuantization where
    toText (H264FlickerAdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H264FlickerAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264FlickerAdaptiveQuantization where
    toEnum i = case i of
        0 -> H264FlckrAdptvQntztnDisabled
        1 -> H264FlckrAdptvQntztnEnabled
        _ -> (error . showText) $ "Unknown index for H264FlickerAdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        H264FlckrAdptvQntztnDisabled -> 0
        H264FlckrAdptvQntztnEnabled -> 1
        H264FlickerAdaptiveQuantization' name -> (error . showText) $ "Unknown H264FlickerAdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H264FlickerAdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264FlickerAdaptiveQuantization where
    minBound = H264FlckrAdptvQntztnDisabled
    maxBound = H264FlckrAdptvQntztnEnabled

instance Hashable     H264FlickerAdaptiveQuantization
instance NFData       H264FlickerAdaptiveQuantization
instance ToByteString H264FlickerAdaptiveQuantization
instance ToQuery      H264FlickerAdaptiveQuantization
instance ToHeader     H264FlickerAdaptiveQuantization

instance ToJSON H264FlickerAdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264FlickerAdaptiveQuantization where
    parseJSON = parseJSONText "H264FlickerAdaptiveQuantization"
